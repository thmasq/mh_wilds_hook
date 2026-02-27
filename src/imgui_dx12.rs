use imgui::{Context, DrawData};
use std::ffi::c_void;
use std::ptr;
use windows::Win32::Foundation::{HWND, RECT};
use windows::Win32::Graphics::Direct3D::D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST;
use windows::Win32::Graphics::Direct3D::Fxc::D3DCOMPILE_ENABLE_STRICTNESS;
use windows::Win32::Graphics::Direct3D::Fxc::D3DCOMPILE_OPTIMIZATION_LEVEL3;
use windows::Win32::Graphics::Direct3D::Fxc::D3DCompile;
use windows::Win32::Graphics::Direct3D::ID3DBlob;
use windows::Win32::Graphics::Direct3D12::*;
use windows::Win32::Graphics::Dxgi::IDXGISwapChain3;
use windows::Win32::Graphics::Dxgi::{Common::*, DXGI_SWAP_CHAIN_DESC};
use windows::core::{Interface, PCSTR, Result};

#[repr(C)]
struct ImDrawVert {
    pos: [f32; 2],
    uv: [f32; 2],
    col: u32,
}

#[allow(dead_code)]
pub struct ImGuiDx12Backend {
    pub imgui_ctx: Context,
    device: ID3D12Device,
    command_list: ID3D12GraphicsCommandList,
    frames: Vec<FrameResources>,
    rtv_heap: ID3D12DescriptorHeap,
    srv_heap: ID3D12DescriptorHeap,
    root_signature: ID3D12RootSignature,
    pipeline_state: ID3D12PipelineState,

    vertex_buffer: ID3D12Resource,
    index_buffer: ID3D12Resource,
    vb_size: usize,
    ib_size: usize,
}

unsafe impl Send for ImGuiDx12Backend {}
unsafe impl Sync for ImGuiDx12Backend {}

#[allow(dead_code)]
pub struct FrameResources {
    pub command_allocator: ID3D12CommandAllocator,
    pub render_target: ID3D12Resource,
    pub rtv_handle: D3D12_CPU_DESCRIPTOR_HANDLE,
}

impl ImGuiDx12Backend {
    pub unsafe fn new(swap_chain: &IDXGISwapChain3, _hwnd: HWND) -> Result<Self> {
        let device: ID3D12Device = unsafe { swap_chain.GetDevice() }?;
        let desc: DXGI_SWAP_CHAIN_DESC = unsafe { swap_chain.GetDesc() }?;

        let rtv_heap: ID3D12DescriptorHeap = unsafe {
            device.CreateDescriptorHeap(&D3D12_DESCRIPTOR_HEAP_DESC {
                Type: D3D12_DESCRIPTOR_HEAP_TYPE_RTV,
                NumDescriptors: desc.BufferCount,
                Flags: D3D12_DESCRIPTOR_HEAP_FLAG_NONE,
                NodeMask: 0,
            })
        }?;

        let srv_heap: ID3D12DescriptorHeap = unsafe {
            device.CreateDescriptorHeap(&D3D12_DESCRIPTOR_HEAP_DESC {
                Type: D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV,
                NumDescriptors: 1,
                Flags: D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE,
                NodeMask: 0,
            })
        }?;

        let mut frames = Vec::new();
        let rtv_size =
            unsafe { device.GetDescriptorHandleIncrementSize(D3D12_DESCRIPTOR_HEAP_TYPE_RTV) }
                as usize;
        let mut rtv_handle = unsafe { rtv_heap.GetCPUDescriptorHandleForHeapStart() };

        for i in 0..desc.BufferCount {
            let render_target: ID3D12Resource = unsafe { swap_chain.GetBuffer(i) }?;
            unsafe { device.CreateRenderTargetView(&render_target, None, rtv_handle) };
            let command_allocator: ID3D12CommandAllocator =
                unsafe { device.CreateCommandAllocator(D3D12_COMMAND_LIST_TYPE_DIRECT) }?;
            frames.push(FrameResources {
                command_allocator,
                render_target,
                rtv_handle,
            });
            rtv_handle.ptr += rtv_size;
        }

        let command_list: ID3D12GraphicsCommandList = unsafe {
            device.CreateCommandList(
                0,
                D3D12_COMMAND_LIST_TYPE_DIRECT,
                &frames[0].command_allocator,
                None,
            )
        }?;
        (unsafe { command_list.Close() })?;

        let ranges = [D3D12_DESCRIPTOR_RANGE {
            RangeType: D3D12_DESCRIPTOR_RANGE_TYPE_SRV,
            NumDescriptors: 1,
            BaseShaderRegister: 0,
            RegisterSpace: 0,
            OffsetInDescriptorsFromTableStart: 0,
        }];

        let root_parameters = [
            D3D12_ROOT_PARAMETER {
                ParameterType: D3D12_ROOT_PARAMETER_TYPE_32BIT_CONSTANTS,
                Anonymous: D3D12_ROOT_PARAMETER_0 {
                    Constants: D3D12_ROOT_CONSTANTS {
                        ShaderRegister: 0,
                        RegisterSpace: 0,
                        Num32BitValues: 16,
                    },
                },
                ShaderVisibility: D3D12_SHADER_VISIBILITY_VERTEX,
            },
            D3D12_ROOT_PARAMETER {
                ParameterType: D3D12_ROOT_PARAMETER_TYPE_DESCRIPTOR_TABLE,
                Anonymous: D3D12_ROOT_PARAMETER_0 {
                    DescriptorTable: D3D12_ROOT_DESCRIPTOR_TABLE {
                        NumDescriptorRanges: 1,
                        pDescriptorRanges: ranges.as_ptr(),
                    },
                },
                ShaderVisibility: D3D12_SHADER_VISIBILITY_PIXEL,
            },
        ];

        let sampler = D3D12_STATIC_SAMPLER_DESC {
            Filter: D3D12_FILTER_MIN_MAG_MIP_LINEAR,
            AddressU: D3D12_TEXTURE_ADDRESS_MODE_WRAP,
            AddressV: D3D12_TEXTURE_ADDRESS_MODE_WRAP,
            AddressW: D3D12_TEXTURE_ADDRESS_MODE_WRAP,
            MipLODBias: 0.0,
            MaxAnisotropy: 0,
            ComparisonFunc: D3D12_COMPARISON_FUNC_ALWAYS,
            BorderColor: D3D12_STATIC_BORDER_COLOR_TRANSPARENT_BLACK,
            MinLOD: 0.0,
            MaxLOD: 0.0,
            ShaderRegister: 0,
            RegisterSpace: 0,
            ShaderVisibility: D3D12_SHADER_VISIBILITY_PIXEL,
        };

        let root_sig_desc = D3D12_ROOT_SIGNATURE_DESC {
            NumParameters: 2,
            pParameters: root_parameters.as_ptr(),
            NumStaticSamplers: 1,
            pStaticSamplers: &sampler,
            Flags: D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT,
        };

        let mut blob = None;
        (unsafe {
            windows::Win32::Graphics::Direct3D12::D3D12SerializeRootSignature(
                &root_sig_desc,
                D3D_ROOT_SIGNATURE_VERSION_1_0,
                &mut blob,
                None,
            )
        })?;
        let blob = blob.unwrap();
        let root_signature: ID3D12RootSignature = unsafe {
            device.CreateRootSignature(
                0,
                std::slice::from_raw_parts(
                    blob.GetBufferPointer() as *const u8,
                    blob.GetBufferSize(),
                ),
            )
        }?;

        let vs_bytecode = unsafe {
            compile_shader(
                b"cbuffer v{float4x4 p;} struct I{float2 pos:POSITION;float2 uv:TEXCOORD0;float4 col:COLOR0;}; struct O{float4 pos:SV_POSITION;float4 col:COLOR0;float2 uv:TEXCOORD0;}; O main(I i){O o; o.pos=mul(p,float4(i.pos.xy,0,1)); o.col=i.col; o.uv=i.uv; return o;}", 
                b"vs_5_0\0"
            )
        };

        let ps_bytecode = unsafe {
            compile_shader(
                b"SamplerState s:register(s0); Texture2D t:register(t0); struct I{float4 pos:SV_POSITION;float4 col:COLOR0;float2 uv:TEXCOORD0;}; float4 main(I i):SV_TARGET{return i.col*t.Sample(s,i.uv);}", 
                b"ps_5_0\0"
            )
        };

        let input_elements = [
            D3D12_INPUT_ELEMENT_DESC {
                SemanticName: PCSTR(b"POSITION\0".as_ptr()),
                SemanticIndex: 0,
                Format: DXGI_FORMAT_R32G32_FLOAT,
                InputSlot: 0,
                AlignedByteOffset: 0,
                InputSlotClass: D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA,
                InstanceDataStepRate: 0,
            },
            D3D12_INPUT_ELEMENT_DESC {
                SemanticName: PCSTR(b"TEXCOORD\0".as_ptr()),
                SemanticIndex: 0,
                Format: DXGI_FORMAT_R32G32_FLOAT,
                InputSlot: 0,
                AlignedByteOffset: 8,
                InputSlotClass: D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA,
                InstanceDataStepRate: 0,
            },
            D3D12_INPUT_ELEMENT_DESC {
                SemanticName: PCSTR(b"COLOR\0".as_ptr()),
                SemanticIndex: 0,
                Format: DXGI_FORMAT_R8G8B8A8_UNORM,
                InputSlot: 0,
                AlignedByteOffset: 16,
                InputSlotClass: D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA,
                InstanceDataStepRate: 0,
            },
        ];

        let mut pso_desc = D3D12_GRAPHICS_PIPELINE_STATE_DESC::default();
        pso_desc.pRootSignature = std::mem::ManuallyDrop::new(Some(root_signature.clone()));
        pso_desc.VS = D3D12_SHADER_BYTECODE {
            pShaderBytecode: vs_bytecode.as_ptr() as *const c_void,
            BytecodeLength: vs_bytecode.len(),
        };
        pso_desc.PS = D3D12_SHADER_BYTECODE {
            pShaderBytecode: ps_bytecode.as_ptr() as *const c_void,
            BytecodeLength: ps_bytecode.len(),
        };
        pso_desc.BlendState.RenderTarget[0].BlendEnable = true.into();
        pso_desc.BlendState.RenderTarget[0].SrcBlend = D3D12_BLEND_SRC_ALPHA;
        pso_desc.BlendState.RenderTarget[0].DestBlend = D3D12_BLEND_INV_SRC_ALPHA;
        pso_desc.BlendState.RenderTarget[0].BlendOp = D3D12_BLEND_OP_ADD;
        pso_desc.BlendState.RenderTarget[0].RenderTargetWriteMask =
            D3D12_COLOR_WRITE_ENABLE_ALL.0 as u8;
        pso_desc.SampleMask = u32::MAX;
        pso_desc.RasterizerState.FillMode = D3D12_FILL_MODE_SOLID;
        pso_desc.RasterizerState.CullMode = D3D12_CULL_MODE_NONE;
        pso_desc.PrimitiveTopologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE;
        pso_desc.NumRenderTargets = 1;
        pso_desc.RTVFormats[0] = DXGI_FORMAT_R8G8B8A8_UNORM;
        pso_desc.SampleDesc.Count = 1;
        pso_desc.InputLayout = D3D12_INPUT_LAYOUT_DESC {
            pInputElementDescs: input_elements.as_ptr(),
            NumElements: 3,
        };

        let pipeline_state: ID3D12PipelineState =
            unsafe { device.CreateGraphicsPipelineState(&pso_desc) }?;

        let vb_size = 5000 * std::mem::size_of::<ImDrawVert>();
        let ib_size = 10000 * std::mem::size_of::<u16>();
        let vertex_buffer = unsafe { create_upload_buffer(&device, vb_size) }?;
        let index_buffer = unsafe { create_upload_buffer(&device, ib_size) }?;

        Ok(Self {
            imgui_ctx: Context::create(),
            device,
            command_list,
            frames,
            rtv_heap,
            srv_heap,
            root_signature,
            pipeline_state,
            vertex_buffer,
            index_buffer,
            vb_size,
            ib_size,
        })
    }

    pub unsafe fn render(
        &mut self,
        swap_chain: &IDXGISwapChain3,
        command_queue: &ID3D12CommandQueue,
    ) -> Result<()> {
        let frame_index = unsafe { swap_chain.GetCurrentBackBufferIndex() } as usize;
        let frame = &self.frames[frame_index];

        unsafe {
            frame.command_allocator.Reset()?;
            self.command_list
                .Reset(&frame.command_allocator, &self.pipeline_state)?;

            self.command_list
                .OMSetRenderTargets(1, Some(&frame.rtv_handle), false, None);
            self.command_list
                .SetDescriptorHeaps(&[Some(self.srv_heap.clone())]);
            self.command_list
                .SetGraphicsRootSignature(&self.root_signature);
        }

        let ui = self.imgui_ctx.frame();
        ui.show_demo_window(&mut true);

        let draw_data = self.imgui_ctx.render();
        let draw_data_ptr = draw_data as *const DrawData;

        unsafe {
            self.render_draw_data(&*draw_data_ptr)?;
            self.command_list.Close()?;
            command_queue.ExecuteCommandLists(&[Some(self.command_list.cast()?)]);
        }
        Ok(())
    }

    unsafe fn render_draw_data(&mut self, draw_data: &DrawData) -> Result<()> {
        let required_vb_size =
            draw_data.total_vtx_count as usize * std::mem::size_of::<ImDrawVert>();
        let required_ib_size = draw_data.total_idx_count as usize * std::mem::size_of::<u16>();

        if required_vb_size > self.vb_size {
            self.vertex_buffer =
                unsafe { create_upload_buffer(&self.device, required_vb_size + 5000) }?;
            self.vb_size = required_vb_size + 5000;
        }
        if required_ib_size > self.ib_size {
            self.index_buffer =
                unsafe { create_upload_buffer(&self.device, required_ib_size + 10000) }?;
            self.ib_size = required_ib_size + 10000;
        }

        let mut vtx_dst: *mut c_void = ptr::null_mut();
        let mut idx_dst: *mut c_void = ptr::null_mut();

        unsafe {
            self.vertex_buffer.Map(0, None, Some(&mut vtx_dst))?;
            self.index_buffer.Map(0, None, Some(&mut idx_dst))?;
        }

        let mut vtx_offset = 0;
        let mut idx_offset = 0;

        for draw_list in draw_data.draw_lists() {
            let vtx_slice = draw_list.vtx_buffer();
            let idx_slice = draw_list.idx_buffer();

            unsafe {
                ptr::copy_nonoverlapping(
                    vtx_slice.as_ptr() as *const u8,
                    (vtx_dst as *mut u8).add(vtx_offset),
                    vtx_slice.len() * std::mem::size_of::<ImDrawVert>(),
                )
            };
            unsafe {
                ptr::copy_nonoverlapping(
                    idx_slice.as_ptr() as *const u8,
                    (idx_dst as *mut u8).add(idx_offset),
                    idx_slice.len() * std::mem::size_of::<u16>(),
                )
            };

            vtx_offset += vtx_slice.len() * std::mem::size_of::<ImDrawVert>();
            idx_offset += idx_slice.len() * std::mem::size_of::<u16>();
        }

        unsafe {
            self.vertex_buffer.Unmap(0, None);
            self.index_buffer.Unmap(0, None);
        }

        let l = draw_data.display_pos[0];
        let r = draw_data.display_pos[0] + draw_data.display_size[0];
        let t = draw_data.display_pos[1];
        let b = draw_data.display_pos[1] + draw_data.display_size[1];
        let mvp = [
            [2.0 / (r - l), 0.0, 0.0, 0.0],
            [0.0, 2.0 / (t - b), 0.0, 0.0],
            [0.0, 0.0, 0.5, 0.0],
            [(r + l) / (l - r), (t + b) / (b - t), 0.5, 1.0],
        ];

        unsafe {
            self.command_list.SetGraphicsRoot32BitConstants(
                0,
                16,
                &mvp as *const _ as *const c_void,
                0,
            )
        };

        let vb_view = D3D12_VERTEX_BUFFER_VIEW {
            BufferLocation: unsafe { self.vertex_buffer.GetGPUVirtualAddress() },
            SizeInBytes: self.vb_size as u32,
            StrideInBytes: std::mem::size_of::<ImDrawVert>() as u32,
        };
        let ib_view = D3D12_INDEX_BUFFER_VIEW {
            BufferLocation: unsafe { self.index_buffer.GetGPUVirtualAddress() },
            SizeInBytes: self.ib_size as u32,
            Format: DXGI_FORMAT_R16_UINT,
        };

        unsafe {
            self.command_list
                .IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
            self.command_list.IASetVertexBuffers(0, Some(&[vb_view]));
            self.command_list.IASetIndexBuffer(Some(&ib_view));
        }

        let mut global_vtx_offset = 0;
        let mut global_idx_offset = 0;
        let clip_off = draw_data.display_pos;

        for draw_list in draw_data.draw_lists() {
            for cmd in draw_list.commands() {
                match cmd {
                    imgui::DrawCmd::Elements { count, cmd_params } => {
                        let clip_rect = RECT {
                            left: (cmd_params.clip_rect[0] - clip_off[0]) as i32,
                            top: (cmd_params.clip_rect[1] - clip_off[1]) as i32,
                            right: (cmd_params.clip_rect[2] - clip_off[0]) as i32,
                            bottom: (cmd_params.clip_rect[3] - clip_off[1]) as i32,
                        };

                        unsafe {
                            self.command_list.RSSetScissorRects(&[clip_rect]);

                            self.command_list.SetGraphicsRootDescriptorTable(
                                1,
                                self.srv_heap.GetGPUDescriptorHandleForHeapStart(),
                            );

                            self.command_list.DrawIndexedInstanced(
                                count as u32,
                                1,
                                (cmd_params.idx_offset + global_idx_offset) as u32,
                                (cmd_params.vtx_offset + global_vtx_offset) as i32,
                                0,
                            );
                        }
                    }
                    _ => {}
                }
            }
            global_idx_offset += draw_list.idx_buffer().len();
            global_vtx_offset += draw_list.vtx_buffer().len();
        }
        Ok(())
    }
}

unsafe fn create_upload_buffer(device: &ID3D12Device, size: usize) -> Result<ID3D12Resource> {
    let heap_props = D3D12_HEAP_PROPERTIES {
        Type: D3D12_HEAP_TYPE_UPLOAD,
        ..Default::default()
    };
    let desc = D3D12_RESOURCE_DESC {
        Dimension: D3D12_RESOURCE_DIMENSION_BUFFER,
        Width: size as u64,
        Height: 1,
        DepthOrArraySize: 1,
        MipLevels: 1,
        Format: DXGI_FORMAT_UNKNOWN,
        SampleDesc: DXGI_SAMPLE_DESC {
            Count: 1,
            Quality: 0,
        },
        Layout: D3D12_TEXTURE_LAYOUT_ROW_MAJOR,
        Flags: D3D12_RESOURCE_FLAG_NONE,
        Alignment: 0,
    };
    let mut resource = None;
    unsafe {
        device.CreateCommittedResource(
            &heap_props,
            D3D12_HEAP_FLAG_NONE,
            &desc,
            D3D12_RESOURCE_STATE_GENERIC_READ,
            None,
            &mut resource,
        )?;
    }
    Ok(resource.unwrap())
}

unsafe fn compile_shader(source: &[u8], target: &[u8]) -> Vec<u8> {
    let mut bytecode: Option<ID3DBlob> = None;
    let mut error_msg: Option<ID3DBlob> = None;

    let hr = unsafe {
        D3DCompile(
            source.as_ptr() as *const c_void,
            source.len(),
            PCSTR::null(),             // Optional: Source name for debugging
            None,                      // Optional: Defines
            None,                      // Optional: Includes
            PCSTR(b"main\0".as_ptr()), // Entrypoint function name
            PCSTR(target.as_ptr()),    // Target profile (vs_5_0 / ps_5_0)
            D3DCOMPILE_ENABLE_STRICTNESS | D3DCOMPILE_OPTIMIZATION_LEVEL3, // Compile flags
            0,                         // Effect flags
            &mut bytecode,
            Some(&mut error_msg),
        )
    };

    if hr.is_err() {
        if let Some(errors) = error_msg {
            let err_ptr = unsafe { errors.GetBufferPointer() } as *const u8;
            let err_size = unsafe { errors.GetBufferSize() };

            let msg =
                String::from_utf8_lossy(unsafe { std::slice::from_raw_parts(err_ptr, err_size) });
            panic!("[DX12] HLSL Shader compilation failed:\n{}", msg);
        } else {
            panic!(
                "[DX12] Shader compilation failed with no error string. {:?}",
                hr
            );
        }
    }

    let blob = bytecode.expect("Shader compilation succeeded but returned no blob.");
    let ptr = unsafe { blob.GetBufferPointer() } as *const u8;
    let size = unsafe { blob.GetBufferSize() };

    unsafe { std::slice::from_raw_parts(ptr, size).to_vec() }
}
