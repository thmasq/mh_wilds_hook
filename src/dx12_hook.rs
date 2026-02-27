use lazy_static::lazy_static;
use retour::GenericDetour;
use std::ffi::c_void;
use std::sync::Mutex;
use std::sync::atomic::{AtomicBool, Ordering};

use windows::Win32::Foundation::{HINSTANCE, HWND};
use windows::Win32::Graphics::Direct3D::D3D_FEATURE_LEVEL_11_0;
use windows::Win32::Graphics::Direct3D12::*;
use windows::Win32::Graphics::Dxgi::Common::*;
use windows::Win32::Graphics::Dxgi::*;
use windows::Win32::System::LibraryLoader::GetModuleHandleA;
use windows::Win32::UI::WindowsAndMessaging::*;
use windows::core::{ComInterface, HRESULT, PCSTR};

use crate::imgui_dx12::ImGuiDx12Backend;

type PresentFn = extern "system" fn(*mut c_void, u32, u32) -> HRESULT;
type ExecuteCommandListsFn = extern "system" fn(*mut c_void, u32, *const *mut c_void);

lazy_static! {
    static ref DX12_POINTERS: (usize, usize) =
        get_dx12_pointers().expect("Failed to initialize dummy DX12 pointers");
    static ref PRESENT_HOOK: GenericDetour<PresentFn> = {
        let present_addr = DX12_POINTERS.0;
        println!("[DX12] Found Present at address: {:#X}", present_addr);
        unsafe {
            let target: PresentFn = std::mem::transmute(present_addr);
            GenericDetour::new(target, hooked_present).expect("Failed to create Present detour")
        }
    };
    static ref EXECUTE_COMMAND_LISTS_HOOK: GenericDetour<ExecuteCommandListsFn> = {
        let exec_addr = DX12_POINTERS.1;
        println!(
            "[DX12] Found ExecuteCommandLists at address: {:#X}",
            exec_addr
        );
        unsafe {
            let target: ExecuteCommandListsFn = std::mem::transmute(exec_addr);
            GenericDetour::new(target, hooked_execute_command_lists)
                .expect("Failed to create ExecuteCommandLists detour")
        }
    };
    static ref COMMAND_QUEUE: Mutex<Option<ID3D12CommandQueue>> = Mutex::new(None);
    pub static ref RENDER_BACKEND: Mutex<Option<ImGuiDx12Backend>> = Mutex::new(None);
}

static IMGUI_INITIALIZED: AtomicBool = AtomicBool::new(false);

pub fn init() -> Result<(), Box<dyn std::error::Error>> {
    unsafe {
        PRESENT_HOOK.enable()?;
        EXECUTE_COMMAND_LISTS_HOOK.enable()?;
    }
    println!("[DX12] Hooks successfully applied!");
    Ok(())
}

/// Creates a dummy window and DX12 device to extract the VTable pointers for
/// `IDXGISwapChain::Present` and `ID3D12CommandQueue::ExecuteCommandLists`
fn get_dx12_pointers() -> Option<(usize, usize)> {
    unsafe {
        let h_inst = GetModuleHandleA(None).unwrap_or(HINSTANCE(0));
        let window_class_name = PCSTR(b"MHWDummyWindow\0".as_ptr());

        let wc = WNDCLASSA {
            style: CS_HREDRAW | CS_VREDRAW,
            lpfnWndProc: Some(DefWindowProcA),
            hInstance: h_inst.into(),
            lpszClassName: window_class_name,
            ..Default::default()
        };

        if RegisterClassA(&wc) == 0 {
            return None;
        }

        let hwnd = CreateWindowExA(
            Default::default(),
            window_class_name,
            PCSTR(b"Dummy\0".as_ptr()),
            WS_OVERLAPPEDWINDOW,
            0,
            0,
            100,
            100,
            HWND(0),
            Default::default(),
            h_inst,
            None,
        );

        if hwnd.0 == 0 {
            UnregisterClassA(window_class_name, h_inst);
            return None;
        }

        let factory: IDXGIFactory = CreateDXGIFactory().ok()?;

        let mut device: Option<ID3D12Device> = None;
        if D3D12CreateDevice(None, D3D_FEATURE_LEVEL_11_0, &mut device).is_err() {
            DestroyWindow(hwnd);
            UnregisterClassA(window_class_name, h_inst);
            return None;
        }
        let device = device?;

        let queue_desc = D3D12_COMMAND_QUEUE_DESC {
            Type: D3D12_COMMAND_LIST_TYPE_DIRECT,
            Priority: 0,
            Flags: D3D12_COMMAND_QUEUE_FLAG_NONE,
            NodeMask: 0,
        };
        let command_queue: ID3D12CommandQueue = device.CreateCommandQueue(&queue_desc).ok()?;

        let swap_desc = DXGI_SWAP_CHAIN_DESC {
            BufferDesc: DXGI_MODE_DESC {
                Width: 100,
                Height: 100,
                RefreshRate: Default::default(),
                Format: DXGI_FORMAT_R8G8B8A8_UNORM,
                ScanlineOrdering: Default::default(),
                Scaling: Default::default(),
            },
            SampleDesc: DXGI_SAMPLE_DESC {
                Count: 1,
                Quality: 0,
            },
            BufferUsage: DXGI_USAGE_RENDER_TARGET_OUTPUT,
            BufferCount: 2,
            OutputWindow: hwnd,
            Windowed: true.into(),
            SwapEffect: DXGI_SWAP_EFFECT_FLIP_DISCARD,
            Flags: 0,
        };

        let swap_chain: IDXGISwapChain =
            factory.CreateSwapChain(&command_queue, &swap_desc).ok()?;

        let swap_chain_ptr: *mut c_void = std::mem::transmute_copy(&swap_chain);
        let swap_chain_vtable = *(swap_chain_ptr as *const *const usize);
        let present_ptr = *swap_chain_vtable.add(8);

        let cmd_queue_ptr: *mut c_void = std::mem::transmute_copy(&command_queue);
        let cmd_queue_vtable = *(cmd_queue_ptr as *const *const usize);
        let exec_cmd_lists_ptr = *cmd_queue_vtable.add(10);

        DestroyWindow(hwnd.expect("Invalid window handle"));
        UnregisterClassA(window_class_name, h_inst);

        Some((present_ptr, exec_cmd_lists_ptr))
    }
}

/// Hook 2: Capture the Command Queue.
/// RE Engine calls this to submit work to the GPU. We steal the pointer here.
extern "system" fn hooked_execute_command_lists(
    command_queue_ptr: *mut c_void,
    num_command_lists: u32,
    pp_command_lists: *const *mut c_void,
) {
    unsafe {
        let mut queue_lock = COMMAND_QUEUE.lock().unwrap();
        if queue_lock.is_none() {
            let queue: ID3D12CommandQueue = std::mem::transmute_copy(&command_queue_ptr);
            *queue_lock = Some(queue.clone());
            println!("[DX12] Successfully captured ID3D12CommandQueue!");
        }
    }

    EXECUTE_COMMAND_LISTS_HOOK.call(command_queue_ptr, num_command_lists, pp_command_lists)
}

/// Hook 1: The Render Loop.
extern "system" fn hooked_present(
    swap_chain_ptr: *mut c_void,
    sync_interval: u32,
    flags: u32,
) -> HRESULT {
    unsafe {
        let swap_chain: IDXGISwapChain3 = std::mem::transmute_copy(&swap_chain_ptr);

        let queue_opt = COMMAND_QUEUE.lock().unwrap().clone();

        if let Some(command_queue) = queue_opt {
            let mut backend_lock = RENDER_BACKEND.lock().unwrap();

            if !IMGUI_INITIALIZED.load(Ordering::SeqCst) {
                let mut desc = Default::default();
                if swap_chain.GetDesc().is_ok() {
                    let hwnd = desc.OutputWindow;

                    println!("[DX12] Initializing ImGui Backend...");
                    match ImGuiDx12Backend::new(&swap_chain, hwnd) {
                        Ok(backend) => {
                            *backend_lock = Some(backend);
                            IMGUI_INITIALIZED.store(true, Ordering::SeqCst);
                            println!("[DX12] ImGui Backend Initialized Successfully!");
                        }
                        Err(e) => println!("[DX12] Failed to init ImGui: {:?}", e),
                    }
                }
            }

            if IMGUI_INITIALIZED.load(Ordering::SeqCst) {
                if let Some(backend) = backend_lock.as_mut() {
                    if let Err(e) = backend.render(&swap_chain, &command_queue) {
                        println!("[DX12] ImGui render error: {:?}", e);
                    }
                }
            }
        }
    }

    unsafe { PRESENT_HOOK.call(swap_chain_ptr, sync_interval, flags) }
}
