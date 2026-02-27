use std::ffi::c_void;
use std::thread;
use std::time::Duration;
use windows::Win32::Foundation::{BOOL, FARPROC, HINSTANCE, HWND, TRUE};
use windows::Win32::System::LibraryLoader::{GetProcAddress, LoadLibraryA};
use windows::Win32::System::SystemServices::{DLL_PROCESS_ATTACH, DLL_PROCESS_DETACH};
use windows::Win32::UI::WindowsAndMessaging::FindWindowA;
use windows::core::{HRESULT, PCSTR};

mod dx12_hook;
mod imgui_dx12;
mod input_hook;

type DirectInput8CreateFn = extern "system" fn(
    hinst: HINSTANCE,
    dwversion: u32,
    riidltf: *const c_void,
    ppvout: *mut *mut c_void,
    punkouter: *mut c_void,
) -> HRESULT;

static mut REAL_DINPUT8_CREATE: Option<DirectInput8CreateFn> = None;

#[unsafe(no_mangle)]
pub extern "system" fn DirectInput8Create(
    hinst: HINSTANCE,
    dwversion: u32,
    riidltf: *const c_void,
    ppvout: *mut *mut c_void,
    punkouter: *mut c_void,
) -> HRESULT {
    unsafe {
        if let Some(real_func) = REAL_DINPUT8_CREATE {
            return real_func(hinst, dwversion, riidltf, ppvout, punkouter);
        }
    }
    HRESULT(0x80004001)
}

#[unsafe(no_mangle)]
#[allow(non_snake_case)]
pub extern "system" fn DllMain(
    _hinst_dll: HINSTANCE,
    fdw_reason: u32,
    _lp_reserved: *mut c_void,
) -> BOOL {
    match fdw_reason {
        DLL_PROCESS_ATTACH => {
            unsafe {
                let system_dir = "C:\\Windows\\System32\\dinput8.dll\0";
                let real_dll = LoadLibraryA(PCSTR(system_dir.as_ptr())).unwrap();
                let real_func_ptr: FARPROC =
                    GetProcAddress(real_dll, PCSTR("DirectInput8Create\0".as_ptr()));

                REAL_DINPUT8_CREATE = Some(std::mem::transmute(real_func_ptr));
            }

            thread::spawn(|| {
                main_thread();
            });
        }
        DLL_PROCESS_DETACH => {
            // Cleanup hooks in case I decide unloading the DLL dynamically might be necessary.
        }
        _ => {}
    }
    TRUE
}

fn main_thread() {
    let _ = unsafe { windows::Win32::System::Console::AllocConsole() };
    println!("[Main] Monster Hunter Wilds Rust Hook Initializing...");
    println!("[Main] Waiting for the REngine window to be created...");

    let window_class = PCSTR(b"REngine\0".as_ptr());
    let mut hwnd = HWND(std::ptr::null_mut());

    while hwnd.0 == std::ptr::null_mut() {
        unsafe {
            hwnd = FindWindowA(window_class, PCSTR::null()).expect("Could not find Window");
        }
        if hwnd.0 == std::ptr::null_mut() {
            thread::sleep(Duration::from_millis(500));
        }
    }

    println!("[Main] Found REngine Window! Applying Hooks...");

    if let Err(e) = input_hook::init() {
        println!("[Main] Failed to hook input: {:?}", e);
    } else {
        println!("[Main] Input hook successful.");
    }

    if let Err(e) = dx12_hook::init() {
        println!("[Main] Failed to hook DX12: {:?}", e);
    } else {
        println!("[Main] DX12 hook successful.");
    }

    println!("[Main] Initialization Complete! Press INSERT to toggle the menu in-game.");
}
