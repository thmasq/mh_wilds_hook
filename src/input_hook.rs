use std::sync::atomic::{AtomicBool, AtomicIsize, Ordering};
use windows::Win32::Foundation::{HWND, LPARAM, LRESULT, WPARAM};
use windows::Win32::UI::Input::KeyboardAndMouse::*;
use windows::Win32::UI::WindowsAndMessaging::*;

static ORIGINAL_WNDPROC: AtomicIsize = AtomicIsize::new(0);

pub static IS_MENU_OPEN: AtomicBool = AtomicBool::new(false);

pub fn init() -> Result<(), Box<dyn std::error::Error>> {
    unsafe {
        let hwnd = FindWindowA(
            windows::core::PCSTR(b"REngine\0".as_ptr()),
            windows::core::PCSTR::null(),
        );

        if hwnd.unwrap().0 == std::ptr::null_mut() {
            return Err("Could not find REngine window. Is the game running?".into());
        }

        let orig = SetWindowLongPtrA(hwnd?, GWLP_WNDPROC, hooked_wndproc as isize);
        if orig == 0 {
            return Err("Failed to set window long ptr".into());
        }

        ORIGINAL_WNDPROC.store(orig, Ordering::SeqCst);
        println!("[Input] WndProc successfully hooked!");
    }
    Ok(())
}

extern "system" fn hooked_wndproc(hwnd: HWND, msg: u32, wparam: WPARAM, lparam: LPARAM) -> LRESULT {
    if msg == WM_KEYUP && wparam.0 == VK_INSERT.0 as usize {
        let current_state = IS_MENU_OPEN.load(Ordering::SeqCst);
        IS_MENU_OPEN.store(!current_state, Ordering::SeqCst);
    }

    let is_menu_open = IS_MENU_OPEN.load(Ordering::SeqCst);
    let mut imgui_wants_capture = false;

    if let Ok(mut backend_lock) = crate::dx12_hook::RENDER_BACKEND.try_lock() {
        if let Some(backend) = backend_lock.as_mut() {
            let io = backend.imgui_ctx.io_mut();

            let mouse_x = (lparam.0 & 0xFFFF) as f32;
            let mouse_y = ((lparam.0 >> 16) & 0xFFFF) as f32;

            match msg {
                WM_MOUSEMOVE => {
                    io.mouse_pos = [mouse_x, mouse_y];
                }
                WM_LBUTTONDOWN => io.mouse_down[0] = true,
                WM_LBUTTONUP => io.mouse_down[0] = false,
                WM_RBUTTONDOWN => io.mouse_down[1] = true,
                WM_RBUTTONUP => io.mouse_down[1] = false,
                WM_MBUTTONDOWN => io.mouse_down[2] = true,
                WM_MBUTTONUP => io.mouse_down[2] = false,
                WM_MOUSEWHEEL => {
                    let wheel_delta = ((wparam.0 >> 16) & 0xFFFF) as i16 as f32;
                    io.mouse_wheel += wheel_delta / WHEEL_DELTA as f32;
                }
                WM_CHAR => {
                    if let Some(c) = std::char::from_u32(wparam.0 as u32) {
                        io.add_input_character(c);
                    }
                }
                _ => {}
            }

            imgui_wants_capture = io.want_capture_mouse || io.want_capture_keyboard;
        }
    }

    if is_menu_open {
        if msg == WM_MOUSEMOVE
            || msg == WM_LBUTTONDOWN
            || msg == WM_LBUTTONUP
            || msg == WM_RBUTTONDOWN
            || msg == WM_RBUTTONUP
            || msg == WM_MOUSEWHEEL
        {
            return LRESULT(1);
        }

        if imgui_wants_capture && (msg == WM_KEYDOWN || msg == WM_KEYUP || msg == WM_CHAR) {
            return LRESULT(1);
        }
    }

    let orig = ORIGINAL_WNDPROC.load(Ordering::SeqCst);
    unsafe {
        let orig_fn: WNDPROC = std::mem::transmute(orig);
        CallWindowProcA(orig_fn, hwnd, msg, wparam, lparam)
    }
}
