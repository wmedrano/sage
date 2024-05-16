use std::time::Duration;

use crossterm::event::{self, KeyEventKind};
use rune::{
    runtime::{Object, Shared},
    ContextError, Module, Value,
};

pub struct Event {
    code: String,
    ctrl: bool,
    alt: bool,
    shift: bool,
}

pub fn module() -> Result<Module, ContextError> {
    let mut module = Module::new();
    module.function_meta(poll_event)?;
    Ok(module)
}

#[rune::function()]
fn poll_event(timeout_ms: i64) -> Option<Value> {
    let timeout = Duration::from_millis(timeout_ms.clamp(0, i64::MAX) as u64);
    if !event::poll(timeout).unwrap_or(false) {
        return None;
    }
    let ev = event::read().ok()?;
    match ev {
        event::Event::Key(event::KeyEvent {
            kind: KeyEventKind::Press,
            code,
            modifiers,
            ..
        }) => Some(Event::new(code, modifiers).to_value()),
        _ => None,
    }
}

impl Event {
    fn to_value(self) -> Value {
        let mut obj = Object::new();
        obj.insert_value(rune::alloc::String::try_from("code").unwrap(), self.code)
            .unwrap();
        obj.insert_value(rune::alloc::String::try_from("ctrl").unwrap(), self.ctrl)
            .unwrap();
        obj.insert_value(rune::alloc::String::try_from("alt").unwrap(), self.alt)
            .unwrap();
        obj.insert_value(rune::alloc::String::try_from("shift").unwrap(), self.shift)
            .unwrap();
        Value::Object(Shared::new(obj).unwrap())
    }

    fn new(code: event::KeyCode, modifiers: event::KeyModifiers) -> Event {
        let code = match code {
            event::KeyCode::Backspace => "<backspace>".to_string(),
            event::KeyCode::Enter => "<enter>".to_string(),
            event::KeyCode::Left => "<left>".to_string(),
            event::KeyCode::Right => "<right>".to_string(),
            event::KeyCode::Up => "<up>".to_string(),
            event::KeyCode::Down => "<down>".to_string(),
            event::KeyCode::Home => "<home>".to_string(),
            event::KeyCode::End => "<end>".to_string(),
            event::KeyCode::PageUp => "<page-up>".to_string(),
            event::KeyCode::PageDown => "<page-down>".to_string(),
            event::KeyCode::Tab => "<tab>".to_string(),
            event::KeyCode::BackTab => "<back-tab>".to_string(),
            event::KeyCode::Delete => "<delete>".to_string(),
            event::KeyCode::Insert => "<insert>".to_string(),
            event::KeyCode::F(n) => format!("<f{n}>"),
            event::KeyCode::Char(ch) => ch.to_string(),
            event::KeyCode::Null => "<null>".to_string(),
            event::KeyCode::Esc => "<esc>".to_string(),
            event::KeyCode::CapsLock => "<caps-lock>".to_string(),
            event::KeyCode::ScrollLock => "<scroll-lock>".to_string(),
            event::KeyCode::NumLock => "<num-lock>".to_string(),
            event::KeyCode::PrintScreen => "<print-screen>".to_string(),
            event::KeyCode::Pause => "<pause>".to_string(),
            event::KeyCode::Menu => "<menu>".to_string(),
            event::KeyCode::KeypadBegin => "<keypad-begin>".to_string(),
            event::KeyCode::Media(_) => "<media-???>".to_string(),
            event::KeyCode::Modifier(m) => match m {
                event::ModifierKeyCode::LeftShift | event::ModifierKeyCode::RightShift => {
                    "<shift>".to_string()
                }
                event::ModifierKeyCode::LeftControl | event::ModifierKeyCode::RightControl => {
                    "<ctrl>".to_string()
                }
                event::ModifierKeyCode::LeftAlt | event::ModifierKeyCode::RightAlt => {
                    "<alt>".to_string()
                }
                _ => "<???>".to_string(),
            },
        };
        Event {
            code,
            ctrl: modifiers.contains(event::KeyModifiers::CONTROL),
            alt: modifiers.contains(event::KeyModifiers::ALT),
            shift: modifiers.contains(event::KeyModifiers::SHIFT),
        }
    }
}
