use std::io::{stdout, Stdout};

use anyhow::Result;
use crossterm::{
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
    ExecutableCommand,
};
use ratatui::{backend::CrosstermBackend, widgets::Paragraph, Terminal};
use rune::{Any, ContextError, Module};

use crate::rope::SageRope;

pub fn module() -> Result<Module, ContextError> {
    let mut module = Module::new();
    module.ty::<Tui>()?;
    module.function_meta(new_tui)?;
    module.function_meta(Tui::draw)?;
    Ok(module)
}

#[derive(Debug, Any)]
pub struct Tui {
    terminal: Terminal<CrosstermBackend<Stdout>>,
}

#[rune::function()]
pub fn new_tui() -> Result<Tui> {
    Tui::new()
}

impl Tui {
    pub fn new() -> Result<Tui> {
        stdout().execute(EnterAlternateScreen)?;
        enable_raw_mode()?;
        let mut terminal = Terminal::new(CrosstermBackend::new(stdout()))?;
        terminal.clear()?;
        Ok(Tui { terminal })
    }

    #[rune::function()]
    pub fn draw(&mut self, text: &SageRope) -> Result<()> {
        self.terminal.draw(|frame| {
            let area = frame.size();
            frame.render_widget(Paragraph::new(text.to_string()), area);
        })?;
        Ok(())
    }
}

impl Drop for Tui {
    fn drop(&mut self) {
        self.terminal.clear().ok();
        stdout().execute(LeaveAlternateScreen).ok();
        disable_raw_mode().ok();
    }
}
