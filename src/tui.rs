use std::io::{stdout, Stdout};

use anyhow::Result;
use crossterm::{
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
    ExecutableCommand,
};
use ratatui::{backend::CrosstermBackend, Terminal};
use rune::{Any, ContextError, Module};

pub fn module() -> Result<Module, ContextError> {
    let mut module = Module::new();
    module.ty::<Tui>()?;
    module.function_meta(new_tui)?;
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
}

impl Drop for Tui {
    fn drop(&mut self) {
        self.terminal.clear().ok();
        stdout().execute(LeaveAlternateScreen).ok();
        disable_raw_mode().ok();
    }
}
