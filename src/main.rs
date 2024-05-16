use rune::termcolor::{ColorChoice, StandardStream};
use rune::{Diagnostics, Source, Sources, Vm};

use std::sync::Arc;

mod tui;

fn main() -> rune::support::Result<()> {
    let mut context = rune_modules::default_context()?;
    context.install(tui::module()?)?;

    let mut sources: Sources = Sources::new();
    sources.insert(Source::from_path("./main.rn")?)?;

    let mut writer = StandardStream::stderr(ColorChoice::Always);
    let mut diagnostics = Diagnostics::new();

    let result = rune::prepare(&mut sources)
        .with_context(&context)
        .with_diagnostics(&mut diagnostics)
        .build();
    if !diagnostics.is_empty() {
        diagnostics.emit(&mut writer, &sources)?;
    }

    let mut vm = Vm::new(Arc::new(context.runtime()?), Arc::new(result?));
    if let Err(err) = vm.execute(["main"], ())?.complete().into_result() {
        err.emit(&mut writer, &sources)?;
    }
    Ok(())
}
