use anyhow::Result;
use rune::termcolor::{ColorChoice, StandardStream};
use rune::{Context, Diagnostics, Source, Sources, Vm};
use std::sync::Arc;

fn main() -> Result<()> {
    let context = Context::with_default_modules()?;
    let runtime = Arc::new(context.runtime()?);

    let mut sources = Sources::new();
    sources.insert(Source::memory("pub fn add(a, b) { a + b }")?)?;

    let mut diagnostics = Diagnostics::new();

    let result = rune::prepare(&mut sources)
        .with_context(&context)
        .with_diagnostics(&mut diagnostics)
        .build();

    if !diagnostics.is_empty() {
        let mut writer = StandardStream::stderr(ColorChoice::Always);
        diagnostics.emit(&mut writer, &sources)?;
    }

    let unit = result?;
    let mut vm = Vm::new(runtime, Arc::new(unit));

    let output = vm.call(["add"], (10i64, 20i64))?;
    let output: i64 = rune::from_value(output)?;

    println!("Test output is {}", output);
    rune::cli::Entry::new()
        .about(format_args!("Sage on Rune"))
        .context(&mut |_opts| {
            let c = rune_modules::default_context()?;
            Ok(c)
        })
        .run();
}
