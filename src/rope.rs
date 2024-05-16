use crop::Rope;
use rune::{Any, ContextError, Module};

pub fn module() -> Result<Module, ContextError> {
    let mut module = Module::new();
    module.ty::<SageRope>()?;
    module.function_meta(new_rope)?;
    module.function_meta(SageRope::len)?;
    module.function_meta(SageRope::replace)?;
    module.function_meta(SageRope::to_string)?;
    Ok(module)
}

#[derive(Debug, Any)]
pub struct SageRope {
    rope: Rope,
}

#[rune::function()]
fn new_rope() -> SageRope {
    SageRope { rope: Rope::new() }
}

impl SageRope {
    #[rune::function()]
    fn len(&self) -> i64 {
        self.rope.byte_len() as i64
    }

    #[rune::function()]
    fn replace(&mut self, start: i64, end: i64, text: &str) {
        self.rope
            .replace(start.max(0) as usize..end.max(0) as usize, text);
    }

    #[rune::function()]
    fn to_string(&self) -> String {
        self.to_string()
    }
}

impl std::fmt::Display for SageRope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.rope.fmt(f)
    }
}
