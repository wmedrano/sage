use steel::{
    declare_module,
    rvals::Custom,
    steel_vm::ffi::{FFIModule, RegisterFFIFn},
};

declare_module!(create_module);

struct Window {}

impl Custom for Window {}

fn create_module() -> FFIModule {
    let mut module = FFIModule::new("sage");
    module.register_fn("make-window", || -> Window { Window {} });
    module
}
