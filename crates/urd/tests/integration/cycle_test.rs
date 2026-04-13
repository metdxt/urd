use urd::prelude::*;

#[test]
fn test_list_cycle_format() {
    let src = r#"
        @entry
        label main {
            let l = []
            l.push(l)
            let s = "{l}"
            end!()
        }
    "#;
    let ast = urd::compiler::loader::parse_source(src).unwrap();
    let graph = urd::compiler::Compiler::compile(&ast).unwrap();
    let registry = DecoratorRegistry::new();
    let mut vm = Vm::new(graph, registry).unwrap();
    loop {
        match vm.next(None) {
            VmStep::Ended => break,
            VmStep::Error(e) => panic!("error: {e}"),
            _ => {}
        }
    }
}

#[test]
fn test_list_cycle_eq() {
    let src = r#"
        @entry
        label main {
            let l = []
            l.push(l)
            let b = l == l
            end!()
        }
    "#;
    let ast = urd::compiler::loader::parse_source(src).unwrap();
    let graph = urd::compiler::Compiler::compile(&ast).unwrap();
    let registry = DecoratorRegistry::new();
    let mut vm = Vm::new(graph, registry).unwrap();
    loop {
        match vm.next(None) {
            VmStep::Ended => break,
            VmStep::Error(e) => panic!("error: {e}"),
            _ => {}
        }
    }
}
