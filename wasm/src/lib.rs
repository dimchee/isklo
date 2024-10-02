use egg::*;
use wasm_bindgen::prelude::*;

define_language! {
    enum SimpleLanguage {
        Num(i32),
        "+" = Add([Id; 2]),
        "*" = Mul([Id; 2]),
        Symbol(Symbol),
    }
}

fn make_rules() -> Vec<Rewrite<SimpleLanguage, ()>> {
    vec![
        rewrite!("commute-add"; "(+ ?a ?b)" => "(+ ?b ?a)"),
        rewrite!("commute-mul"; "(* ?a ?b)" => "(* ?b ?a)"),
        rewrite!("add-0"; "(+ ?a 0)" => "?a"),
        rewrite!("mul-0"; "(* ?a 0)" => "0"),
        rewrite!("mul-1"; "(* ?a 1)" => "?a"),
    ]
}

#[wasm_bindgen]
pub fn simplify(s: &str) -> String {
    let expr = s.parse::<RecExpr<SimpleLanguage>>();
    match expr {
        Ok(expr) => {
        let runner = Runner::default().with_expr(&expr).run(&make_rules());
        let root = runner.roots[0];

        let extractor = Extractor::new(&runner.egraph, AstSize);
        let (best_cost, best) = extractor.find_best(root);
        println!("Simplified {} to {} with cost {}", expr, best, best_cost);
        best.to_string()
        },
        _ => "Parsing error".to_owned()
    }
}
#[test]
fn simple_tests() {
    assert_eq!(simplify("(* 0 42)"), "0");
    assert_eq!(simplify("(+ 0 (* 1 foo))"), "foo");
}
