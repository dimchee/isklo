use egg::*;
use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;

#[derive(Serialize, Deserialize)]
struct Rule {
    name: String,
    searcher: String,
    applier: String,
}

#[wasm_bindgen]
pub fn simplify(rules: Vec<JsValue>, s: &str) -> String {
    let rules = rules
        .iter()
        .flat_map(|x| serde_wasm_bindgen::from_value::<Rule>(x.clone()).ok())
        .collect::<Vec<_>>();
    simplify_core(&rules, s)
}

#[wasm_bindgen]
pub fn explain(rules: Vec<JsValue>, s: &str) -> Result<Vec<String>, String> {
    let rules = rules
        .iter()
        .flat_map(|x| serde_wasm_bindgen::from_value::<Rule>(x.clone()).ok())
        .collect::<Vec<_>>();
    explain_core(&rules, s)
}
#[wasm_bindgen]
pub fn egraph(rules: Vec<JsValue>, s: &str) -> JsValue {
    let rules = rules
        .iter()
        .flat_map(|x| serde_wasm_bindgen::from_value::<Rule>(x.clone()).ok())
        .flat_map(|rule| {
            let searcher = rule.searcher.parse::<Pattern<SymbolLang>>().ok()?;
            let applier = rule.applier.parse::<Pattern<SymbolLang>>().ok()?;
            Some(Rewrite::new(rule.name.clone(), searcher, applier))
        })
        .flatten()
        .collect::<Vec<Rewrite<SymbolLang, ()>>>();
    if let Ok(expr) = s.parse::<RecExpr<SymbolLang>>() {
        let runner = Runner::default()
            .with_node_limit(100)
            // .with_explanations_enabled()
            .with_expr(&expr)
            .run(&rules);
        serde_wasm_bindgen::to_value(&runner.egraph).unwrap()
    } else {
        serde_wasm_bindgen::to_value(&()).unwrap()
    }
}

fn simplify_core(rules: &Vec<Rule>, s: &str) -> String {
    let expr = s.parse::<RecExpr<SymbolLang>>();
    let rules = rules
        .iter()
        .flat_map(|rule| {
            let searcher = rule.searcher.parse::<Pattern<SymbolLang>>().ok()?;
            let applier = rule.applier.parse::<Pattern<SymbolLang>>().ok()?;
            Some(Rewrite::new(rule.name.clone(), searcher, applier))
        })
        .flatten()
        .collect::<Vec<Rewrite<SymbolLang, ()>>>();
    match expr {
        Ok(expr) => simplify_egg(rules, expr),
        _ => "Parsing error".to_owned(),
    }
}
fn explain_core(rules: &Vec<Rule>, s: &str) -> Result<Vec<String>, String> {
    let expr = s.parse::<RecExpr<SymbolLang>>();
    let rules = rules
        .iter()
        .flat_map(|rule| {
            let searcher = rule.searcher.parse::<Pattern<SymbolLang>>().ok()?;
            let applier = rule.applier.parse::<Pattern<SymbolLang>>().ok()?;
            Some(Rewrite::new(rule.name.clone(), searcher, applier))
        })
        .flatten()
        .collect::<Vec<Rewrite<SymbolLang, ()>>>();
    expr.map(|x| explain_egg(rules, x)).map_err(|e| e.to_string())
}

fn explain_egg(rules: Vec<Rewrite<SymbolLang, ()>>, expr: RecExpr<SymbolLang>) -> Vec<String> {
    let mut runner = Runner::default()
        .with_node_limit(5000)
        .with_explanations_enabled()
        .with_expr(&expr)
        .run(&rules);
    let root = runner.roots[0];

    let extractor = Extractor::new(&runner.egraph, AstSize);
    let (_, best) = extractor.find_best(root);
    // println!("Simplified {} to {} with cost {}", expr, best, best_cost);
    runner.explain_equivalence(&expr, &best).get_flat_strings()
}
fn simplify_egg(rules: Vec<Rewrite<SymbolLang, ()>>, expr: RecExpr<SymbolLang>) -> String {
    let runner = Runner::default()
        .with_explanations_enabled()
        .with_expr(&expr)
        .run(&rules);
    let root = runner.roots[0];

    let extractor = Extractor::new(&runner.egraph, AstSize);
    let (best_cost, best) = extractor.find_best(root);
    println!("Simplified {} to {} with cost {}", expr, best, best_cost);
    best.to_string()
}
