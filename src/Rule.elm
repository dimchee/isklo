module Rule exposing (..)


type alias Rule =
    { name : String
    , searcher : String
    , applier : String
    }


revRule : Rule -> Rule
revRule { searcher, applier, name } =
    { searcher = applier, applier = searcher, name = name }


arithmeticRules : List Rule
arithmeticRules =
    [ { name = "commute-add", searcher = "(+ ?a ?b)", applier = "(+ ?b ?a)" }
    , { name = "commute-mul", searcher = "(* ?a ?b)", applier = "(* ?b ?a)" }
    , { name = "add-0", searcher = "(+ ?a 0)", applier = "?a" }
    , { name = "mul-0", searcher = "(* ?a 0)", applier = "0" }
    , { name = "mul-1", searcher = "(* ?a 1)", applier = "?a" }
    , { name = "lnot", searcher = "(¬ ⊤)", applier = "⊥" }
    , { name = "lnot", searcher = "(¬ ⊥)", applier = "⊤" }
    ]


logicRules : List Rule
logicRules =
    [ { searcher = "(∧ ⊤ ?p)", applier = "?p", name = "top_land" }
    , { searcher = "(∨ ⊤ ?p)", applier = "⊤", name = "top_lor" }
    , { searcher = "(∧ ⊥ ?p)", applier = "⊥", name = "bot_land" }
    , { searcher = "(∨ ⊥ ?p)", applier = "?p", name = "bot_lor" }
    , { searcher = "(∨ ?p (¬ ?p))", applier = "⊤", name = "p_lor_lnot_p" }
    , { searcher = "(¬ (¬ ?p))", applier = "?p", name = "lnot_lnot_p" }
    , { searcher = "(⇒ (¬ ?q) (¬ ?p))", applier = "(⇒ ?p ?q)", name = "kontrapozicija" }
    , { searcher = "(¬ (∧ ?p ?q))", applier = "(∨ (¬ ?p) (¬ ?q))", name = "demorgan_land" }
    , { searcher = "(¬ (∨ ?p ?q))", applier = "(∧ (¬ ?p) (¬ ?q))", name = "demorgan_lor" }
    , { searcher = "(⇒ ?p (⇒ ?q ?r))", applier = "(⇒ (∧ ?p ?q) ?r)", name = "curry" }
    , { searcher = "(∧ ?p ?q)", applier = "(∧ ?q ?p)", name = "commut_land" }
    , { searcher = "(∨ ?p ?q)", applier = "(∨ ?q ?p)", name = "commut_lor" }
    , { searcher = "(⇔ ?p ?q)", applier = "(⇔ ?q ?p)", name = "commut_equiv" }
    , { searcher = "(⇒ ?p ?q)", applier = "(∨ (¬ ?p) ?q)", name = "impl_def" }
    , { searcher = "(⇔ ?p ?q)", applier = "(∧ (⇒ ?p ?q) (⇒ ?q ?p))", name = "iff_def" }
    , { searcher = "(∨ ?p (∧ ?p ?q))", applier = "?p", name = "absorb_lor" }
    , { searcher = "(∧ ?p (∨ ?p ?q))", applier = "?p", name = "absorb_land" }
    , { searcher = "(∧ ?p ?p)", applier = "?p", name = "idemp_land" }
    , { searcher = "(∨ ?p ?p)", applier = "?p", name = "idemp_lor" }
    , { searcher = "(⇔ ?p ?p)", applier = "⊤", name = "p_equiv_p_top" }
    , { searcher = "(∧ ?p (∧ ?q ?r))", applier = "(∧ (∧ ?p ?q) ?r)", name = "assoc_land" }
    , { searcher = " (∨ ?p (∨ ?q ?r))", applier = "(∨ (∨ ?p ?q) ?r)", name = "assoc_lor" }
    , { searcher = " (∨ (∧ ?p ?q) (∧ ?p ?r))", applier = "(∧ ?p (∨ ?q ?r))", name = "dist_land_lor" }
    , { searcher = " (∧ (∨ ?p ?q) (∨ ?p ?r))", applier = "(∨ ?p (∧ ?q ?r))", name = "dist_lor_land" }
    ]
