#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Rule<Atom> {
    And(Vec<Self>),      // All rules must match
    Or(Vec<Self>),       // At least one rule must match
    Optional(Box<Self>), // The rule may or may not be present
    Atom(Atom),          // Match a single atom
}

impl<Atom> Rule<Atom> {
    pub fn match_atoms(&self, atoms: &mut Vec<Atom>) -> Result<(), Atom>
    where
        Atom: PartialEq + Clone,
    {
        match self {
            Self::Atom(expected_atom) => {
                // Try to find the expected atom in the list
                if let Some(pos) = atoms.iter().position(|atom| atom == expected_atom) {
                    // Remove the atom at the found position
                    atoms.remove(pos);
                    Ok(())
                } else {
                    Err(expected_atom.clone())
                }
            }
            Self::And(subrules) => {
                // For AND, all subrules must match
                let mut backup = atoms.clone();
                for subrule in subrules {
                    subrule.match_atoms(&mut backup)?
                }
                // Only if all subrules match, update the original atoms list
                *atoms = backup;
                Ok(())
            }
            Self::Or(subrules) => {
                let mut last_err = None;
                // For OR, only one of the subrules needs to match
                for subrule in subrules {
                    let mut backup = atoms.clone();
                    match subrule.match_atoms(&mut backup) {
                        Ok(()) => {
                            // If one subrule matches, update the original list and return true
                            *atoms = backup;
                            return Ok(());
                        }
                        Err(err) => {
                            last_err = Some(err);
                        }
                    }
                }
                Err(last_err.unwrap())
            }
            Self::Optional(subrule) => {
                // For Optional, either match or skip
                let mut backup = atoms.clone();
                let _ = subrule.match_atoms(&mut backup);
                *atoms = backup;
                Ok(())
            }
        }
    }
}

#[macro_export]
macro_rules! rule {
    // Base case for single atoms
    ($atom:ident) => {
        Rule::Atom($atom)
    };

    // Optional rule with ? - can wrap any subexpression
    (($($inner:tt)+) ?) => {
        Rule::Optional(Box::new(rule!($($inner)+)))
    };
    ($atom:ident ?) => {
        Rule::Optional(Box::new(Rule::Atom($atom)))
    };

    // Handle parenthesized expressions
    (($($inner:tt)+)) => {
        rule!($($inner)+)
    };

    // // AND operation with & (multiple operands)
    // ($first:tt & $($rest:tt)&+ $(&)?) => {{
    //     let mut rules = vec![rule!($first)];
    //     $(
    //         rules.push(rule!($rest));
    //     )+
    //     Rule::And(rules)
    // }};

    // // OR operation with | (multiple operands)
    // ($first:tt | $($rest:tt)|+ $(|)?) => {{
    //     let mut rules = vec![rule!($first)];
    //     $(
    //         rules.push(rule!($rest));
    //     )+
    //     Rule::Or(rules)
    // }};

    // binary AND operation with last ?
    ($first:tt & $second:tt+ ?) => {{
        let first = rule!($first);
        let second = rule!($second ?);
        Rule::And(vec![first, second])
    }};

    // AND operation with & (recursive)
    ($first:tt & $($rest:tt)+) => {{
        let first_rule = rule!($first);
        let rest_rule = rule!($($rest)+);
        match rest_rule {
            Rule::And(mut rules) => {
                rules.insert(0, first_rule);
                Rule::And(rules)
            },
            _ => Rule::And(vec![first_rule, rest_rule])
        }
    }};

    // AND operation with & (recursive)
    ($first:tt ? & $($rest:tt)+) => {{
        let first_rule = rule!($first ?);
        let rest_rule = rule!($($rest)+);
        match rest_rule {
            Rule::And(mut rules) => {
                rules.insert(0, first_rule);
                Rule::And(rules)
            },
            _ => Rule::And(vec![first_rule, rest_rule])
        }
    }};

    // binary OR operation with last ?
    ($first:tt | $second:tt+ ?) => {{
        let first = rule!($first);
        let second = rule!($second ?);
        Rule::Or(vec![first, second])
    }};

    // OR operation with | (recursive)
    ($first:tt | $($rest:tt)+) => {{
        let first_rule = rule!($first);
        let rest_rule = rule!($($rest)+);
        match rest_rule {
            Rule::Or(mut rules) => {
                rules.insert(0, first_rule);
                Rule::Or(rules)
            },
            _ => Rule::Or(vec![first_rule, rest_rule])
        }
    }};

    // OR operation with | (recursive)
    ($first:tt ? | $($rest:tt)+) => {{
        let first_rule = rule!($first ?);
        let rest_rule = rule!($($rest)+);
        match rest_rule {
            Rule::Or(mut rules) => {
                rules.insert(0, first_rule);
                Rule::Or(rules)
            },
            _ => Rule::Or(vec![first_rule, rest_rule])
        }
    }};
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    #[allow(non_camel_case_types)]
    enum Foo {
        A,
        B,
        C,
        D,
    }

    fn complex_grammar() -> Rule<Foo> {
        use Foo::*;
        // trace_macros!(true);

        let complex_rule = rule!((A & B & C) | (D & (B | C)) | C);

        // trace_macros!(false);
        dbg!(complex_rule)
    }

    #[test]
    fn rule_correct() {
        let rule = complex_grammar();
        assert_eq!(
            rule,
            Rule::Or(vec![
                Rule::And(vec![
                    Rule::Atom(Foo::A),
                    Rule::Atom(Foo::B),
                    Rule::Atom(Foo::C)
                ]),
                Rule::And(vec![
                    Rule::Atom(Foo::D),
                    Rule::Or(vec![Rule::Atom(Foo::B), Rule::Atom(Foo::C)])
                ]),
                Rule::Atom(Foo::C)
            ])
        );
    }

    #[test]
    fn single_c_matches() {
        let mut parts = vec![Foo::C];
        let rule = complex_grammar();
        assert!(rule.match_atoms(&mut parts).is_ok());
    }

    #[test]
    fn single_a_does_not_match() {
        let mut parts = vec![Foo::A];
        let rule = complex_grammar();
        assert!(rule.match_atoms(&mut parts).is_err());
    }

    #[test]
    fn a_d_does_not_match() {
        let mut parts = vec![Foo::A, Foo::D];
        let rule = complex_grammar();
        assert!(rule.match_atoms(&mut parts).is_err());
    }

    #[test]
    fn b_d_matches() {
        let mut parts = vec![Foo::B, Foo::D];
        let rule = complex_grammar();
        assert!(rule.match_atoms(&mut parts).is_ok());
    }

    #[test]
    fn c_d_matches() {
        let mut parts = vec![Foo::C, Foo::D];
        let rule = complex_grammar();
        assert!(rule.match_atoms(&mut parts).is_ok());
    }

    #[test]
    fn a_b_does_not_match() {
        let mut parts = vec![Foo::A, Foo::B];
        let rule = complex_grammar();
        assert!(rule.match_atoms(&mut parts).is_err());
    }

    #[test]
    fn a_b_c_matches() {
        let mut parts = vec![Foo::A, Foo::B, Foo::C];
        let rule = complex_grammar();
        assert!(rule.match_atoms(&mut parts).is_ok());
    }

    fn complex_grammar_with_optionals() -> Rule<Foo> {
        use Foo::*;
        // trace_macros!(true);

        let complex_rule = rule!((A? & (B & C)) | (D & (B | C)?));

        // trace_macros!(false);
        dbg!(complex_rule)
    }

    #[test]
    fn singles_only_d_matches() {
        let rule = complex_grammar_with_optionals();

        let mut parts = vec![Foo::A];
        assert!(rule.match_atoms(&mut parts).is_err());

        let mut parts = vec![Foo::B];
        assert!(rule.match_atoms(&mut parts).is_err());

        let mut parts = vec![Foo::C];
        assert!(rule.match_atoms(&mut parts).is_err());

        let mut parts = vec![Foo::D];
        assert!(rule.match_atoms(&mut parts).is_ok());
    }

    #[test]
    fn pairs_matches() {
        let rule = complex_grammar_with_optionals();

        let mut parts = vec![Foo::A, Foo::B];
        assert!(rule.match_atoms(&mut parts).is_err());

        let mut parts = vec![Foo::A, Foo::C];
        assert!(rule.match_atoms(&mut parts).is_err());

        // TODO: excessive A
        // let mut parts = vec![Foo::A, Foo::D];
        // assert!(rule.match_atoms(&mut parts).is_err());

        let mut parts = vec![Foo::B, Foo::C];
        assert!(rule.match_atoms(&mut parts).is_ok());

        let mut parts = vec![Foo::B, Foo::D];
        assert!(rule.match_atoms(&mut parts).is_ok());

        let mut parts = vec![Foo::C, Foo::D];
        assert!(rule.match_atoms(&mut parts).is_ok());
    }

    #[test]
    fn triples_matches() {
        let rule = complex_grammar_with_optionals();

        let mut parts = vec![Foo::A, Foo::B, Foo::C];
        assert!(rule.match_atoms(&mut parts).is_ok());

        // TODO: excessive A
        // let mut parts = vec![Foo::A, Foo::B, Foo::D];
        // assert!(rule.match_atoms(&mut parts).is_err());

        // TODO: excessive A or D
        // let mut parts = vec![Foo::A, Foo::C, Foo::D];
        // assert!(rule.match_atoms(&mut parts).is_err());

        // TODO: excessive C or D
        // let mut parts = vec![Foo::B, Foo::C, Foo::D];
        // assert!(rule.match_atoms(&mut parts).is_err());
    }
}
