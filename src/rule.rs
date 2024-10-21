use thiserror::Error;

/// Defines the rules to match combinations of atoms in a (unordered) set
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Rule<Atom> {
    And(Box<[Self]>),    // All rules must match
    Or(Box<[Self]>),     // At least one rule must match
    Optional(Box<Self>), // The rule may or may not be present
    Atom(Atom),          // Match a single atom
}

impl<Atom> Rule<Atom> {
    /// Shortcut to create the set of AND conditions.
    pub fn and(args: Vec<Self>) -> Self {
        Self::And(args.into_boxed_slice())
    }

    /// Shortcut to create the set of OR conditions.
    ///
    /// # Error
    /// - [`Error::EmptyOrList`] if the rule itself contains empty [`Rule::Or`] somewhere in it.
    pub fn or(args: Vec<Self>) -> Result<Self, Error<Atom>> {
        if args.is_empty() {
            Err(Error::EmptyOrList)
        } else {
            Ok(Self::Or(args.into_boxed_slice()))
        }
    }

    /// Shortcut to create the set of OR conditions with at least one element.
    pub fn or_1(head: Self, tail: impl Into<Vec<Self>>) -> Self {
        let mut args = tail.into();
        args.insert(0, head);
        Self::Or(args.into_boxed_slice())
    }
}

#[derive(Debug, PartialEq, Eq, Error)]
pub enum Error<Atom> {
    #[error("Detected empty OR condition: match nothing")]
    EmptyOrList,
    #[error("The atom {0:?} repeats at least twice")]
    RepeatingAtom(Atom),
    #[error("The atom {0:?} is missing")]
    MissingAtom(Atom),
    #[error("Left unused atoms after validation: {0:?}")]
    ExcessAtoms(Vec<Atom>),
}

impl<Atom> Rule<Atom>
where
    Atom: PartialEq + Clone,
{
    /// Validate the given list of atoms matches the defintion of a set.
    ///
    /// # Error
    /// - [`Error::EmptyOrList`] if the rule itself contains empty [`Rule::Or`] somewhere in it.
    /// - [`Error::RepeatingAtom`] if the list contains one of the atom twice.
    /// - [`Error::MissingAtom`] if an `Atom` is missing in the given list.
    /// - [`Error::ExcessAtoms`] if the list contains more atoms that is needed for the full match.
    pub fn validate_atoms(&self, atoms: Vec<Atom>) -> Result<(), Error<Atom>> {
        for (i, atom) in atoms.iter().enumerate() {
            if atoms[i + 1..].contains(atom) {
                return Err(Error::RepeatingAtom(atom.clone()));
            }
        }

        let mut atoms = atoms;
        self.match_atoms(&mut atoms)
            .map_err(|atom| atom.map_or(Error::EmptyOrList, Error::MissingAtom))?;
        if !atoms.is_empty() {
            Err(Error::ExcessAtoms(atoms))
        } else {
            Ok(())
        }
    }

    /// Try to recursively match the given list of atoms
    /// with the defintion of rule(s).
    ///
    /// # Error
    ///
    /// - `Some(Atom)` if an `Atom` is missing in the given list;
    /// - `None` if the rule itself contains empty [`Rule::Or`] somewhere in it.
    fn match_atoms(&self, atoms: &mut Vec<Atom>) -> Result<(), Option<Atom>> {
        match self {
            Self::Atom(expected_atom) => {
                // Try to find the expected atom in the list
                if let Some(pos) = atoms.iter().position(|atom| atom == expected_atom) {
                    // Remove the atom at the found position
                    atoms.remove(pos);
                    Ok(())
                } else {
                    Err(Some(expected_atom.clone()))
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
                            // If one subrule matches, update the original list
                            *atoms = backup;
                            return Ok(());
                        }
                        Err(err) => {
                            last_err = Some(err);
                        }
                    }
                }
                Err(last_err.flatten())
            }
            Self::Optional(subrule) => {
                let _ = subrule.match_atoms(atoms);
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
    //     Rule::and(rules)
    // }};

    // // OR operation with | (multiple operands)
    // ($first:tt | $($rest:tt)|+ $(|)?) => {{
    //     let mut rules = vec![];
    //     $(
    //         rules.push(rule!($rest));
    //     )+
    //     Rule::or_1(rule!($first), rules)
    // }};

    // binary AND operation using '&' with the second optional
    ($first:tt & $second:tt+ ?) => {{
        let first = rule!($first);
        let second = rule!($second ?);
        Rule::and(vec![first, second])
    }};

    // AND operation(s) using '&' (recursive)
    ($first:tt & $($rest:tt)+) => {{
        let first_rule = rule!($first);
        let rest_rule = rule!($($rest)+);
        match rest_rule {
            Rule::And(rules) => {
                let mut rules: Vec<_> = rules.into();
                rules.insert(0, first_rule);
                Rule::and(rules)
            },
            _ => Rule::and(vec![first_rule, rest_rule])
        }
    }};

    // AND operation(s) using '&' with the first optional (recursive)
    ($first:tt ? & $($rest:tt)+) => {{
        let first_rule = rule!($first ?);
        let rest_rule = rule!($($rest)+);
        match rest_rule {
            Rule::And(rules) => {
                let mut rules: Vec<_> = rules.into();
                rules.insert(0, first_rule);
                Rule::and(rules)
            },
            _ => Rule::and(vec![first_rule, rest_rule])
        }
    }};

    // binary OR operation using '|' with the second optional
    ($first:tt | $second:tt+ ?) => {{
        let first = rule!($first);
        let second = rule!($second ?);
        Rule::or_1(first, vec![second])
    }};

    // OR operation(s) using '|' (recursive)
    ($first:tt | $($rest:tt)+) => {{
        let first_rule = rule!($first);
        let rest_rule = rule!($($rest)+);
        match rest_rule {
            Rule::Or(rules) => {
                Rule::or_1(first_rule, rules)
            },
            _ => Rule::or_1(first_rule, vec![rest_rule])
        }
    }};

    // OR operation(s) using '|' with the first optional (recursive)
    ($first:tt ? | $($rest:tt)+) => {{
        let first_rule = rule!($first ?);
        let rest_rule = rule!($($rest)+);
        match rest_rule {
            Rule::Or(rules) => {
                Rule::or_1(first_rule, rules)
            },
            _ => Rule::or_1(first_rule, vec![rest_rule])
        }
    }};
}

#[cfg(test)]
mod tests {
    #![allow(non_snake_case)]
    use super::*;

    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    #[allow(non_camel_case_types)]
    enum Foo {
        A,
        B,
        C,
        D,
    }

    #[test]
    fn repeating_atoms() {
        use Foo::*;

        let rule = rule!(A);
        assert_eq!(rule, Rule::Atom(A));

        assert_eq!(
            rule.validate_atoms(vec![A, A]).unwrap_err(),
            Error::RepeatingAtom(A)
        );

        assert_eq!(
            rule.validate_atoms(vec![A, B, A]).unwrap_err(),
            Error::RepeatingAtom(A)
        );
    }

    #[test]
    fn grammar_empty() {
        use Foo::*;

        // cannot create the empty OR with the macro
        let rule = Rule::and(vec![Rule::Atom(A), Rule::Or(vec![].into_boxed_slice())]);

        assert_eq!(
            rule.validate_atoms(vec![A]).unwrap_err(),
            Error::EmptyOrList
        );
    }

    mod without_optionals {
        use super::*;

        fn grammar() -> Rule<Foo> {
            use Foo::*;

            let complex_rule = rule!((A & B & C) | (D & (B | C)) | C);

            dbg!(complex_rule)
        }

        #[test]
        fn rule_correct() {
            let rule = grammar();
            assert_eq!(
                rule,
                // (A & B & C) | (D & (B | C)) | C
                Rule::or(vec![
                    Rule::and(vec![
                        Rule::Atom(Foo::A),
                        Rule::Atom(Foo::B),
                        Rule::Atom(Foo::C)
                    ]),
                    Rule::and(vec![
                        Rule::Atom(Foo::D),
                        Rule::or(vec![Rule::Atom(Foo::B), Rule::Atom(Foo::C)]).unwrap()
                    ]),
                    Rule::Atom(Foo::C)
                ])
                .unwrap()
            );
        }

        #[test]
        fn empty_does_not_match() {
            let rule = grammar();
            assert_eq!(
                rule.validate_atoms(vec![]).unwrap_err(),
                Error::MissingAtom(Foo::C)
            );
        }

        #[test]
        fn singles_only_c_matches() {
            let rule = grammar();

            let parts = vec![Foo::A];
            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::MissingAtom(Foo::C)
            );

            let parts = vec![Foo::B];
            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::MissingAtom(Foo::C)
            );

            let parts = vec![Foo::C];
            rule.validate_atoms(parts).unwrap();

            let parts = vec![Foo::D];
            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::MissingAtom(Foo::C)
            );
        }

        #[test]
        fn pairs_matches() {
            let rule = grammar();

            let parts = vec![Foo::A, Foo::B];
            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::MissingAtom(Foo::C)
            );

            let parts = vec![Foo::A, Foo::C];
            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::ExcessAtoms(vec![Foo::A])
            );

            let parts = vec![Foo::A, Foo::D];
            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::MissingAtom(Foo::C)
            );

            let parts = vec![Foo::B, Foo::C];
            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::ExcessAtoms(vec![Foo::B])
            );

            let parts = vec![Foo::B, Foo::D];
            rule.validate_atoms(parts).unwrap();

            let parts = vec![Foo::C, Foo::D];
            rule.validate_atoms(parts).unwrap();
        }

        #[test]
        fn triples_matches() {
            let rule = grammar();

            let parts = vec![Foo::A, Foo::B, Foo::C];
            rule.validate_atoms(parts).unwrap();

            let parts = vec![Foo::A, Foo::B, Foo::D];
            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::ExcessAtoms(vec![Foo::A])
            );

            let parts = vec![Foo::A, Foo::C, Foo::D];
            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::ExcessAtoms(vec![Foo::A])
            );

            let parts = vec![Foo::B, Foo::C, Foo::D];
            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::ExcessAtoms(vec![Foo::C])
            );
        }

        #[test]
        fn all_does_not_match() {
            let rule = grammar();

            let parts = vec![Foo::D, Foo::A, Foo::B, Foo::C];

            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::ExcessAtoms(vec![Foo::D])
            );
        }
    }

    mod with_optionals_in_AND_operation {
        use super::*;

        fn grammar() -> Rule<Foo> {
            use Foo::*;

            let complex_rule = rule!((A? & (B & C)) | (D & (B | C)?));

            dbg!(complex_rule)
        }

        #[test]
        fn rule_correct() {
            let rule = grammar();
            assert_eq!(
                rule,
                Rule::or(vec![
                    // (A? & (B & C)) | (D & (B | C)?)
                    Rule::and(vec![
                        Rule::Optional(Box::new(Rule::Atom(Foo::A))),
                        // automatically open the parentheses
                        Rule::Atom(Foo::B),
                        Rule::Atom(Foo::C)
                    ]),
                    Rule::and(vec![
                        Rule::Atom(Foo::D),
                        Rule::Optional(Box::new(
                            Rule::or(vec![Rule::Atom(Foo::B), Rule::Atom(Foo::C)]).unwrap()
                        ))
                    ]),
                ])
                .unwrap()
            );
        }

        #[test]
        fn empty_does_not_match() {
            let rule = grammar();
            assert_eq!(
                rule.validate_atoms(vec![]).unwrap_err(),
                Error::MissingAtom(Foo::D)
            );
        }

        #[test]
        fn singles_only_d_matches() {
            let rule = grammar();

            let parts = vec![Foo::A];
            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::MissingAtom(Foo::D)
            );

            let parts = vec![Foo::B];
            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::MissingAtom(Foo::D)
            );

            let parts = vec![Foo::C];
            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::MissingAtom(Foo::D)
            );

            let parts = vec![Foo::D];
            rule.validate_atoms(parts).unwrap();
        }

        #[test]
        fn pairs_matches() {
            let rule = grammar();

            let parts = vec![Foo::A, Foo::B];
            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::MissingAtom(Foo::D)
            );

            let parts = vec![Foo::A, Foo::C];
            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::MissingAtom(Foo::D)
            );

            let parts = vec![Foo::A, Foo::D];
            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::ExcessAtoms(vec![Foo::A])
            );

            let parts = vec![Foo::B, Foo::C];
            rule.validate_atoms(parts).unwrap();

            let parts = vec![Foo::B, Foo::D];
            rule.validate_atoms(parts).unwrap();

            let parts = vec![Foo::C, Foo::D];
            rule.validate_atoms(parts).unwrap();
        }

        #[test]
        fn triples_matches() {
            let rule = grammar();

            let parts = vec![Foo::A, Foo::B, Foo::C];
            rule.validate_atoms(parts).unwrap();

            let parts = vec![Foo::A, Foo::B, Foo::D];
            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::ExcessAtoms(vec![Foo::A])
            );

            let parts = vec![Foo::A, Foo::C, Foo::D];
            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::ExcessAtoms(vec![Foo::A])
            );

            let parts = vec![Foo::B, Foo::C, Foo::D];
            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::ExcessAtoms(vec![Foo::D])
            );
        }

        #[test]
        fn all_does_not_match() {
            let rule = grammar();

            let parts = vec![Foo::D, Foo::A, Foo::B, Foo::C];

            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::ExcessAtoms(vec![Foo::D])
            );
        }
    }

    mod with_optionals_in_OR_operation {
        use super::*;

        fn grammar() -> Rule<Foo> {
            use Foo::*;

            let complex_rule = rule!((((A & C)? | (B & C)) & D) | (B & (C | (A & D)?)));

            dbg!(complex_rule)
        }

        #[test]
        fn rule_correct() {
            let rule = grammar();
            assert_eq!(
                rule,
                Rule::or(vec![
                    // (((A & C)? | (B & C)) & D) | (B & (C | (A & D)?))
                    Rule::and(vec![
                        Rule::or(vec![
                            Rule::Optional(Box::new(Rule::and(vec![
                                Rule::Atom(Foo::A),
                                Rule::Atom(Foo::C)
                            ]))),
                            Rule::and(vec![Rule::Atom(Foo::B), Rule::Atom(Foo::C)]),
                        ])
                        .unwrap(),
                        Rule::Atom(Foo::D)
                    ]),
                    Rule::and(vec![
                        Rule::Atom(Foo::B),
                        Rule::or(vec![
                            Rule::Atom(Foo::C),
                            Rule::Optional(Box::new(Rule::and(vec![
                                Rule::Atom(Foo::A),
                                Rule::Atom(Foo::D)
                            ])))
                        ])
                        .unwrap()
                    ])
                ])
                .unwrap()
            );
        }

        #[test]
        fn empty_matches() {
            let rule = grammar();
            assert_eq!(
                rule.validate_atoms(vec![]).unwrap_err(),
                Error::MissingAtom(Foo::B)
            );
        }

        #[test]
        fn singles_b_and_d_matches() {
            let rule = grammar();

            let parts = vec![Foo::A];
            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::MissingAtom(Foo::B)
            );

            let parts = vec![Foo::B];
            rule.validate_atoms(parts).unwrap();

            let parts = vec![Foo::C];
            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::MissingAtom(Foo::B)
            );

            let parts = vec![Foo::D];
            rule.validate_atoms(parts).unwrap();
        }

        #[test]
        fn pairs_matches() {
            let rule = grammar();

            let parts = vec![Foo::A, Foo::B];
            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::ExcessAtoms(vec![Foo::A])
            );

            let parts = vec![Foo::A, Foo::C];
            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::MissingAtom(Foo::B)
            );

            let parts = vec![Foo::A, Foo::D];
            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::ExcessAtoms(vec![Foo::A])
            );

            let parts = vec![Foo::B, Foo::C];
            rule.validate_atoms(parts).unwrap();

            let parts = vec![Foo::B, Foo::D];
            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::ExcessAtoms(vec![Foo::B])
            );

            let parts = vec![Foo::C, Foo::D];
            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::ExcessAtoms(vec![Foo::C])
            );
        }

        #[test]
        fn triples_matches() {
            let rule = grammar();

            let parts = vec![Foo::A, Foo::B, Foo::C];
            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::ExcessAtoms(vec![Foo::A])
            );

            let parts = vec![Foo::A, Foo::B, Foo::D];
            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::ExcessAtoms(vec![Foo::A, Foo::B])
            );

            let parts = vec![Foo::A, Foo::C, Foo::D];
            rule.validate_atoms(parts).unwrap();

            let parts = vec![Foo::B, Foo::C, Foo::D];
            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::ExcessAtoms(vec![Foo::B, Foo::C])
            );
        }

        #[test]
        fn all_does_not_match() {
            let rule = grammar();

            let parts = vec![Foo::D, Foo::A, Foo::B, Foo::C];

            assert_eq!(
                rule.validate_atoms(parts).unwrap_err(),
                Error::ExcessAtoms(vec![Foo::B])
            );
        }
    }
}
