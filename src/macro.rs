#[macro_export]
macro_rules! gen_parts {
    (
        $(#[$outer:meta])*
        $struct_vis:vis enum $Part:ident: $Atom:ident: $AtomName:ident {
            $(
            $part:ident($part_ty:ty) as oneof {
                $(
                    $(#[$inner:ident $($args:tt)*])*
                    $field:ident($field_ty:ty) = $symbol:expr => $parse_fn:expr ; $valid:expr
                ),+ $(,)?
            }
            )+
        }
    ) => {
        $crate::unique_chars![ $($($symbol,)+)+ => "Duplicate format specifiers" ];

        $(#[$outer])*
        $struct_vis enum $Part {
          $(
            $part($part_ty),
          )+
        }

        impl From<$Atom> for $Part {
            fn from(val: $Atom) -> Self {
                match val {
                    $(
                        $(
                          $Atom::$field(x) => Self::$part(<$part_ty>::from(x)),
                        )+
                    )+
                }
            }
        }

        impl $crate::PartParser for $Part {
            type Atom = $Atom;
        }

        $(#[$outer])*
        $struct_vis enum $Atom {
            $(
            $(
                $(#[$inner $($args)*])*
                $field($field_ty),
            )+
            )+
        }

        $(#[$outer])*
        enum $AtomName {
          $(
          $(
            $field,
          )+
          )+
        }

        impl $crate::FormatSpecifier for $AtomName {
            fn symbol(&self) -> char {
                match self {
                    $(
                    $(
                      Self::$field => $symbol,
                    )+
                    )+
                }
            }

            fn from_symbol(ch: char) -> Option<Self> {
                match ch {
                    $(
                    $(
                       $symbol => Some(Self::$field),
                    )+
                    )+
                    _ => None,
                }
            }
        }

        impl $crate::AtomParser for $Atom {
            type ItemSpecifier = $AtomName;

            fn get_specifier(&self) -> Self::ItemSpecifier {
                match self {
                    $(
                    $(
                      Self::$field(_) => Self::ItemSpecifier::$field,
                    )+
                    )+
                }
            }

            fn is_valid(&self) -> bool {
                 match self {
                    $(
                    $(
                      Self::$field(x) => $valid(x),
                    )+
                    )+
                }
            }

            fn parse_domain<'i>(input: &'i str, format: &Self::ItemSpecifier) -> IResult<&'i str, Self> {
                 match format {
                    $(
                    $(
                      Self::ItemSpecifier::$field => {
                        $parse_fn(input).map(|(rest, value)| (rest, Self::$field(value)))
                      }
                    )+
                    )+
                }
            }
        }
    };
}

#[macro_export]
macro_rules! unique_chars {
    ( $( $x:expr ),+ $(,)? => $msg: expr ) => {
        mod _unique {
            // Create a constant array from the input elements
            const TEMP_ARRAY: &[char] = &[$($x),*];

            // Ensure the array length is available at compile time
            const TEMP_ARRAY_LEN: usize = TEMP_ARRAY.len();

            // Use a constant assertion to check uniqueness
            const _: () = {
                assert!($crate::check_unique::<TEMP_ARRAY_LEN>(TEMP_ARRAY), $msg);
            };
        }
    };

    ( $( $x:expr ),+ $(,)? ) => {
        $crate::unique_chars!( $($x),+ => "Duplicate values found in the input");
    }
}
