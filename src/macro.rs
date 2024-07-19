#[macro_export]
macro_rules! gen_parts {
    (
        $(#[$outer:meta])*
        $struct_vis:vis enum $StructName:ident: $ItemName:ident {
            $(
                $(#[$inner:ident $($args:tt)*])*
                $field:ident($field_ty:ty) = $symbol:expr => $parse_fn:expr ; $valid:expr
            ),+ $(,)?
        }
    ) => {
        $crate::unique_chars![ $($symbol,)+ => "Duplicate format specifiers" ];

        $(#[$outer])*
        $struct_vis enum $StructName {
            $(
                $(#[$inner $($args)*])*
                $field($field_ty),
            )+
        }

        $(#[$outer])*
        enum $ItemName {
          $(
            $field,
          )+
        }

        impl $crate::FormatSpecifier for $ItemName {
            fn symbol(&self) -> char {
                match self {
                    $(
                      Self::$field => $symbol,
                    )+
                }
            }

            fn from_symbol(ch: char) -> Option<Self> {
                match ch {
                    $(
                       $symbol => Some(Self::$field),
                    )+
                    _ => None,
                }
            }
        }

        impl $crate::PartParser for $StructName {
            type ItemSpecifier = $ItemName;

            fn get_specifier(&self) -> Self::ItemSpecifier {
                match self {
                    $(
                      Self::$field(_) => Self::ItemSpecifier::$field,
                    )+
                }
            }

            fn is_valid(&self) -> bool {
                match self {
                    $(
                      Self::$field(x) => $valid(x),
                    )+
                }
            }

            fn parse_domain<'i>(input: &'i str, format: &Self::ItemSpecifier) -> IResult<&'i str, Self> {
                match format {
                    $(
                      Self::ItemSpecifier::$field => {
                        $parse_fn(input).map(|(rest, value)| (rest, Self::$field(value)))
                      }
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
