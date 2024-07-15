use std::fmt::Debug;

use nom::{
    bytes::complete::tag,
    character::complete::{alphanumeric1, digit1},
    combinator::map_res,
    error::Error as NomInternalError,
    number::complete::double,
    IResult,
};

trait FormatSpecifier {
    const TEMPLATE_SYMBOL: char = '%';

    fn symbol(&self) -> char;

    fn from_symbol(ch: char) -> Option<Self>
    where
        Self: Sized;

    fn parse_format_string(format: &str) -> Result<Vec<FullFormatSpecifier<Self>>, Error>
    where
        Self: Sized,
    {
        let percent = Self::TEMPLATE_SYMBOL;

        let mut specifiers = Vec::new();
        let mut chars = format.chars().peekable();

        while let Some(c) = chars.next() {
            if c == percent {
                if let Some(ch) = chars.next() {
                    if let Some(spec) = Self::from_symbol(ch) {
                        specifiers.push(FullFormatSpecifier::Domain(spec));
                    } else {
                        // TODO: if ch == percent: start `FullFormatSpecifier::Literal("%")`
                        return Err(Error::InvalidFormatSpecifier(ch));
                    }
                } else {
                    return Err(Error::UnexpectedEol);
                }
            } else {
                let mut literal = c.to_string();
                while let Some(&next_c) = chars.peek() {
                    if next_c == percent {
                        break;
                    }
                    literal.push(chars.next().unwrap());
                }
                specifiers.push(FullFormatSpecifier::Literal(literal));
            }
        }

        Ok(specifiers)
    }
}

#[derive(Debug)]
enum FullFormatSpecifier<FS> {
    Domain(FS),
    Literal(String),
    // TODO: whitespaces
}

trait PartParser {
    type ItemSpecifier: FormatSpecifier;

    fn get_specifier(&self) -> Self::ItemSpecifier;

    fn is_valid(&self) -> bool;

    fn parse_domain<'i>(input: &'i str, format: &Self::ItemSpecifier) -> IResult<&'i str, Self>
    where
        Self: Sized;

    fn parse_single<'i>(
        input: &'i str,
        format: &FullFormatSpecifier<Self::ItemSpecifier>,
    ) -> Result<(&'i str, Option<Self>), Error>
    where
        Self: Sized + Debug,
        Self::ItemSpecifier: Debug,
    {
        match format {
            FullFormatSpecifier::Domain(format) => {
                let (rest, value) = Self::parse_domain(input, format)?;
                if value.is_valid() {
                    Ok((rest, Some(value)))
                } else {
                    Err(Error::InvalidValue {
                        msg: format!("Value {value:?} does not match requirements of {format:?}"),
                    })
                }
            }
            FullFormatSpecifier::Literal(literal) => {
                let (rest, _) = tag::<_, _, NomInternalError<_>>(literal.as_str())(input)?;
                Ok((rest, None))
            }
        }
    }

    fn parse_many<'i>(
        input: &'i str,
        format_specifiers: &[FullFormatSpecifier<Self::ItemSpecifier>],
    ) -> Result<(&'i str, Vec<Self>), Error>
    where
        Self: Sized + Debug,
        Self::ItemSpecifier: Debug,
    {
        let mut results = Vec::new();
        let mut remainder = input;

        for specifier in format_specifiers {
            let (rest, value) = Self::parse_single(remainder, specifier)?;
            if let Some(val) = value {
                results.push(val);
            }
            remainder = rest;
        }

        Ok((remainder, results))
    }

    fn parse<'i>(input: &'i str, format: &str) -> Result<(&'i str, Vec<Self>), Error>
    where
        Self: Sized + Debug,
        Self::ItemSpecifier: Debug,
    {
        let format_specifiers = Self::ItemSpecifier::parse_format_string(format)?;
        Self::parse_many(input, &format_specifiers)
    }
}

type NomError<E> = nom::Err<NomInternalError<E>>;

#[derive(Debug)]
enum Error {
    Nom(NomError<String>),
    InvalidValue { msg: String },
    InvalidFormatSpecifier(char),
    UnexpectedEol,
}

impl From<NomError<String>> for Error {
    fn from(value: NomError<String>) -> Self {
        Self::Nom(value)
    }
}

impl<'a> From<NomError<&'a str>> for Error {
    fn from(value: NomError<&'a str>) -> Self {
        Self::Nom(value.map_input(str::to_string))
    }
}

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

        impl FormatSpecifier for $ItemName {
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

        impl PartParser for $StructName {
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

// ============== THERE GOES THE IMPLEMENTATION ==============

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Pole {
    North,
    South,
}

// TODO: valid combinations

gen_parts! {
    #[derive(Debug, Copy, Clone, PartialEq)]
    enum LatitudeParts: LatitudeItem {
        Degrees(u8) = 'd' => parse_u8; |x: &u8| *x <= 90,
        DecimalDegrees(f64) = 'D' => parse_f64; |x: &f64| x.is_sign_positive() && *x <= 90.0,
        Minutes(u8) = 'm' => parse_u8; |x: &u8| *x < 60,
        Seconds(f64) = 'S' => parse_f64; |x: &f64| x.is_sign_positive() && *x <= 60.0,
        Hemisphere(Pole) = 'h' => parse_hemisphere; |_| true,
    }
}

fn parse_u8(input: &str) -> IResult<&str, u8> {
    map_res(digit1, str::parse)(input)
}

fn parse_f64(input: &str) -> IResult<&str, f64> {
    double(input)
}

fn parse_hemisphere(input: &str) -> IResult<&str, Pole> {
    let (input, hemisphere) = alphanumeric1(input)?;
    match hemisphere {
        "N" => Ok((input, Pole::North)),
        "S" => Ok((input, Pole::South)),
        _ => Err(nom::Err::Error(NomInternalError::new(
            input,
            nom::error::ErrorKind::Alpha,
        ))),
    }
}

fn main() {
    let format = "%d deg %m min %S sec (%h)";
    let input = "41 deg 37 min 19.11792 sec (N)";

    let res = LatitudeParts::parse(input, format);

    match res {
        Ok((_, values)) => println!("Parsed values: {values:?}"),
        Err(e) => eprintln!("Error parsing input: {e:?}"),
    }
}
