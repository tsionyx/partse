use std::fmt::Debug;

use nom::{
    character::complete::{alphanumeric1, digit1},
    combinator::map_res,
    number::complete::double,
    IResult,
};
use partse::{gen_parts, Error, PartParser};

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
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Alpha,
        ))),
    }
}

fn main() {
    let format = "%d deg %m min %S sec (%h)";
    let input = "72 deg 37 min 19.11792 sec (N)";

    let res = LatitudeParts::parse(input, format);

    match res {
        Ok((_, values)) => println!("Parsed values: {values:?}"),
        Err(e) => eprintln!("Error parsing input: {e:?}"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_valid_latitude() {
        let format = "%d deg %m min %S sec (%h)";
        let input = "72 deg 37 min 19.11792 sec (N)";

        let (_, values) = LatitudeParts::parse(input, format).expect("Parsing failed");

        let lat = [
            LatitudeParts::Degrees(72),
            LatitudeParts::Minutes(37),
            LatitudeParts::Seconds(19.11792),
            LatitudeParts::Hemisphere(Pole::North),
        ];
        assert_eq!(values, lat);
    }

    #[test]
    fn test_parse_valid_decimal_latitude() {
        let format = "%D (%h)";
        let input = "72.61976 (N)";

        let (_, values) = LatitudeParts::parse(input, format).expect("Parsing failed");

        let lat = [
            LatitudeParts::DecimalDegrees(72.61976),
            LatitudeParts::Hemisphere(Pole::North),
        ];
        assert_eq!(values, lat);
    }

    #[test]
    #[should_panic = "Degrees(91) does not match requirements"]
    fn test_parse_invalid_latitude() {
        let format = "%d deg %m min %S sec (%h)";
        let input = "91 deg 37 min 19.11792 sec (N)";

        let _ = LatitudeParts::parse(input, format).unwrap();
    }

    #[test]
    fn test_parse_invalid_format_specifier() {
        let format = "%d deg %m min %x sec (%h)";
        let input = "72 deg 37 min 19.11792 sec (N)";

        let err = LatitudeParts::parse(input, format).expect_err("Parsing succeeded");
        assert_eq!(err, Error::InvalidFormatSpecifier('x'));
    }

    #[test]
    fn test_unexpected_end_of_input() {
        let format = "%d deg %m min %S sec (%h)";
        let input = "72 deg 37 min 19.11792 sec";

        let err = LatitudeParts::parse(input, format).expect_err("Parsing succeeded");
        assert_eq!(err, Error::UnexpectedEol);
    }

    #[test]
    fn test_literal_parsing() {
        let format = "Latitude: %d degrees";
        let input = "Latitude: 72 degrees";

        let (_, values) = LatitudeParts::parse(input, format).expect("Parsing failed");

        let lat = [LatitudeParts::Degrees(72)];
        assert_eq!(values, lat);
    }
}

#[cfg(test)]
mod helper_tests {
    use super::*;

    #[test]
    fn test_parse_u8() {
        let input = "123";
        let res = parse_u8(input);
        assert!(res.is_ok(), "Parsing failed: {:?}", res);

        let (rest, value) = res.unwrap();
        assert_eq!(rest, "");
        assert_eq!(value, 123);
    }

    #[test]
    fn test_parse_f64() {
        let input = "123.456";
        let res = parse_f64(input);
        assert!(res.is_ok(), "Parsing failed: {:?}", res);

        let (rest, value) = res.unwrap();
        assert_eq!(rest, "");
        assert_eq!(value, 123.456);
    }

    #[test]
    fn test_parse_hemisphere_north() {
        let input = "N";
        let res = parse_hemisphere(input);
        assert!(res.is_ok(), "Parsing failed: {:?}", res);

        let (rest, value) = res.unwrap();
        assert_eq!(rest, "");
        assert_eq!(value, Pole::North);
    }

    #[test]
    fn test_parse_hemisphere_south() {
        let input = "S";
        let res = parse_hemisphere(input);
        assert!(res.is_ok(), "Parsing failed: {:?}", res);

        let (rest, value) = res.unwrap();
        assert_eq!(rest, "");
        assert_eq!(value, Pole::South);
    }

    #[test]
    fn test_parse_hemisphere_invalid() {
        let input = "X";
        let res = parse_hemisphere(input);
        assert!(res.is_err(), "Parsing succeeded with invalid hemisphere");
    }
}
