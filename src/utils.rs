/// Checks in compile time the characters provided are all unique.
///
/// # Note
/// Cannot make this function generic because
/// of unsupported yet syntax `~const PartialEq`.
pub const fn check_unique<const N: usize>(items: &'static [char]) -> bool {
    let mut i = 0;
    while i < N {
        let mut j = i + 1;
        while j < N {
            if items[i] == items[j] {
                return false;
            }
            j += 1;
        }
        i += 1;
    }
    true
}
