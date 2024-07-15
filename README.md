# Parse complex structure from its simpler parts using format string

Two most obvious examples are:
- date and/or time;
- coordinates;

The former is supported by the very popular [chrono crate](https://crates.io/crates/chrono)
using [strptime](https://docs.rs/chrono/latest/chrono/format/strftime/index.html#specifiers).
Also the python standard library has
a [good documentation](https://docs.python.org/3/library/datetime.html#format-codes)
on the same issue.

But the lack of any generic enough library that is able to create coordinates parser,
inspired me to write this piece.
