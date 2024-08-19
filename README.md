This crate provides a function `indentsort` which performs structure-preserving sorting of
arbitrary text that is structured using indentation.

```text
// input                      // output
foo {                         foo {
    b,                            a: 0,
    c: {                          b,
        9,         --->           c: {
        1,                            1,    
    },                                9,
    a: 0,                         },
}                             }
```

Some details to note:
* Empty lines are not preserved.
* By default, no syntax is parsed. Everything is treated as plain text. Note in the output of the
  example below how the comma separating the two blocks `foo{}` and `bar{}` ends up at the bottom
  and how there is no longer a comma separating the two blocks.
* An [option](Options::LeaveSquareBracketedUnsorted) is available for leaving square-bracketed
  lists unsorted.

# Example
```rust
use indentsort::{indentsort, Options};

let sorted = indentsort(Options::Default, r#"
foo {
    b,
    c,
    a,
},

bar {
    xyz: 123,
    nums: [
        3,
        2,
        1,
    ],
}"#
);

assert_eq!(sorted.join("\n"),
r#"bar {
    nums: [
        1,
        2,
        3,
    ],
    xyz: 123,
}
foo {
    a,
    b,
    c,
},"#
);
```

# Motivation

`indentsort` provides a cheap and cheerful way to do equality testing on nested,
indentation-structured text where the ordering of elements is not guaranteed. The original
motivating example was a need for equality testing over a complex Rust struct where deriving
`PartialEq, Eq` was not an option, but `Debug` was available for pretty-printing.

Equality testing of complex objects by string comparison is likely to be a bad idea in
general, so for production you probably want to parse your text properly instead of using
something like this.
