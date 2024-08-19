// MIT License
//
// Copyright (c) 2024 Mikael Forsberg (github.com/mkforsb)

#![forbid(unsafe_code)]
#![forbid(missing_docs)]
#![forbid(missing_debug_implementations)]
#![forbid(clippy::missing_docs_in_private_items)]
#![forbid(rustdoc::missing_crate_level_docs)]
#![warn(clippy::all)]
#![warn(clippy::pedantic)]
#![doc = include_str!("../README.md")]

use std::cmp::Ordering;

/// Option flags.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum Options {
    /// Default options.
    ///
    /// The default options will assume the that indented blocks are surrounded / delimited by an
    /// opening line above the block and a closing line below the block, e.g:
    ///
    /// ```text
    /// Foo {
    ///     y: 4,
    ///     x: [
    ///         3,
    ///         1,
    ///     ],
    ///     z: 92,
    /// }
    /// ```
    #[default]
    Default,

    /// Consider blocks to end immediately once a line appears with lesser indentation (or the
    /// end of the text is reached), without including said line in the chunk containing the
    /// block. Suitable for e.g Python-style indentation.
    NoClosingLine,

    /// Lines inside a block delimited by square brackets should not be sorted.
    LeaveSquareBracketedUnsorted,

    /// Multiple options simultaneously.
    ///
    /// Not intended to be used directly; use [`Options::new`] to set multiple options.
    Sum(usize),
}

impl Options {
    /// Select options.
    ///
    /// If passed `None`, default options will be used (equivalent to `Options::Default`).
    /// If passed `Some([ .. ])`, all options present in the slice will be enabled. If passing a
    /// slice with more than one option, including `Options::Default` in the slice will have no
    /// effect.
    #[allow(clippy::missing_panics_doc, clippy::unnecessary_unwrap)]
    pub fn new(opts: Option<&[Options]>) -> Options {
        if opts.is_none() {
            Options::Default
        } else if opts.is_some_and(|o| o.len() == 1) {
            opts.unwrap()[0]
        } else {
            Options::Sum(opts.unwrap().iter().map(Options::value).sum())
        }
    }

    /// Retrieve the "bitset" value of an option.
    fn value(&self) -> usize {
        match self {
            Options::Default => 0,
            Options::NoClosingLine => 1,
            Options::LeaveSquareBracketedUnsorted => 2,
            Options::Sum(n) => *n,
        }
    }

    /// Check if the option matches or contains one or more given options.
    fn contains(&self, value: Options) -> bool {
        if value == Options::Default {
            *self == Options::Default
        } else {
            self.value() & value.value() == value.value()
        }
    }
}

/// Text chunk type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ChunkType {
    /// Chunk contains a single line.
    SingleLine,

    /// Chunk is the start of a sequence of lines.
    Head,

    /// Chunk is part of the sequences of lines starting at most recent `Head`.
    Tail,
}

/// Text chunk storage.
#[derive(Debug, PartialEq, Eq)]
struct Chunk<'a> {
    /// Text chunk type.
    typ: ChunkType,

    /// Lines contained in chunk.
    lines: Vec<&'a str>,

    /// Indent as determined by first line in `lines`.
    indent: usize,
}

/// Order chunks by contents of first line.
///
/// As an example of the idea, running `chunk_sort(..)` on the following chunks:
/// ```text
/// [
///     Chunk { typ: ChunkType::SingleLine, lines: vec!["ccc", "aaa"], indent: 0 }
/// ]
/// ```
fn chunk_sort<'a>(left: &Chunk<'a>, right: &Chunk<'a>) -> Ordering {
    left.lines[0].cmp(right.lines[0])
}

/// Find all ranges `(startindex, endindex)` of consecutive chunks with indent equal to `target`.
///
/// As an example of the idea, running `get_indent_groups(.., 4)` should produce `[(1,2), (5,5)]`
/// on the following text, where line numbers have been added for clarity:
/// ```text
/// 0|  foo:
/// 1|     a
/// 2|     b
/// 3|  bar
/// 4|  baz:
/// 5|     c
/// ```
fn get_indent_groups(chunks: &[Chunk<'_>], target: usize) -> Vec<(usize, usize)> {
    chunks
        .iter()
        .enumerate()
        .filter_map(|(index, chunk)| {
            if chunk.indent == target {
                Some(index)
            } else {
                None
            }
        })
        .fold(vec![], |mut acc, x| {
            if acc.is_empty() || x != acc.last().unwrap().1 + 1 {
                acc.push((x, x));
            } else {
                acc.last_mut().unwrap().1 = x;
            }
            acc
        })
}

/// Transform all sequences of `[Head, Tail, ..., Tail]` into a single `Head` containing all of the
/// lines of the sequence.
///
/// As an example of the idea, running `join_chunked(..)` on the following set of chunks:
/// ```text
/// [
///     Chunk { typ: ChunkType::SingleLine, .. },
///     Chunk { typ: ChunkType::Head, lines: ["a"] },
///     Chunk { typ: ChunkType::Tail, lines: ["b"] },
///     Chunk { typ: ChunkType::Tail, lines: ["b"] },
///     Chunk { typ: ChunkType::SingleLine, .. },
/// ]
/// ```
/// .. should produce the following result:
/// ```text
/// [
///     Chunk { typ: ChunkType::SingleLine, .. },
///     Chunk { typ: ChunkType::Head, lines: ["a", "b", "c"] },
///     Chunk { typ: ChunkType::SingleLine, .. },
/// ]
/// ```
fn join_chunked(chunks: Vec<Chunk>) -> Vec<Chunk> {
    chunks.into_iter().fold(vec![], |mut acc, x| {
        match x.typ {
            ChunkType::SingleLine | ChunkType::Head => acc.push(x),
            ChunkType::Tail => {
                debug_assert!(matches!(acc.last_mut().unwrap().typ, ChunkType::Head));
                acc.last_mut().unwrap().lines.extend(x.lines);
            }
        }
        acc
    })
}

/// Perform a structure-preserving lexicographic sort over arbitrary text that is structured using
/// indentation.
///
/// # Examples
///
/// ```
/// use indentsort::{indentsort, Options};
///
/// assert_eq!(
///     indentsort(Default::default(), "symbols(\n  foo,\n  bar,\n  qux,\n)\n").join("\n"),
///     "symbols(\n  bar,\n  foo,\n  qux,\n)"
/// );
///
/// assert_eq!(
///     indentsort(
///         Default::default(),
///         "symbols(\n  foo,\n  bar,\n  qux,\n),\naffinity: full,"
///     ).join("\n"),
///     "affinity: full,\nsymbols(\n  bar,\n  foo,\n  qux,\n),"
/// );
///
/// assert_eq!(
///     indentsort(
///         Default::default(),
///         "symbols(\n  foo,\n  bar: [\n    7,\n    1,\n  ],\n  qux,\n),\naffinity: full,"
///     ).join("\n"),
///     "affinity: full,\nsymbols(\n  bar: [\n    1,\n    7,\n  ],\n  foo,\n  qux,\n),"
/// );
///
/// assert_eq!(
///     indentsort(
///         Options::LeaveSquareBracketedUnsorted,
///         "symbols(\n  foo,\n  bar: [\n    7,\n    1,\n  ],\n  qux,\n),\naffinity: full,"
///     ).join("\n"),
///     "affinity: full,\nsymbols(\n  bar: [\n    7,\n    1,\n  ],\n  foo,\n  qux,\n),"
/// );
/// ```
pub fn indentsort(opts: Options, text: &str) -> Vec<&str> {
    let mut chunks = text
        .trim()
        .lines()
        .filter_map(|s| {
            if s.trim().is_empty() {
                None
            } else {
                Some(Chunk {
                    typ: ChunkType::SingleLine,
                    lines: vec![s],
                    indent: s.len() - s.trim_start().len(),
                })
            }
        })
        .collect::<Vec<_>>();

    if chunks.is_empty() {
        return vec![];
    }

    let mut distinct_indents = chunks.iter().map(|node| node.indent).collect::<Vec<_>>();
    distinct_indents.sort_unstable();
    distinct_indents.reverse();
    distinct_indents.dedup();

    for indent in &distinct_indents[0..(distinct_indents.len() - 1)] {
        for group in get_indent_groups(&chunks, *indent).iter().rev() {
            chunks[group.0 - 1].typ = ChunkType::Head;

            if !(opts.contains(Options::LeaveSquareBracketedUnsorted)
                && chunks[group.0 - 1].lines[0].trim_end().ends_with('['))
            {
                chunks[(group.0)..=(group.1)].sort_by(chunk_sort);
            }

            let end = if opts.contains(Options::NoClosingLine) {
                group.1
            } else {
                group.1 + 1
            };

            for chunk in &mut chunks[(group.0)..=end] {
                chunk.typ = ChunkType::Tail;
            }

            chunks = join_chunked(chunks);
        }
    }

    chunks.sort_by(chunk_sort);

    chunks.into_iter().fold(vec![], |mut acc, x| {
        acc.extend(x.lines);
        acc
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_options() {
        let opts = Options::new(None);

        assert_eq!(opts, Options::Default);
        assert_eq!(opts, Options::default());

        let opts = Options::new(Some(&[Options::Default]));
        assert_eq!(opts, Options::Default);

        let opts = Options::new(Some(&[Options::NoClosingLine]));
        assert_eq!(opts, Options::NoClosingLine);
        assert!(opts.contains(Options::NoClosingLine));

        let opts = Options::new(Some(&[Options::LeaveSquareBracketedUnsorted]));
        assert_eq!(opts, Options::LeaveSquareBracketedUnsorted);
        assert!(opts.contains(Options::LeaveSquareBracketedUnsorted));

        let opts = Options::new(Some(&[
            Options::NoClosingLine,
            Options::LeaveSquareBracketedUnsorted,
        ]));
        assert!(opts.contains(Options::NoClosingLine));
        assert!(opts.contains(Options::LeaveSquareBracketedUnsorted));
        assert!(!opts.contains(Options::Default));
    }

    #[test]
    fn test_chunk_sort() {
        let mut chunks = [
            Chunk {
                typ: ChunkType::Head,
                lines: vec!["bbb", "ccc"],
                indent: 0,
            },
            Chunk {
                typ: ChunkType::Head,
                lines: vec!["aaa", "ddd"],
                indent: 0,
            },
        ];

        chunks.sort_by(chunk_sort);

        assert_eq!(chunks[0].lines, vec!["aaa", "ddd"]);
        assert_eq!(chunks[1].lines, vec!["bbb", "ccc"]);
    }

    #[test]
    fn test_get_indent_groups() {
        fn chunk(indent: usize) -> Chunk<'static> {
            Chunk {
                typ: ChunkType::SingleLine,
                lines: vec![],
                indent,
            }
        }

        let chunks = [
            chunk(0),
            chunk(4),
            chunk(4),
            chunk(4),
            chunk(0),
            chunk(4),
            chunk(8),
            chunk(8),
        ];

        assert_eq!(get_indent_groups(&chunks, 8), vec![(6, 7)]);
        assert_eq!(get_indent_groups(&chunks, 4), vec![(1, 3), (5, 5)]);
    }

    #[test]
    fn test_join_chunked() {
        fn chunk(typ: ChunkType, lines: Vec<&str>) -> Chunk {
            Chunk {
                typ,
                lines,
                indent: 0,
            }
        }

        let chunks = vec![
            chunk(ChunkType::SingleLine, vec![]),
            chunk(ChunkType::Head, vec!["a"]),
            chunk(ChunkType::Tail, vec!["b"]),
            chunk(ChunkType::Tail, vec!["c"]),
            chunk(ChunkType::SingleLine, vec![]),
            chunk(ChunkType::Head, vec!["d", "e"]),
            chunk(ChunkType::Tail, vec!["f"]),
            chunk(ChunkType::Tail, vec!["g"]),
        ];

        let chunks = join_chunked(chunks);

        assert_eq!(
            chunks,
            vec![
                chunk(ChunkType::SingleLine, vec![]),
                chunk(ChunkType::Head, vec!["a", "b", "c"]),
                chunk(ChunkType::SingleLine, vec![]),
                chunk(ChunkType::Head, vec!["d", "e", "f", "g"]),
            ],
        );
    }

    #[test]
    fn test_indentsort() {
        fn load(filename: &str) -> String {
            std::fs::read_to_string(format!(
                "{}/test_assets/{}",
                std::env::var("CARGO_MANIFEST_DIR").unwrap(),
                filename
            ))
            .unwrap()
        }

        assert_eq!(indentsort(Options::Default, "").join("\n"), "");
        assert_eq!(
            indentsort(Options::Default, "abc 123").join("\n"),
            "abc 123"
        );

        assert_eq!(
            indentsort(Options::Default, &load("c-like.in.txt")).join("\n"),
            load("c-like.default.out.txt").trim()
        );

        assert_eq!(
            indentsort(Options::Default, &load("c-like-square.in.txt")).join("\n"),
            load("c-like-square.default.out.txt").trim()
        );

        assert_eq!(
            indentsort(
                Options::LeaveSquareBracketedUnsorted,
                &load("c-like-square.in.txt")
            )
            .join("\n"),
            load("c-like-square.nosortsquare.out.txt").trim()
        );

        assert_eq!(
            indentsort(Options::NoClosingLine, &load("python-like.in.txt")).join("\n"),
            load("python-like.noclosing.out.txt").trim()
        );
    }
}
