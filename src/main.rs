use syntect::{highlighting::ThemeSet, html::{ClassStyle, css_for_theme_with_class_style}, parsing::{BasicScopeStackOp, ParseState, Scope, ScopeStack, SyntaxSet, SCOPE_REPO}, util::LinesWithEndings};

#[derive(Debug, Clone)]
struct Span<'a> {
    offset: usize,
    text: &'a str,
    class: String,
}

#[derive(Debug, Clone)]
struct HighlightOp {
    offset: usize,
    kind: HighlightOpKind,
}

#[derive(Debug, Clone)]
enum HighlightOpKind {
    Push { class: String },
    Pop,
}

fn main() {
    let code = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/src/main.rs"));

    const INPUT: &str = "use std::path::Path as StdPath;\n";

    let stack = highlight_line(INPUT);
    let spans = diff("what core:path::Path ad_StdPatj", INPUT);

    println!("HIGHLIGHTS:\n{:#?}", stack);
    println!("DIFF:\n{:#?}", spans);

    let mut merged_stack = Vec::new();

    let mut last_index = 0;
    for (insert_points, span) in spans.into_iter().map(|span| (find_insert_points(&stack, &span), span)) {
        for insert_point in insert_points {
            for i in last_index..insert_point.index {
                merged_stack.push(stack[i].clone());
                last_index += 1;
            }
            merged_stack.push(HighlightOp {
                offset: insert_point.push_offset,
                kind: HighlightOpKind::Push {
                    class: span.class.clone(),
                }
            });
            merged_stack.push(HighlightOp {
                offset: insert_point.pop_offset,
                kind: HighlightOpKind::Pop,
            });
        }
    }

    println!("MERGED:\n{:#?}", merged_stack);

    let mut html = String::new();
    let mut last_char = 0;
    for op in merged_stack {
        if op.offset > last_char {
            html.push_str(&Escape(&INPUT[last_char..op.offset]).to_string());
            last_char = op.offset;
        }

        match op.kind {
            HighlightOpKind::Push { class } => html.push_str(&format!("<span class=\"{}\">", class)),
            HighlightOpKind::Pop => html.push_str("</span>")
        }
    }
    html.push_str(&Escape(&INPUT[last_char..]).to_string());

    let theme_set = ThemeSet::load_defaults();
    let theme = &theme_set.themes["base16-ocean.dark"];

    let css = css_for_theme_with_class_style(theme, ClassStyle::Spaced);

    std::fs::write("output.html", format!("
        <style>
            .diff-insert {{
                background-color: #24bd82;
            }}

            .diff-delete {{
                background-color: #bd244a;
            }}
            {}
        </style>
        <pre>
        {}
        </pre>
    ", css, html)).unwrap();
}

#[derive(Debug, Clone)]
pub struct InsertPoint {
    index: usize,
    push_offset: usize,
    pop_offset: usize,
}

fn find_insert_points(stack: &[HighlightOp], span: &Span<'_>) -> Vec<InsertPoint> {
    let mut insert_points = Vec::new();

    let mut span = span.clone();
    for i in 0..stack.len()-1 {
        let op = &stack[i];
        // Find a slot in `stack` such that it would stay sorted if `span` was inserted there.
        // That is, find a slot where `stack[i].offset <= span.offset <= stack[i].offset`.
        if op.offset <= span.offset {
            if let Some(peek_op) = stack.get(i+1) {
                if peek_op.offset >= span.offset {
                    let available_space = peek_op.offset - span.offset;

                    // If it was found, mark this slot as an `InsertPoint`
                    insert_points.push(InsertPoint {
                        index: i+1,
                        push_offset: span.offset,
                        pop_offset: span.offset + span.text.len().min(available_space),
                    });

                    if available_space < span.text.len() {
                        // If `span` doesn't fit in this slot, then split `span` and
                        // find a slot for the next part.
                        span.text = &span.text[peek_op.offset - span.offset..];
                        span.offset = peek_op.offset
                    } else {
                        // If it fits, we're done.
                        return insert_points;
                    }

                }
            } else {
                // If we're in the final slot, then we put the `InsertPoint` here
                insert_points.push(InsertPoint {
                    index: i+1,
                    push_offset: span.offset,
                    pop_offset: span.offset+span.text.len(),
                })
            }
        }
    }

    insert_points
}

fn highlight_line<'a>(line: &'a str) -> Vec<HighlightOp> {
    let syntax_set = SyntaxSet::load_defaults_newlines();
    let syntax_ref = syntax_set.find_syntax_by_token("rs").unwrap();

    let mut parse_state = ParseState::new(syntax_ref);
    let mut scope_stack = ScopeStack::new();
    let mut output = Vec::new();

    let ops = parse_state.parse_line(line, &syntax_set);

    for (i, op) in ops {
        scope_stack.apply_with_hook(&op, |basic_op, _| match basic_op {
            BasicScopeStackOp::Push(scope) => {
                let mut class = String::new();
                scope_to_classes(&mut class, scope, ClassStyle::Spaced);
                output.push(HighlightOp {
                    offset: i,
                    kind: HighlightOpKind::Push { class },
                });
            }
            BasicScopeStackOp::Pop => output.push(HighlightOp {
                offset: i,
                kind: HighlightOpKind::Pop,
            }),
        });
    }

    output
}

fn scope_to_classes(s: &mut String, scope: Scope, style: ClassStyle) {
    let repo = SCOPE_REPO.lock().unwrap();
    for i in 0..(scope.len()) {
        let atom = scope.atom_at(i as usize);
        let atom_s = repo.atom_str(atom);
        if i != 0 {
            s.push_str(" ")
        }
        match style {
            ClassStyle::SpacedPrefixed { prefix } => {
                s.push_str(&prefix);
            }
            _ => {}
        }
        s.push_str(atom_s);
    }
}

// Copyright 2013 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::fmt;

/// Wrapper struct which will emit the HTML-escaped version of the contained
/// string when passed to a format string.
pub struct Escape<'a>(pub &'a str);

impl<'a> fmt::Display for Escape<'a> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Because the internet is always right, turns out there's not that many
        // characters to escape: http://stackoverflow.com/questions/7381974
        let Escape(s) = *self;
        let pile_o_bits = s;
        let mut last = 0;
        for (i, ch) in s.bytes().enumerate() {
            match ch as char {
                '<' | '>' | '&' | '\'' | '"' => {
                    fmt.write_str(&pile_o_bits[last..i])?;
                    let s = match ch as char {
                        '>' => "&gt;",
                        '<' => "&lt;",
                        '&' => "&amp;",
                        '\'' => "&#39;",
                        '"' => "&quot;",
                        _ => unreachable!(),
                    };
                    fmt.write_str(s)?;
                    last = i + 1;
                }
                _ => {}
            }
        }

        if last < s.len() {
            fmt.write_str(&pile_o_bits[last..])?;
        }
        Ok(())
    }
}

fn diff<'a>(old: &'a str, new: &'a str) -> Vec<Span<'a>> {
    dissimilar::diff(old, new)
        .into_iter()
        .filter(|c| !matches!(c, dissimilar::Chunk::Delete(_)))
        .scan(0, |offset, change| {
            let (text, class) = match change {
                dissimilar::Chunk::Equal(t) => (t, "diff-equal".into()),
                dissimilar::Chunk::Insert(t) => (t, "diff-insert".into()),
                dissimilar::Chunk::Delete(_) => unreachable!(),
            };

            let span = Some(Span {
                offset: *offset,
                text,
                class,
            });

            *offset += text.len();

            span
        })
        .collect()
}
