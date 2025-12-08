use std::{ffi::OsStr, fs, io, iter::Peekable, path::Path, str::Chars};

use proc_macro::TokenStream;
use quote::quote;

#[proc_macro]
pub fn anti_types(_: TokenStream) -> TokenStream {
    let files = find_type_files().unwrap();

    let mut out = Vec::new();
    out.push(quote! {});

    for file in files {
        let mut it = file.chars().peekable();
        let it = &mut it;
        loop {
            match it.next() {
                Some('s') => {
                    assert_eq!(it.take(5).collect::<String>(), String::from("truct"));
                    let name = parse_name(it);
                    skip_whitespace(it);
                    assert_eq!(it.next(), Some('{'));
                    assert_eq!(it.next(), Some('}'));

                    let name: proc_macro2::TokenStream = syn::parse_str(&name).unwrap();
                    out.push(quote! {
                        pub(mod) struct #name {

                        }
                    })
                }
                Some('u') => {
                    assert_eq!(it.take(4).collect::<String>(), String::from("nion"));
                    let name = parse_name(it);
                    let name: proc_macro2::TokenStream = syn::parse_str(&name).unwrap();
                    skip_whitespace(it);
                    assert_eq!(it.next(), Some('{'));
                    let cases = parse_cases(it)
                        .iter()
                        .map(|c| match c {
                            Case {
                                ty: Some(ty),
                                label: Some(label),
                            } => {
                                let mut label = label.clone();
                                upper(&mut label);
                                format!("{}({ty})", label)
                            }
                            Case {
                                ty: None,
                                label: Some(label),
                            } => {
                                let mut label = label.clone();
                                upper(&mut label);
                                label
                            }
                            Case {
                                ty: Some(ty),
                                label: None,
                            } => format!("{ty}({ty})"),
                            Case {
                                ty: None,
                                label: None,
                            } => unreachable!(),
                        })
                        .collect::<Vec<_>>()
                        .join(",\n");
                    assert_eq!(it.next(), Some('}'));

                    let cases: proc_macro2::TokenStream = syn::parse_str(&cases).unwrap();
                    out.push(quote! {
                        pub(crate) enum #name {
                            #cases
                        }
                    })
                }
                Some(ch) if ch.is_whitespace() => continue,
                Some(ch) => panic!("Unexpected character {ch}"),
                None => break,
            }
        }
    }

    out.into_iter().map(|tt| TokenStream::from(tt)).collect()
}

fn upper(s: &mut str) {
    s.get_mut(0..1).unwrap().make_ascii_uppercase();
}

fn skip_whitespace(chars: &mut Peekable<Chars<'_>>) {
    while chars.peek().is_some_and(|ch| ch.is_whitespace()) {
        _ = chars.next()
    }
}

fn parse_name(it: &mut Peekable<Chars<'_>>) -> String {
    let mut name = String::new();
    loop {
        match it.peek() {
            Some(ch) if ch.is_ascii_alphanumeric() => {
                name.push(it.next().unwrap());
            }
            Some(ch) if ch.is_whitespace() => _ = it.next(),
            Some(_) => break,
            None => break,
        }
    }

    name
}

struct Case {
    label: Option<String>,
    ty: Option<String>,
}

fn parse_cases(it: &mut Peekable<Chars<'_>>) -> Vec<Case> {
    let mut cases = Vec::new();
    loop {
        match it.peek() {
            Some('A'..='Z') => cases.push(Case {
                ty: Some(parse_name(it)),
                label: None,
            }),
            Some('a'..='z') => {
                let label: Option<String> = Some(parse_name(it));
                skip_whitespace(it);
                let ty = if it.peek().is_some_and(|&ch| ch == ':') {
                    it.next();
                    skip_whitespace(it);
                    Some(parse_name(it))
                } else {
                    None
                };
                cases.push(Case { label, ty })
            }
            Some('}') => break,
            Some(ch) if ch.is_whitespace() => _ = it.next(),
            // TODO: figure out why this triggers on the label separating ':'
            Some(ch) => panic!("Unexpected char: {ch}"),
            None => break,
        }
    }

    cases
}

fn find_type_files() -> io::Result<Vec<String>> {
    let mut files = Vec::new();

    add_dir_files(&mut files, ".")?;

    Ok(files)
}

fn add_dir_files<P>(files: &mut Vec<String>, path: P) -> io::Result<()>
where
    P: AsRef<Path>,
{
    for entry in fs::read_dir(path)? {
        let path = entry?.path();
        if path.is_dir() {
            add_dir_files(files, path)?;
        } else {
            if path.extension() != Some(OsStr::new("anti")) {
                continue;
            }
            let content = fs::read_to_string(path)?;
            files.push(content);
        }
    }

    Ok(())
}
