// Copyright 2012-2013 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::collections::{HashMap, hash_map};

use ast::{MetaItem, Item, Expr};
use ast;
use codemap::Span;
use ext::base::ExtCtxt;
use ext::build::AstBuilder;
use ext::deriving::generic::*;
use ext::deriving::generic::ty::*;
use parse::token::{self, InternedString};
use ptr::P;

pub fn expand_deriving_from<F>(cx: &mut ExtCtxt,
                               span: Span,
                               mitem: &MetaItem,
                               item: &Item,
                               mut push: F) where
    F: FnMut(P<Item>),
{
    let mut seen = HashMap::new();

    let mitems = match mitem.node {
        ast::MetaList(_, ref mitems) => mitems,
        _ => {
            cx.span_err(span, "expected list of type identifiers");
            return
        }
    };

    for mty in mitems {
        let type_ = match mty.node {
            ast::MetaWord(ref type_) => type_,
            _ => {
                cx.span_err(mty.span, "expected type identifier");
                continue
            }
        };

        let valid = match type_.char_at(0) {
            'u' | 'i' => match &type_[1..] {
                "8" | "16" | "32" | "64" => true,
                _ => false
            },
            'f' => match &type_[1..] {
                "32" | "64" => true,
                _ => false
            },
            _ => false,
        };
        if !valid {
            cx.span_err(mty.span, "unknown type; `derive(From(...))` \
                                   only accepts primitive numeric types");
            continue
        }

        // ensure uniqueness: we can give more helpful error messages
        // here than the ones that will appear from creating multiple
        // impls.
        match seen.entry(type_) {
            hash_map::Entry::Vacant(v) => { v.insert(mty.span); }
            hash_map::Entry::Occupied(o) => {
                cx.span_err(mty.span, &format!("type `{}` specified multiple times", type_));
                cx.span_note(*o.get(), "first occurrence");
                continue
            }
        }

        seen.insert(type_, mty.span);
        let trait_def = from_trait_def(cx, span, &type_);
        trait_def.expand(cx, mty, item, |it| push(it))
    }
}

pub fn from_trait_def<'t>(cx: &mut ExtCtxt,
                          span: Span,
                          input_type: &'t str) -> TraitDef<'t> {
    let inline = cx.meta_word(span, InternedString::new("inline"));
    let attrs = vec!(cx.attribute(span, inline));
    TraitDef {
        span: span,
        attributes: Vec::new(),
        path: path_std!(cx, core::convert::From),
        additional_bounds: Vec::new(),
        generics: LifetimeBounds::empty(),
        methods: vec![
            MethodDef {
                name: "from",
                generics: LifetimeBounds::empty(),
                explicit_self: None,
                args: vec!(Literal(Path::new_local(input_type))),
                ret_ty: Self_,
                // #[inline] liable to cause code-bloat
                attributes: attrs.clone(),
                combine_substructure: combine_substructure(Box::new(move |c, s, sub| {
                    cs_from(input_type, c, s, sub)
                })),
            }
            ],
        associated_types: Vec::new(),
    }
}

fn cs_from(name: &str, cx: &mut ExtCtxt, trait_span: Span, substr: &Substructure) -> P<Expr> {
    let n = match substr.nonself_args {
        [ref n] => n,
        _ => cx.span_bug(trait_span, "incorrect number of arguments in `derive(From(...))`")
    };

    match *substr.fields {
        StaticStruct(..) => {
            cx.span_err(trait_span, "`From<...>` cannot be derived for structs");
            return cx.expr_fail(trait_span, InternedString::new(""));
        }
        StaticEnum(enum_def, _) => {
            if enum_def.variants.is_empty() {
                cx.span_err(trait_span,
                            "`From<...>` cannot be derived for enums with no variants");
                return cx.expr_fail(trait_span, InternedString::new(""));
            }

            let mut arms = Vec::new();

            for variant in &enum_def.variants {
                match variant.node.kind {
                    ast::TupleVariantKind(ref args) => {
                        if !args.is_empty() {
                            cx.span_err(trait_span,
                                        "`From<...>` cannot be derived for \
                                        enum variants with arguments");
                            return cx.expr_fail(trait_span,
                                                InternedString::new(""));
                        }
                        let span = variant.span;

                        // expr for `$n == $variant as $name`
                        let path = cx.path(span, vec![substr.type_ident, variant.node.name]);
                        let variant = cx.expr_path(path);
                        let ty = cx.ty_ident(span, cx.ident_of(name));
                        let cast = cx.expr_cast(span, variant.clone(), ty);
                        let guard = cx.expr_binary(span, ast::BiEq, n.clone(), cast);

                        // expr for `$variant`
                        let body = variant;

                        // arm for `_ if $guard => $body`
                        let arm = ast::Arm {
                            attrs: vec!(),
                            pats: vec!(cx.pat_wild(span)),
                            guard: Some(guard),
                            body: body,
                        };

                        arms.push(arm);
                    }
                    ast::StructVariantKind(_) => {
                        cx.span_err(trait_span,
                                    "`From<...>` cannot be derived for enums \
                                    with struct variants");
                        return cx.expr_fail(trait_span,
                                            InternedString::new(""));
                    }
                }
            }

            // arm for `_ => panic!("...")`
            let arm = ast::Arm {
                attrs: vec!(),
                pats: vec!(cx.pat_wild(trait_span)),
                guard: None,
                body: cx.expr_fail(trait_span,
                                      token::intern_and_get_ident("invalid value in From")),
            };
            arms.push(arm);

            cx.expr_match(trait_span, n.clone(), arms)
        }
        _ => cx.span_bug(trait_span, "expected StaticEnum in derive(From(...))")
    }
}
