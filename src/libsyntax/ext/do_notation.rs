// Copyright 2013 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

/*!
Do-notation

Allows any type with a .chain method to be used like a monad in
Haskell's do notation, e.g.

~~~
fn main() {
    let x = do_!(bind a = Some(1);
                 bind b = None;
                 let res = a + b;
                 Some(res));
    println(fmt!("%?", x));
}
~~~

Very similar to Haskell, except `... <- ...` is `bind ... = ...`.
*/

use ast;
use codemap::span;
use ext::base;
use ext::base::*;
use parse;
use parse::token;
use ext::build::AstBuilder;

enum Item {
    Bind(ast::ident, @ast::expr),
    Let(ast::ident, @ast::expr),
    Expr(@ast::expr)
}

pub fn expand_do(cx: @ExtCtxt, sp: span, tts: &[ast::token_tree]) -> base::MacResult {
    let p = parse::new_parser_from_tts(cx.parse_sess(),
                                       cx.cfg(),
                                       tts.to_owned());
    let bind_id = cx.ident_of("bind");
    let let_id = cx.ident_of("let");
    let mut legal_eof = false;

    let items = do vec::build |push| {
        loop {
            match *p.token {
                token::EOF => {
                    if legal_eof {
                        break;
                    } else {
                        cx.span_fatal(sp, "Last item to do_! should be an expression")
                    }
                }
                token::IDENT(id, false) if id == bind_id => {
                    p.bump();
                    let id = p.parse_ident();

                    p.expect(&token::EQ);
                    push(Bind(id, p.parse_expr()));
                    legal_eof = false;
                    p.expect(&token::SEMI);
                }
                token::IDENT(id, false) if id == let_id => {
                    p.bump();
                    let id = p.parse_ident();

                    p.expect(&token::EQ);
                    push(Let(id, p.parse_expr()));
                    legal_eof = false;
                    p.expect(&token::SEMI);
                }
                _ => {
                    push(Expr(p.parse_expr()));
                    p.eat(&token::SEMI); // just ignore semicolons entirely
                    legal_eof = true;
                }
            }
        }
    };

    let expr = build(cx, sp, items);
    MRExpr(expr)
}

fn build(cx: @ExtCtxt, sp: span, items: &[Item]) -> @ast::expr {
    let chain_id = cx.ident_of("chain");
    match items {
        [.. rest, Expr(e)] => {
            let mut expr = e;
            for rest.each_reverse |it| {
                expr = match *it {
                    Expr(e) => {
                        // { e; expr }
                        cx.expr_blk(cx.blk(sp,
                                           ~[cx.stmt_expr(e)],
                                           Some(expr)))
                    }
                    Let(id, e) => {
                        // { let id = e; expr }
                        cx.expr_blk(cx.blk(sp,
                                           ~[cx.stmt_let(sp, false, id, e)],
                                           Some(expr)))
                    }
                    Bind(id, to_bind) => {
                        // to_bind.chain(|id| expr)
                        let lambda = cx.lambda_expr_1(sp, expr, id);
                        cx.expr_method_call(sp, to_bind, chain_id, ~[lambda])
                    }
                };
            }
            expr
        }
        _ => cx.span_bug(sp, "A do_! notation without a trailing Expr got through")
    }
}
