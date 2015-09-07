// Copyright 2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![feature(rustc_private)]
#![feature(custom_attribute)]
#![allow(unused_attributes)]


#[macro_use]
extern crate log;

extern crate rustc;
extern crate rustc_driver;
extern crate syntax;


use rustc::session::Session;
use rustc_driver::{driver, CompilerCalls, Compilation};

use syntax::ast::{self, NodeId, Ident, Name};
use syntax::codemap::{CodeMap, Span};
use syntax::visit;

pub struct Visitor<'a> {
    codemap: &'a CodeMap,
}

impl<'a> Visitor<'a> {
    fn print_span(&self, span: Span) {
        println!("`{}`", self.codemap.span_to_snippet(span).unwrap());
    }
}

impl<'a, 'v> visit::Visitor<'v> for Visitor<'a> {
    fn visit_mac(&mut self, mac: &'v ast::Mac) {
        visit::walk_mac(self, mac)
    }

    fn visit_name(&mut self, span: Span, _name: Name) {
        self.print_span(span);
        // Nothing to do.
    }
    fn visit_ident(&mut self, span: Span, ident: Ident) {
        self.visit_name(span, ident.name);
    }
    fn visit_mod(&mut self, m: &'v ast::Mod, s: Span, _n: NodeId) {
        self.print_span(s);
        visit::walk_mod(self, m)
    }
    fn visit_foreign_item(&mut self, i: &'v ast::ForeignItem) {
        self.print_span(i.span);
        visit::walk_foreign_item(self, i)
    }
    fn visit_item(&mut self, i: &'v ast::Item) {
        self.print_span(i.span);
        visit::walk_item(self, i)
    }
    fn visit_local(&mut self, l: &'v ast::Local) {
        self.print_span(l.span);
        visit::walk_local(self, l)
    }
    fn visit_block(&mut self, b: &'v ast::Block) {
        self.print_span(b.span);
        visit::walk_block(self, b)
    }
    fn visit_stmt(&mut self, s: &'v ast::Stmt) {
        self.print_span(s.span);
        visit::walk_stmt(self, s)
    }
    fn visit_arm(&mut self, a: &'v ast::Arm) {
        visit::walk_arm(self, a)
    }
    fn visit_pat(&mut self, p: &'v ast::Pat) {
        self.print_span(p.span);
        visit::walk_pat(self, p)
    }
    fn visit_decl(&mut self, d: &'v ast::Decl) {
        self.print_span(d.span);
        visit::walk_decl(self, d)
    }
    fn visit_expr(&mut self, ex: &'v ast::Expr) {
        self.print_span(ex.span);
        visit::walk_expr(self, ex)
    }
    fn visit_ty(&mut self, t: &'v ast::Ty) {
        self.print_span(t.span);
        visit::walk_ty(self, t)
    }
    fn visit_generics(&mut self, g: &'v ast::Generics) {
        visit::walk_generics(self, g)
    }
    fn visit_fn(&mut self, fk: visit::FnKind<'v>, fd: &'v ast::FnDecl, b: &'v ast::Block, s: Span, _: NodeId) {
        self.print_span(s);
        visit::walk_fn(self, fk, fd, b, s)
    }
    fn visit_trait_item(&mut self, ti: &'v ast::TraitItem) {
        self.print_span(ti.span);
        visit::walk_trait_item(self, ti)
    }
    fn visit_impl_item(&mut self, ii: &'v ast::ImplItem) {
        self.print_span(ii.span);
        visit::walk_impl_item(self, ii)
    }
    fn visit_trait_ref(&mut self, t: &'v ast::TraitRef) {
        visit::walk_trait_ref(self, t)
    }
    fn visit_ty_param_bound(&mut self, bounds: &'v ast::TyParamBound) {
        visit::walk_ty_param_bound(self, bounds)
    }
    fn visit_poly_trait_ref(&mut self, t: &'v ast::PolyTraitRef, m: &'v ast::TraitBoundModifier) {
        self.print_span(t.span);
        visit::walk_poly_trait_ref(self, t, m)
    }
    fn visit_struct_def(&mut self, s: &'v ast::StructDef, _: Ident, _g: &'v ast::Generics, _: NodeId) {
        visit::walk_struct_def(self, s)
    }
    fn visit_struct_field(&mut self, s: &'v ast::StructField) {
        self.print_span(s.span);
        visit::walk_struct_field(self, s)
    }
    fn visit_enum_def(&mut self, enum_definition: &'v ast::EnumDef,
                      generics: &'v ast::Generics) {
        visit::walk_enum_def(self, enum_definition, generics)
    }

    fn visit_variant(&mut self, v: &'v ast::Variant, g: &'v ast::Generics) {
        self.print_span(v.span);
        visit::walk_variant(self, v, g)
    }

    fn visit_opt_lifetime_ref(&mut self,
                              span: Span,
                              opt_lifetime: &'v Option<ast::Lifetime>) {
        self.print_span(span);
        match *opt_lifetime {
            Some(ref l) => self.visit_lifetime_ref(l),
            None => ()
        }
    }
    fn visit_lifetime_bound(&mut self, lifetime: &'v ast::Lifetime) {
        self.print_span(lifetime.span);
        visit::walk_lifetime_bound(self, lifetime)
    }
    fn visit_lifetime_ref(&mut self, lifetime: &'v ast::Lifetime) {
        self.print_span(lifetime.span);
        visit::walk_lifetime_ref(self, lifetime)
    }
    fn visit_lifetime_def(&mut self, lifetime: &'v ast::LifetimeDef) {
        visit::walk_lifetime_def(self, lifetime)
    }
    fn visit_explicit_self(&mut self, es: &'v ast::ExplicitSelf) {
        self.print_span(es.span);
        visit::walk_explicit_self(self, es)
    }
    fn visit_path(&mut self, path: &'v ast::Path, _id: NodeId) {
        self.print_span(path.span);
        visit::walk_path(self, path)
    }
    fn visit_path_segment(&mut self, path_span: Span, path_segment: &'v ast::PathSegment) {
        self.print_span(path_span);
        visit::walk_path_segment(self, path_span, path_segment)
    }
    fn visit_path_parameters(&mut self, path_span: Span, path_parameters: &'v ast::PathParameters) {
        self.print_span(path_span);
        visit::walk_path_parameters(self, path_span, path_parameters)
    }
    fn visit_assoc_type_binding(&mut self, type_binding: &'v ast::TypeBinding) {
        self.print_span(type_binding.span);
        visit::walk_assoc_type_binding(self, type_binding)
    }
    fn visit_attribute(&mut self, attr: &'v ast::Attribute) {
        self.print_span(attr.span);
    }
}

struct SpansCalls;

impl<'a> CompilerCalls<'a> for SpansCalls {
    fn build_controller(&mut self, _: &Session) -> driver::CompileController<'a> {
        let mut control = driver::CompileController::basic();
        control.after_parse.stop = Compilation::Stop;
        control.after_parse.callback = Box::new(move |state| {
            let krate = state.krate.unwrap();
            let codemap = state.session.codemap();

            let mut v = Visitor { codemap: codemap };
            visit::walk_crate(&mut v, krate);
        });

        control
    }
}

pub fn main() {
    let args: Vec<_> = std::env::args().collect();
    let mut call_ctxt = SpansCalls;
    rustc_driver::run_compiler(&args, &mut call_ctxt);
}
