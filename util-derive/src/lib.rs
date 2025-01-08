#![warn(clippy::nursery)]
#![allow(clippy::option_if_let_else)]

use check_keyword::CheckKeyword;
use convert_case::{Case, Casing};
use darling::{
    ast::{Data, Fields, Style},
    util::Ignored,
    FromDeriveInput, FromField, FromVariant,
};
use proc_macro::TokenStream;
use proc_macro2::{Ident, Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{DeriveInput, Generics, Type};

#[derive(Debug, FromField)]
#[darling(attributes(rc_term))]
struct RcTermField {
    ident: Option<Ident>,
    ty: Type,
    name: Option<String>,
    #[darling(default)]
    into: bool,
}

#[derive(Debug, FromVariant)]
struct RcTermVariant {
    ident: Ident,
    fields: Fields<RcTermField>,
}

#[derive(Debug, FromDeriveInput)]
#[darling(supports(enum_any))]
struct RcTermDeriveInput {
    ident: Ident,
    generics: Generics,
    data: Data<RcTermVariant, Ignored>,
}

impl RcTermDeriveInput {
    fn derive_rc_term(&self) -> TokenStream2 {
        let ident = &self.ident;
        let (impl_generics, ty_generics, where_clause) = self.generics.split_for_impl();
        let variants = self.data.as_ref().take_enum().unwrap();

        let variant_constructors = variants.iter().map(|variant| {
            let variant_ident = &variant.ident;

            let mut fn_name = variant_ident.to_string().to_case(Case::Snake);
            if fn_name.is_keyword() {
                fn_name.push('_');
            }

            let fn_ident = Ident::new(&fn_name, variant_ident.span());

            let fields = variant.fields.iter().enumerate().map(|(i, field)| {
                let arg_ident = if let Some(name) = &field.name {
                    Ident::new(name, Span::call_site())
                } else if let Some(ident) = &field.ident {
                    ident.clone()
                } else {
                    Ident::new(&format!("arg{i}"), Span::call_site())
                };

                let ty = &field.ty;

                if field.into {
                    let arg = quote! { #arg_ident: impl ::std::convert::Into<#ty> };
                    let field_value = quote! { #arg_ident.into() };
                    (arg, &field.ident, field_value)
                } else {
                    let arg = quote! { #arg_ident: #ty };
                    let field_value = quote! { #arg_ident };
                    (arg, &field.ident, field_value)
                }
            });

            let fn_args = fields.clone().map(|(arg, _, _)| arg);

            let fn_body = match variant.fields.style {
                Style::Unit => {
                    quote! { ::std::rc::Rc::new(Self::#variant_ident) }
                }
                Style::Tuple => {
                    let fields = fields.map(|(_, _, field_value)| field_value);
                    quote! { ::std::rc::Rc::new(Self::#variant_ident(#(#fields),*)) }
                }
                Style::Struct => {
                    let fields = fields.clone().map(|(_, field_ident, field_value)| {
                        let field_ident = field_ident.as_ref().unwrap();
                        quote! { #field_ident: #field_value }
                    });
                    quote! { ::std::rc::Rc::new(Self::#variant_ident { #(#fields),* }) }
                }
            };

            quote! {
                pub fn #fn_ident(#(#fn_args),*) -> ::std::rc::Rc<Self> {
                    #fn_body
                }
            }
        });

        quote! {
            impl #impl_generics #ident #ty_generics #where_clause {
                #(#variant_constructors)*
            }
        }
    }
}

/// For each variant in the enum, generate a constructor function that wraps the variant in an `Rc`.
#[proc_macro_derive(RcTerm, attributes(rc_term))]
pub fn rc_term_derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse_macro_input!(input as DeriveInput);
    let input = RcTermDeriveInput::from_derive_input(&ast).unwrap();
    input.derive_rc_term().into()
}
