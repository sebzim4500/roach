use openapiv3::{OpenAPI, ReferenceOr, Schema, SchemaKind, Type, ObjectType, Components, Paths, PathItem, Operation, RequestBody, MediaType};
use proc_macro2::{TokenStream, Ident};
use quote::{quote, ToTokens, format_ident, TokenStreamExt};
use inflector::cases::snakecase::to_snake_case;
use inflector::cases::pascalcase::to_pascal_case;
use crate::spec::{Spec, TypeDefinition, TypeKind, ScalarType, Property, Variant, Endpoint};
//
//trait ToTokens {
//    fn to_tokens(&self, tokens: &mut TokenStream);
//}

impl ToTokens for Spec {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let type_definitions = &self.type_definitions;
        let endpoints = &self.endpoints;

        (quote! {
            use serde::{Serialize, Deserialize};
            use tower_service::Service;

            #[derive(Clone)]
            pub struct Client<S: Service<hyper::Request<hyper::Body>>> {
                service: S,
                base_path: String,
            }

            #(#type_definitions)*

            impl<S> Client<S> where S: Service<hyper::Request<hyper::Body>, Response = hyper::Response<hyper::Body>> {
                pub fn new(service: S, base_path: String) -> Self {
                    Self { service, base_path }
                }

                #(#endpoints)*
            }

        }).to_tokens(tokens)
    }
}

impl ToTokens for TypeDefinition {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = format_ident!("{}", self.name);
        let result = match &self.kind {
            TypeKind::Alias(type_name) => quote! { type #ident = #type_name; },
            TypeKind::Object { properties } => quote! {
                #[derive(Serialize, Deserialize)]
                pub struct #ident {
                    #(#properties),*
                }
            },
            TypeKind::Enum { variants } => quote! {
                #[derive(Serialize, Deserialize)]
                pub enum #ident {
                    #(#variants),*
                }
            },
        };
        result.to_tokens(tokens);
    }
}

impl ToTokens for ScalarType {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        (match self {
            ScalarType::String => quote! { String },
            ScalarType::Number => quote! { f64 },
        }).to_tokens(tokens);
    }
}

impl ToTokens for Property {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let original_name = &self.original_name;
        let property_ident = format_ident!("{}", self.name);
        let property_type_ident = format_ident!("{}", self.type_name);
        let property_type = if self.required {
            quote! { #property_type_ident }
        } else {
            quote! { Option<#property_type_ident> }
        };
        let result = if original_name == &self.name {
            quote! { pub #property_ident: #property_type }
        } else {
            quote! {
                #[serde(rename = #original_name)]
                pub #property_ident: #property_type
            }
        };
        result.to_tokens(tokens);
    }
}

impl ToTokens for Variant {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let original_name = &self.original_name;
        let ident = format_ident!("{}", self.name);

        let result = if original_name == &self.name {
            quote! { #ident }
        } else {
            quote! { #[serde(rename = #original_name)] #ident }
        };
        result.to_tokens(tokens);
    }
}

impl ToTokens for Endpoint {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = format_ident!("{}", self.name);

        let body = self.request_body.as_ref().map(|request_body| {
            let request_body_type = format_ident!("{}", request_body.type_name);
            if request_body.required {
                quote! { body: &#request_body_type }
            } else {
                quote! { body: Option<&#request_body_type> }
            }
        }).into_iter();

        let parameters = self.parameters.iter().map(|parameter| {
            let ident = format_ident!("{}", parameter.name);
            let type_ident = format_ident!("{}", parameter.type_name);
            if parameter.required {
                quote! { #ident: &#type_ident }
            } else {
                quote! { #ident: Option<&#type_ident> }
            }
        });

        let request_initializer = if let Some(request_body) = &self.request_body {
            quote! { let mut request = hyper::Request::new(hyper::Body::from(serde_json::to_vec(body).unwrap())); }
        } else {
            quote! { let mut request = hyper::Request::new(hyper::Body::empty()); }
        };

        let result = quote! {
            pub async fn #ident(&mut self, #(#parameters,)* #(#body),*) {
                #request_initializer

            }
        };
        result.to_tokens(tokens);
    }
}