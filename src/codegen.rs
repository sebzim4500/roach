use openapiv3::{OpenAPI, ReferenceOr, Schema, SchemaKind, Type, ObjectType, Components, Paths, PathItem, Operation, RequestBody};
use proc_macro2::{TokenStream, Ident};
use quote::{quote, ToTokens, format_ident};
use inflector::cases::snakecase::to_snake_case;
use inflector::cases::pascalcase::to_pascal_case;

pub fn generate_spec(spec: OpenAPI, tokens: &mut TokenStream) {
    (quote! {
        use serde::{Serialize, Deserialize};
        use tower_service::Service;

        #[derive(Clone)]
        pub struct Client<S: Service<hyper::Request<Vec<u8>>>> {
            service: S,
            base_path: String,
        }
    }).to_tokens(tokens);
    if let Some(components) = spec.components {
        generate_components(components, tokens);
    }
    generate_paths(spec.paths, tokens);
}

pub fn generate_components(components: Components, tokens: &mut TokenStream) {
    for (name, schema_or_ref) in components.schemas {
        if let ReferenceOr::Item(schema) = schema_or_ref {
            match schema.schema_kind {
                SchemaKind::Type(Type::Object(object)) => {
                    generate_object(&name, object, tokens);
                }
                SchemaKind::Type(Type::String(string_type)) if !string_type.enumeration.is_empty() => {
                    generate_enum(&name, string_type.enumeration, tokens);
                }
                SchemaKind::Type(Type::String(string_type)) => {
                    let type_ident = format_ident!("{}", to_pascal_case(&name));
                    (quote! { type #type_ident = String; }).to_tokens(tokens);
                }
                SchemaKind::OneOf { one_of } => {
                    // TODO generate this
                }
                kind => unimplemented!("Unknown outer schema kind {:?}", kind)
            }
        }
    }
}

fn to_property_name(outer_name: &str, field_name: &str) -> String {
    match field_name.to_lowercase().as_str() {
        "type" => format!("{}_type", to_snake_case(outer_name)),
        _ => to_snake_case(field_name),
    }
}

const SCHEMA_PREFIX: &str = "#/components/schemas/";

fn type_from_reference(reference: &str) -> Ident {
    if reference.starts_with(SCHEMA_PREFIX) {
        format_ident!("{}", to_pascal_case(&reference[SCHEMA_PREFIX.len()..]))
    } else {
        unimplemented!("Weird reference {}", reference)
    }
}

pub fn generate_object(name: &str, object: ObjectType, tokens: &mut TokenStream) {
    let type_ident = format_ident!("{}", to_pascal_case(name));
    let properties = object.properties.into_iter().map(|(field_name, reference_or_properties)| {
        let property_name = to_property_name(name, &field_name);
        let property_ident = format_ident!("{}", property_name);
        let property_type = match reference_or_properties {
            ReferenceOr::Item(item) => {
                let x: Box<Schema> = item;
                match x.schema_kind {
                    SchemaKind::Type(Type::String(string_type)) => quote! { String },
                    SchemaKind::Type(Type::Number(number_type)) => quote! { f64 },
                    SchemaKind::Type(Type::Boolean {}) => quote! { bool },
                    y => unimplemented!("Unexpected schema kind {:?}", y)
                }

            },
            ReferenceOr::Reference { reference } => {
                let ident = type_from_reference(&reference);
                quote! { #ident }
            }
        };
        if property_name == field_name {
            quote! {
                pub #property_ident: #property_type
            }
        } else {
            quote! {
                #[serde(rename = #property_name)]
                pub #property_ident: #property_type
            }
        }
    });
    (quote! {
        #[derive(Serialize, Deserialize)]
        pub struct #type_ident {
            #(#properties),*
        }
    }).to_tokens(tokens);
}

fn generate_enum(name: &str, variant_names: Vec<String>, tokens: &mut TokenStream) {
    let type_ident = format_ident!("{}", to_pascal_case(name));
    let variants = variant_names.into_iter().map(|variant_name| {
        let variant = to_pascal_case(&variant_name);
        let variant_ident = format_ident!("{}", variant);
        if variant == variant_name {
            variant_ident.into_token_stream()
        } else {
            quote! {
                #[serde(rename = #variant_name)]
                #variant_ident
            }
        }
    });
    (quote! {
        #[derive(Serialize, Deserialize)]
        pub enum #type_ident {
            #(#variants),*
        }
    }).to_tokens(tokens);
}

#[derive(Debug, Clone, Copy)]
enum Method {
    Get,
    Put,
    Post,
    Delete,
    Options,
    Head,
    Patch,
    Trace,
}

fn get_methods_from_type(url: &str, path: PathItem) -> Vec<(String, Method, Operation)> {
    let mut results = Vec::new();
    results.extend(path.get.map(|o| (url.to_string(), Method::Get, o)));
    results.extend(path.put.map(|o| (url.to_string(), Method::Put, o)));
    results.extend(path.post.map(|o| (url.to_string(), Method::Post, o)));
    results.extend(path.delete.map(|o| (url.to_string(), Method::Delete, o)));
    results.extend(path.options.map(|o| (url.to_string(), Method::Options, o)));
    results.extend(path.head.map(|o| (url.to_string(), Method::Head, o)));
    results.extend(path.patch.map(|o| (url.to_string(), Method::Patch, o)));
    results.extend(path.trace.map(|o| (url.to_string(), Method::Trace, o)));

    results
}

fn generate_paths(paths: Paths, tokens: &mut TokenStream) {
    let functions = paths.into_iter()
        .flat_map(|(url, item)| {
            if let ReferenceOr::Item(item) = item {
                get_methods_from_type(&url, item)
            } else {
                Vec::new()
            }
        })
        .map(|(url, method, operation): (String, Method, Operation)| {
            let name = to_snake_case(&format!("{:?}_{}", method, url.trim_start_matches("/").replace("/", "_")));
            let ident = format_ident!("{}", name);

//            let parameters = operation.parameters.into_iter().filter_map(|parameter| {
//                None
//            });
            let body: std::option::IntoIter<TokenStream> = operation.request_body.and_then(|body| {
               match body {
                   ReferenceOr::Reference { reference } => {
                       let ident = type_from_reference(&reference);
                       Some(quote! { body: #ident })
                   },
                   ReferenceOr::Item(_) => unimplemented!(),
               }
            }).into_iter();
            quote! {
                async fn #ident(&mut self #(#body)*) {

                }
            }
        });
    (quote! {
        impl<S> Client<S> where S: Service<hyper::Request<Vec<u8>>, Response = hyper::Response<Vec<u8>>> {
            pub fn new(service: S, base_path: String) -> Self {
                Self { service, base_path }
            }

            #(#functions)*
        }
    }).to_tokens(tokens);
}