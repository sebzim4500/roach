use openapiv3::{OpenAPI, ReferenceOr, Schema, SchemaKind, Type, ObjectType, Components, Paths, PathItem, Operation, RequestBody, MediaType};
use proc_macro2::{TokenStream, Ident};
use quote::{quote, ToTokens, format_ident};
use inflector::cases::snakecase::to_snake_case;
use inflector::cases::pascalcase::to_pascal_case;

struct CodeGenerator<'s> {
    spec: &'s OpenAPI,
    tokens: TokenStream,
}

pub fn generate(spec: OpenAPI) -> TokenStream {
    let generator = CodeGenerator::new(&spec);
    generator.generate()
}

impl<'s> CodeGenerator<'s> {
    fn new(spec: &'s OpenAPI) -> Self {
        Self {
            spec,
            tokens: TokenStream::new(),
        }
    }

    fn generate(mut self) -> TokenStream {
        self.write(quote! {
            use serde::{Serialize, Deserialize};
            use tower_service::Service;

            #[derive(Clone)]
            pub struct Client<S: Service<hyper::Request<Vec<u8>>>> {
                service: S,
                base_path: String,
            }
        });

        if let Some(components) = self.spec.components.as_ref() {
            self.generate_components(components);
        }
        self.generate_paths(&self.spec.paths);

        self.tokens
    }

    fn write<T: ToTokens>(&mut self, value: T) {
        value.to_tokens(&mut self.tokens);
    }

    fn generate_components(&mut self, components: &Components) {
        for (name, schema_or_ref) in &components.schemas {
            if let ReferenceOr::Item(schema) = schema_or_ref {
                match &schema.schema_kind {
                    SchemaKind::Type(Type::Object(object)) => {
                        self.generate_object(&name, object);
                    }
                    SchemaKind::Type(Type::String(string_type)) if !string_type.enumeration.is_empty() => {
                        self.generate_enum(&name, string_type.enumeration.as_slice());
                    }
                    SchemaKind::Type(Type::String(string_type)) => {
                        let type_ident = format_ident!("{}", to_pascal_case(&name));
                        self.write(quote! { type #type_ident = String; });
                    }
                    SchemaKind::OneOf { one_of } => {
                        // TODO generate this
                    }
                    kind => unimplemented!("Unknown outer schema kind {:?}", kind)
                }
            }

        }
    }

    fn generate_object(&mut self, name: &str, object: &ObjectType) {
        let type_ident = format_ident!("{}", to_pascal_case(name));
        let properties = object.properties.iter().map(|(field_name, reference_or_properties)| {
            let property_name = to_property_name(name, &field_name);
            let property_ident = format_ident!("{}", property_name);
            let property_type = match reference_or_properties {
                ReferenceOr::Item(item) => {
                    match &item.schema_kind {
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
            if &property_name == field_name {
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
        self.write(quote! {
            #[derive(Serialize, Deserialize)]
            pub struct #type_ident {
                #(#properties),*
            }
        });
    }

    fn generate_enum(&mut self, name: &str, variant_names: &[String]) {
        let type_ident = format_ident!("{}", to_pascal_case(name));
        let variants = variant_names.into_iter().map(|variant_name| {
            let variant = to_pascal_case(&variant_name);
            let variant_ident = format_ident!("{}", variant);
            if &variant == variant_name {
                quote ! { #variant_ident }
            } else {
                quote! {
                    #[serde(rename = #variant_name)]
                    #variant_ident
                }
            }
        });
        self.write(quote! {
            #[derive(Serialize, Deserialize)]
            pub enum #type_ident {
                #(#variants),*
            }
        });
    }

    fn generate_paths(&mut self, paths: &Paths) {
        let functions = paths.iter()
            .flat_map(|(url, item)| {
                if let ReferenceOr::Item(item) = item {
                    get_methods_from_type(&url, item)
                } else {
                    Vec::new()
                }
            })
            .map(|(url, method, operation)| {
                let name = to_snake_case(&format!("{:?}_{}", method, url.trim_start_matches("/").replace("/", "_")));
                let ident = format_ident!("{}", name);

//            let parameters = operation.parameters.into_iter().filter_map(|parameter| {
//                None
//            });
                let body: std::option::IntoIter<TokenStream> = operation.request_body.as_ref().and_then(|body| {
                    match body {
                        ReferenceOr::Reference { reference } => {
                            self.type_from_request_body_reference(&reference).map(|type_name| {
                                quote! { body: #type_name }
                            })
                        },
                        ReferenceOr::Item(item) => {
                            self.type_from_request_body(item)
                        }
                    }
                }).into_iter();
                quote! {
                    async fn #ident(&mut self #(#body)*) {

                    }
                }
            });
        let client_impl = quote! {
            impl<S> Client<S> where S: Service<hyper::Request<Vec<u8>>, Response = hyper::Response<Vec<u8>>> {
                pub fn new(service: S, base_path: String) -> Self {
                    Self { service, base_path }
                }

                #(#functions)*
            }
        };
        self.write(client_impl);
    }

    fn type_from_request_body_reference(&mut self, reference: &str) -> Option<TokenStream> {
        let request_body_name = if reference.starts_with(REQUEST_BODY_PREFIX) {
            &reference[REQUEST_BODY_PREFIX.len()..]
        } else {
            unimplemented!();
        };
        let request_body_or_ref = &self.spec.components.as_ref().unwrap().request_bodies[request_body_name];
        match request_body_or_ref {
            ReferenceOr::Item(request_body) => {
                self.type_from_request_body(request_body)
            }
            ReferenceOr::Reference { .. } => unimplemented!()
        }
    }

    fn type_from_request_body(&mut self, request_body: &RequestBody) -> Option<TokenStream> {
        request_body.content.get("application/json").and_then(|media: &MediaType| {
            media.schema.as_ref()
        }).and_then(|schema: &ReferenceOr<Schema>| {
            match schema {
                ReferenceOr::Reference { reference } => {
                    let ident = type_from_reference(reference);
                    Some(quote! { #ident })
                },
                ReferenceOr::Item(item) => unimplemented!("Unexpected inline request {:?}", item),
            }
        })
    }
}

fn to_property_name(outer_name: &str, field_name: &str) -> String {
    match field_name.to_lowercase().as_str() {
        "type" => format!("{}_type", to_snake_case(outer_name)),
        _ => to_snake_case(field_name),
    }
}

const SCHEMA_PREFIX: &str = "#/components/schemas/";
const REQUEST_BODY_PREFIX: &str = "#/components/requestBodies/";

fn type_from_reference(reference: &str) -> Ident {
    if reference.starts_with(SCHEMA_PREFIX) {
        format_ident!("{}", to_pascal_case(&reference[SCHEMA_PREFIX.len()..]))
    } else {
        unimplemented!("Weird reference {}", reference)
    }
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

fn get_methods_from_type<'p>(url: &str, path: &'p PathItem) -> Vec<(String, Method, &'p Operation)> {
    let mut results = Vec::new();
    results.extend(path.get.as_ref().map(|o| (url.to_string(), Method::Get, o)));
    results.extend(path.put.as_ref().map(|o| (url.to_string(), Method::Put, o)));
    results.extend(path.post.as_ref().map(|o| (url.to_string(), Method::Post, o)));
    results.extend(path.delete.as_ref().map(|o| (url.to_string(), Method::Delete, o)));
    results.extend(path.options.as_ref().map(|o| (url.to_string(), Method::Options, o)));
    results.extend(path.head.as_ref().map(|o| (url.to_string(), Method::Head, o)));
    results.extend(path.patch.as_ref().map(|o| (url.to_string(), Method::Patch, o)));
    results.extend(path.trace.as_ref().map(|o| (url.to_string(), Method::Trace, o)));

    results
}