use openapiv3::{OpenAPI, Components, SchemaKind, Type, ReferenceOr, ObjectType, Schema, Paths, PathItem, Operation, MediaType, ParameterSchemaOrContent, StatusCode, Responses};
use inflector::cases::{snakecase::to_snake_case, pascalcase::to_pascal_case};

#[derive(Default)]
pub struct Spec {
    pub endpoints: Vec<Endpoint>,
    pub type_definitions: Vec<TypeDefinition>,
}

impl Spec {
    pub fn from_open_api(open_api: &OpenAPI) -> Spec {
        let mut generator = SpecGenerator::new(open_api);
        generator.generate();
        generator.spec
    }
}

pub struct Endpoint {
    pub name: String,
    pub path: String,
    pub method: Method,
    pub parameters: Vec<Parameter>,
    pub request_body: Option<RequestBody>,
    pub success_response: Response,
}

pub struct Response {
    pub status_code: u16,
    pub response_type: Option<String>,
}

pub struct Parameter {
    pub name: String,
    pub original_name: String,
    pub type_name: String,
    pub required: bool,
    pub parameter_type: ParameterType,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum ParameterType {
    Path,
    Query,
}

pub struct RequestBody {
    pub required: bool,
    pub type_name: String,
}

pub struct TypeDefinition {
    pub name: String,
    pub kind: TypeKind,
}

pub enum TypeKind {
    Alias(String),
    Object {
        properties: Vec<Property>
    },
    Enum {
        variants: Vec<Variant>
    }
}

pub struct Variant {
    pub name: String,
    pub original_name: String,
}

pub struct Property {
    pub name: String,
    pub original_name: String,
    pub type_name: String,
    pub required: bool,
}

pub enum ScalarType {
    String,
    Number,
}

struct SpecGenerator<'o> {
    pub spec: Spec,
    pub open_api: &'o OpenAPI,
}

impl<'s> SpecGenerator<'s> {
    fn new(open_api: &'s OpenAPI) -> Self {
        Self {
            spec: Spec::default(),
            open_api,
        }
    }

    fn generate(&mut self) {
        if let Some(components) = self.open_api.components.as_ref() {
            self.generate_components(components);
        }
        self.generate_paths(&self.open_api.paths);
    }

    fn generate_components(&mut self, components: &Components) {
        for (name, schema_or_ref) in &components.schemas {
            if let ReferenceOr::Item(schema) = schema_or_ref {
                self.generate_schema(&name, schema, false);
            }
        }
    }

    fn generate_schema(&mut self, name: &str, schema: &Schema, inline: bool) -> String {
        match &schema.schema_kind {
            SchemaKind::Type(Type::Object(object)) => {
                self.generate_object(&name, object);
                name.to_string()
            }
            SchemaKind::Type(Type::String(string_type)) if !string_type.enumeration.is_empty() => {
                self.generate_enum(&name, string_type.enumeration.as_slice());
                name.to_string()
            }
            SchemaKind::Type(Type::String(string_type)) => {
                if inline {
                    "String".to_string()
                } else {
                    self.spec.type_definitions.push(TypeDefinition {
                        name: to_pascal_case(&name),
                        kind: TypeKind::Alias("String".to_string())
                    });
                    name.to_string()
                }
            }
            SchemaKind::Type(Type::Array(array_type)) => {
                if inline {
                    let element_name = format!("{}Element", name);
                    format!("Vec<{}>", self.generate_type_name(&array_type.items.clone().unbox()))
                } else {
                    unimplemented!("array aliases")
                }
            }
            SchemaKind::OneOf { one_of } => {
                // TODO actually generate an enum or something
                if inline {
                    "()".to_string()
                } else {
                    self.spec.type_definitions.push(TypeDefinition {
                        name: to_pascal_case(&name),
                        kind: TypeKind::Alias("()".to_string()),
                    });
                    name.to_string()

                }
            }
            kind => unimplemented!("Unknown outer schema kind {:?}", kind)
        }
    }

    fn generate_type_name(&mut self, type_schema: &ReferenceOr<Schema>) -> String {
        match type_schema {
            ReferenceOr::Item(item) => {
                match &item.schema_kind {
                    SchemaKind::Type(Type::String(string_type)) => "String".to_string(),
                    SchemaKind::Type(Type::Number(number_type)) => "f64".to_string(),
                    SchemaKind::Type(Type::Boolean {}) => "bool".to_string(),
                    y => unimplemented!("Unexpected schema kind {:?}", y)
                }
            }
            ReferenceOr::Reference { reference } => {
                type_from_reference(&reference)
            }
        }
    }

    fn generate_object(&mut self, name: &str, object: &ObjectType) {
        let properties = object.properties.iter().map(|(field_name, reference_or_properties): (&String, &ReferenceOr<Box<Schema>>)| {
            let property_name = to_property_name(name, &field_name);
            let type_name = self.generate_type_name(&reference_or_properties.clone().unbox());
            let required = object.required.contains(&property_name);
            Property {
                name: property_name,
                original_name: field_name.to_string(),
                type_name,
                required,
            }
        }).collect();
        self.spec.type_definitions.push(TypeDefinition {
            name: to_pascal_case(name),
            kind: TypeKind::Object { properties }
        })
    }

    fn generate_enum(&mut self, name: &str, variant_names: &[String]) {
        let variants = variant_names.iter().map(|original_name| {
            Variant {
                name: to_pascal_case(original_name),
                original_name: original_name.clone(),
            }
        }).collect();
        self.spec.type_definitions.push(TypeDefinition {
            name: to_pascal_case(name),
            kind: TypeKind::Enum { variants }
        })
    }

    fn generate_paths(&mut self, paths: &Paths) {
        let function_iter: Vec<(String, Method, &Operation)> = paths.iter()
            .flat_map(|(url, item)| {
                if let ReferenceOr::Item(item) = item {
                    get_methods_from_type(&url, item)
                } else {
                    Vec::new()
                }
            }).collect();
        for (url, method, operation) in function_iter {
            let name = to_snake_case(&format!("{:?}_{}", method, url.trim_start_matches("/").replace("/", "_")));

            let request_body = operation.request_body.as_ref().and_then(|request_body| {
                match request_body {
                    ReferenceOr::Reference { reference } => {
                        if reference.starts_with(REQUEST_BODY_PREFIX) {
                            let item = &self.open_api.components.as_ref().unwrap().request_bodies[&reference[REQUEST_BODY_PREFIX.len()..]];
                            match item {
                                ReferenceOr::Item(item) => self.generate_request_body(&name, item),
                                ReferenceOr::Reference { reference } => unimplemented!("Chained reference")
                            }
                        } else {
                            unimplemented!("Unexpected request body reference {}", reference);
                        }
                    },
                    ReferenceOr::Item(item) => self.generate_request_body(&name, item),
                }
            });

            let parameters = operation.parameters.iter().map(|parameter_or_ref| {
                match parameter_or_ref {
                    ReferenceOr::Reference { reference } => if reference.starts_with(PARAMETER_PREFIX) {
                        let parameter = &self.open_api.components.as_ref().unwrap().parameters[&reference[PARAMETER_PREFIX.len()..]];
                        match parameter {
                            ReferenceOr::Item(item) => {
                                self.generate_parameter(item)
                            }
                            ReferenceOr::Reference { .. } => unimplemented!()
                        }
                    } else {
                        unimplemented!()
                    },
                    ReferenceOr::Item(parameter) => self.generate_parameter(parameter),
                }
            }).collect();

            let success_response = operation.responses.responses.get(&StatusCode::Code(200)).map(|resp| (200, resp))
                .or(operation.responses.responses.get(&StatusCode::Code(204)).map(|resp| (204, resp)))
                .map(|(code, parameter_or_ref)| {
                    match parameter_or_ref {
                        ReferenceOr::Reference { reference} => if reference.starts_with(RESPONSE_PREFIX) {
                            let response = &self.open_api.components.as_ref().unwrap().responses[&reference[RESPONSE_PREFIX.len()..]];
                            match response {
                                ReferenceOr::Item(item) => (code, item),
                                ReferenceOr::Reference { .. } => unimplemented!()
                            }
                        } else {
                            unimplemented!()
                        },
                        ReferenceOr::Item(item) => (code, item),
                    }
                })
                .map(|(code, response): (u16, &openapiv3::Response)| {
                    let response_type = response.content.get(APPLICATION_JSON_CONTENT_TYPE)
                        .and_then(|content: &MediaType| content.schema.as_ref())
                        .map(|schema: &ReferenceOr<Schema>| {
                            match schema {
                                ReferenceOr::Reference { reference } => type_from_reference(reference),
                                ReferenceOr::Item(item) => {
                                    let body_name = format!("{}Response", to_pascal_case(&name));
                                    self.generate_schema(&body_name, item, true)
                                },
                            }
                        });
                    Response {
                        status_code: code,
                        response_type,
                    }
                }).unwrap();

            self.spec.endpoints.push(Endpoint {
                name,
                path: url,
                parameters,
                request_body,
                success_response,
                method,
            });
        }
    }

    fn generate_parameter(&mut self, parameter: &openapiv3::Parameter) -> Parameter {
        match parameter {
            openapiv3::Parameter::Query { parameter_data, .. } => {
                Parameter {
                    name: to_snake_case(&parameter_data.name),
                    original_name: parameter_data.name.clone(),
                    type_name: self.generate_type_name(from_parameter_schema(&parameter_data.format)),
                    required: parameter_data.required,
                    parameter_type: ParameterType::Query,
                }
            }
            openapiv3::Parameter::Header { .. } => unimplemented!(),
            openapiv3::Parameter::Path { parameter_data, .. } => {
                Parameter {
                    name: to_snake_case(&parameter_data.name),
                    original_name: parameter_data.name.clone(),
                    type_name: self.generate_type_name(from_parameter_schema(&parameter_data.format)),
                    required: parameter_data.required,
                    parameter_type: ParameterType::Path,
                }
            },
            openapiv3::Parameter::Cookie { .. } => unimplemented!(),
        }
    }

    fn generate_request_body(&mut self, endpoint_name: &str, request_body: &openapiv3::RequestBody) -> Option<RequestBody> {
        request_body.content.get(APPLICATION_JSON_CONTENT_TYPE).and_then(|ty: &MediaType| {
            ty.schema.as_ref()
        }).and_then(|schema: &ReferenceOr<Schema>| {
            match schema {
                ReferenceOr::Reference { reference } => {
                    Some(RequestBody {
                        required: request_body.required,
                        type_name: type_from_reference(reference)
                    })
                },
                ReferenceOr::Item(item) => {
                    let body_name = format!("{}Body", to_pascal_case(endpoint_name));
                    self.generate_schema(&body_name, item, true);
                    Some(RequestBody {
                        required: request_body.required,
                        type_name: body_name
                    })
                },
            }
        })
    }
}

fn from_parameter_schema(schema: &ParameterSchemaOrContent) -> &ReferenceOr<Schema> {
    match schema {
        ParameterSchemaOrContent::Schema(schema) => schema,
        ParameterSchemaOrContent::Content(_) => unimplemented!(),
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
const PARAMETER_PREFIX: &str = "#/components/parameters/";
const RESPONSE_PREFIX: &str = "#/components/responses/";
const APPLICATION_JSON_CONTENT_TYPE: &str = "application/json";

fn type_from_reference(reference: &str) -> String {
    if reference.starts_with(SCHEMA_PREFIX) {
        to_pascal_case(&reference[SCHEMA_PREFIX.len()..])
    } else {
        unimplemented!("Weird reference {}", reference)
    }
}


#[derive(Debug, Clone, Copy)]
pub enum Method {
    Get,
    Put,
    Post,
    Delete,
    Options,
    Head,
    Patch,
    Trace,
}

impl Method {
    pub fn get_name(&self) -> &str {
        match self {
            Method::Get => "GET",
            Method::Put => "PUT",
            Method::Post => "POST",
            Method::Delete => "DELETE",
            Method::Options => "OPTIONS",
            Method::Head => "HEAD",
            Method::Patch => "PATCH",
            Method::Trace => "TRACE",
        }
    }
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