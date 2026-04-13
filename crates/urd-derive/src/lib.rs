//! # urd-derive
//!
//! Procedural derive macro for the [`ExternObject`] trait from the `urd`
//! scripting language runtime.
//!
//! ## Usage
//!
//! ```rust,ignore
//! use urd::prelude::*;
//!
//! #[derive(ExternObject)]
//! #[extern_object(type_name = "Player")]
//! struct Player {
//!     hp: i32,
//!     name: String,
//!
//!     #[extern_object(readonly)]
//!     id: u64,
//!
//!     #[extern_object(skip)]
//!     internal: SomeInternalType,
//!
//!     #[extern_object(rename = "pos_x")]
//!     position_x: f64,
//! }
//! ```
//!
//! ## Attributes
//!
//! ### Container-level (`#[extern_object(...)]` on the struct)
//!
//! | Attribute | Description |
//! |-----------|-------------|
//! | `type_name = "..."` | Override the type name returned by `ExternObject::type_name()`. Defaults to the struct name. |
//!
//! ### Field-level (`#[extern_object(...)]` on a field)
//!
//! | Attribute | Description |
//! |-----------|-------------|
//! | `skip` | Omit this field entirely — not readable, not writable, not listed. |
//! | `readonly` | Field is readable but not writable from scripts. |
//! | `rename = "..."` | Expose the field under a different name in scripts. |

use proc_macro::TokenStream;
use quote::quote;
use syn::{Data, DeriveInput, Fields, Lit, parse_macro_input};

/// Derive the `ExternObject` trait for a struct.
///
/// See the [module-level documentation](crate) for usage and supported
/// attributes.
#[proc_macro_derive(ExternObject, attributes(extern_object))]
pub fn derive_extern_object(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match impl_extern_object(&input) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

// ── Container-level attribute parsing ─────────────────────────────────────────

struct ContainerAttrs {
    type_name: Option<String>,
}

fn parse_container_attrs(attrs: &[syn::Attribute]) -> syn::Result<ContainerAttrs> {
    let mut type_name = None;

    for attr in attrs {
        if !attr.path().is_ident("extern_object") {
            continue;
        }
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("type_name") {
                if type_name.is_some() {
                    return Err(meta.error("duplicate `type_name` attribute"));
                }
                let value = meta.value()?;
                let lit: Lit = value.parse()?;
                if let Lit::Str(s) = lit {
                    type_name = Some(s.value());
                    Ok(())
                } else {
                    Err(meta.error("expected string literal for `type_name`"))
                }
            } else {
                Err(meta.error("unknown extern_object container attribute"))
            }
        })?;
    }

    Ok(ContainerAttrs { type_name })
}

// ── Field-level attribute parsing ─────────────────────────────────────────────

struct FieldAttrs {
    skip: bool,
    readonly: bool,
    rename: Option<String>,
}

fn parse_field_attrs(attrs: &[syn::Attribute]) -> syn::Result<FieldAttrs> {
    let mut skip = false;
    let mut readonly = false;
    let mut rename = None;

    for attr in attrs {
        if !attr.path().is_ident("extern_object") {
            continue;
        }

        // Handle bare `#[extern_object(skip)]` / `#[extern_object(readonly)]`
        // as well as `#[extern_object(rename = "...")]`.
        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("skip") {
                if skip {
                    return Err(meta.error("duplicate `skip` attribute"));
                }
                skip = true;
                Ok(())
            } else if meta.path.is_ident("readonly") {
                if readonly {
                    return Err(meta.error("duplicate `readonly` attribute"));
                }
                readonly = true;
                Ok(())
            } else if meta.path.is_ident("rename") {
                if rename.is_some() {
                    return Err(meta.error("duplicate `rename` attribute"));
                }
                let value = meta.value()?;
                let lit: Lit = value.parse()?;
                if let Lit::Str(s) = lit {
                    rename = Some(s.value());
                    Ok(())
                } else {
                    Err(meta.error("expected string literal for `rename`"))
                }
            } else {
                Err(meta.error("unknown extern_object field attribute"))
            }
        })?;
    }

    if skip && (readonly || rename.is_some()) {
        if let Some(attr) = attrs.first() {
            return Err(syn::Error::new_spanned(
                attr,
                "skipped fields cannot have `readonly` or `rename` attributes"
            ));
        }
    }

    Ok(FieldAttrs {
        skip,
        readonly,
        rename,
    })
}

// ── Code generation ───────────────────────────────────────────────────────────

struct FieldInfo {
    /// The Rust field identifier (for `self.field_name`).
    ident: syn::Ident,
    /// The script-visible name (after rename).
    script_name: String,
    /// Whether the field is read-only from scripts.
    readonly: bool,
}

fn impl_extern_object(input: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let struct_name = &input.ident;
    let container = parse_container_attrs(&input.attrs)?;
    let type_name_str = container
        .type_name
        .unwrap_or_else(|| struct_name.to_string());

    let fields = match &input.data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(named) => &named.named,
            _ => {
                return Err(syn::Error::new_spanned(
                    struct_name,
                    "ExternObject can only be derived for structs with named fields",
                ));
            }
        },
        _ => {
            return Err(syn::Error::new_spanned(
                struct_name,
                "ExternObject can only be derived for structs",
            ));
        }
    };

    // Collect field metadata, filtering out skipped fields.
    let mut field_infos: Vec<FieldInfo> = Vec::new();
    for field in fields {
        let attrs = parse_field_attrs(&field.attrs)?;
        if attrs.skip {
            continue;
        }
        let Some(ident) = field.ident.clone() else {
            return Err(syn::Error::new_spanned(field, "named field must have an ident"));
        };
        let script_name = attrs.rename.unwrap_or_else(|| ident.to_string());
        field_infos.push(FieldInfo {
            ident,
            script_name,
            readonly: attrs.readonly,
        });
    }

    // ── type_name() ───────────────────────────────────────────────────────

    let type_name_impl = quote! {
        fn type_name(&self) -> &str {
            #type_name_str
        }
    };

    // ── display() ─────────────────────────────────────────────────────────

    let display_fields: Vec<proc_macro2::TokenStream> = field_infos
        .iter()
        .enumerate()
        .map(|(i, fi)| {
            let ident = &fi.ident;
            let name = &fi.script_name;
            let comma = if i > 0 {
                quote! { s.push_str(", "); }
            } else {
                quote! {}
            };
            quote! {
                #comma
                ::std::write!(&mut s, "{}: {}", #name, ::urd::runtime::extern_object::display_brief(
                    &::urd::runtime::extern_object::IntoRuntimeValue::to_runtime_value(&self.#ident)
                )).unwrap();
            }
        })
        .collect();

    let display_impl = if display_fields.is_empty() {
        quote! {
            fn display(&self) -> String {
                format!("{} {{}}", #type_name_str)
            }
        }
    } else {
        quote! {
            fn display(&self) -> String {
                use ::std::fmt::Write;
                let mut s = String::new();
                ::std::write!(&mut s, "{} {{ ", #type_name_str).unwrap();
                #(#display_fields)*
                s.push_str(" }");
                s
            }
        }
    };

    // ── get() ─────────────────────────────────────────────────────────────

    let get_arms: Vec<proc_macro2::TokenStream> = field_infos
        .iter()
        .map(|fi| {
            let ident = &fi.ident;
            let name = &fi.script_name;
            quote! {
                #name => ::std::result::Result::Ok(
                    ::urd::runtime::extern_object::IntoRuntimeValue::to_runtime_value(&self.#ident)
                ),
            }
        })
        .collect();

    let get_impl = quote! {
        fn get(&self, field: &str) -> ::std::result::Result<::urd::RuntimeValue, String> {
            match field {
                #(#get_arms)*
                other => ::std::result::Result::Err(
                    format!("no field '{}' on {}", other, #type_name_str)
                ),
            }
        }
    };

    // ── set() ─────────────────────────────────────────────────────────────

    let set_arms: Vec<proc_macro2::TokenStream> = field_infos
        .iter()
        .map(|fi| {
            let ident = &fi.ident;
            let name = &fi.script_name;
            if fi.readonly {
                quote! {
                    #name => ::std::result::Result::Err(
                        format!("field '{}' on {} is read-only", #name, #type_name_str)
                    ),
                }
            } else {
                quote! {
                    #name => {
                        self.#ident = ::urd::runtime::extern_object::FromRuntimeValue::from_runtime_value(value)?;
                        ::std::result::Result::Ok(())
                    }
                }
            }
        })
        .collect();

    let set_impl = quote! {
        fn set(&mut self, field: &str, value: ::urd::RuntimeValue) -> ::std::result::Result<(), String> {
            match field {
                #(#set_arms)*
                other => ::std::result::Result::Err(
                    format!("no field '{}' on {}", other, #type_name_str)
                ),
            }
        }
    };

    // ── fields() ──────────────────────────────────────────────────────────

    let field_names: Vec<&String> = field_infos.iter().map(|fi| &fi.script_name).collect();

    let fields_impl = quote! {
        fn fields(&self) -> Vec<String> {
            vec![#(#field_names.to_string()),*]
        }
    };

    // ── Assemble ──────────────────────────────────────────────────────────

    let mut generics = input.generics.clone();
    for param in generics.type_params_mut() {
        param.bounds.push(syn::parse_quote!(::urd::runtime::extern_object::IntoRuntimeValue));
        param.bounds.push(syn::parse_quote!(::urd::runtime::extern_object::FromRuntimeValue));
    }
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let expanded = quote! {
        impl #impl_generics ::urd::ExternObject for #struct_name #ty_generics #where_clause {
            #type_name_impl
            #display_impl
            #get_impl
            #set_impl
            #fields_impl
        }
    };

    Ok(quote! {
        #expanded
    })
}
