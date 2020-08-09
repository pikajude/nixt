#[macro_use] extern crate darling;
#[macro_use] extern crate quote;

use darling::{ast, util, FromDeriveInput};
use proc_macro::TokenStream;
use syn::*;

#[proc_macro_attribute]
pub fn settings(_: TokenStream, input: TokenStream) -> TokenStream {
  let item = parse_macro_input!(input as DeriveInput);
  let item_attrs = &item.attrs;
  let items = match Settings::from_derive_input(&item) {
    Ok(x) => x,
    Err(e) => return e.write_errors().into(),
  };

  let mut flags = vec![];
  let mut field_toks = vec![];
  let mut field_values = vec![];

  let fields = items.data.take_struct().unwrap();

  for SettingField {
    ident,
    ty,
    attrs,
    help,
    flag,
    alias,
    value,
    hide,
  } in fields.iter()
  {
    let flag = flag
      .as_ref()
      .cloned()
      .unwrap_or_else(|| ident.as_ref().unwrap().to_string().replace('-', "_"));
    flags.push(quote! { #(#attrs)* #flag });
    if let Some(a) = alias {
      flags.push(quote! { #(#attrs)* #a });
    }
    let value = syn::parse_str::<Expr>(value).unwrap();
    field_values.push(quote! {
      #(#attrs)*
      #ident: #value
    });
    if *hide {
      field_toks.push(quote! {
        #(#attrs)*
        #[doc(hidden)]
        pub #ident: #ty
      });
    } else {
      field_toks.push(quote! {
        #(#attrs)*
        #[doc = #help]
        pub #ident: #ty
      });
    }
  }

  (quote! {
    static FLAGS: &'static [&'static str] = &[
      #(#flags),*
    ];

    #(#item_attrs),*
    pub struct Settings {
      #(#field_toks,)*
      paths: Paths
    }

    impl Default for Settings {
      fn default() -> Self {
        let paths = Paths::default();
        Self {
          #(#field_values,)*
          paths
        }
      }
    }
  })
  .into()
}

#[derive(FromField, Debug)]
#[darling(attributes(setting), forward_attrs(allow, doc, cfg))]
struct SettingField {
  ident: Option<Ident>,
  ty: Type,
  value: String,
  #[darling(default)]
  help: String,
  #[darling(default)]
  hide: bool,
  #[darling(default)]
  flag: Option<String>,
  #[darling(default)]
  alias: Option<String>,
  attrs: Vec<Attribute>,
}

#[derive(Debug, FromDeriveInput)]
#[darling(attributes(setting), supports(struct_named))]
struct Settings {
  data: ast::Data<util::Ignored, SettingField>,
}
