use proc_macro::TokenStream;

use quote::spanned::Spanned;
use quote::{format_ident, quote, quote_spanned};
use std::boxed::Box;
use syn::parse::{Parse, ParseBuffer, ParseStream, Result};
use syn::{bracketed, parse_macro_input, Expr, Ident, Stmt, Token, Type};

trait InsField {
    fn parse(input: &ParseBuffer) -> Result<Self>
    where
        Self: Sized;
    fn setup(&self) -> Option<Result<Stmt>> {
        None
    }
    fn byte(&self, nth: u8) -> Result<Expr>;
    fn len(&self) -> u8;

    fn label(&self) -> Option<Result<Expr>> {
        None
    }
}

struct ModRM {
    mod_field: Expr,
    reg: Expr,
    rm: Expr,
}

impl InsField for ModRM {
    fn parse(input: &ParseBuffer) -> Result<Self> {
        let mod_field = input.parse::<Expr>()?;
        input.parse::<Token![,]>()?;
        let reg = input.parse::<Expr>()?;
        input.parse::<Token![,]>()?;
        let rm = input.parse::<Expr>()?;

        Ok(ModRM { mod_field, reg, rm })
    }
    fn byte(&self, _: u8) -> Result<Expr> {
        let mod_field = &self.mod_field;
        let reg = &self.reg;
        let rm = &self.rm;

        let expr = quote! {
           {
                let mod_field = u8::from(#mod_field);
                let reg_field = u8::from(#reg);
                let rm_field = u8::from(#rm);
                assert!(mod_field <= 0b11);
                assert!(reg_field <= 0b111);
                assert!(rm_field <= 0b111);

               (mod_field << 6) + (reg_field << 3) + rm_field
           }
        };
        syn::parse2(expr)
    }

    fn len(&self) -> u8 {
        1
    }
}

struct Imm32 {
    val: Expr,
}

impl InsField for Imm32 {
    fn parse(input: &ParseBuffer) -> Result<Self> {
        Ok(Self {
            val: input.parse()?,
        })
    }

    fn setup(&self) -> Option<Result<Stmt>> {
        let val = &self.val;
        let span = val.__span();
        let stmt = quote_spanned! {span =>
            let imm32 = (#val).to_le_bytes();
        };
        Some(syn::parse2(stmt))
    }

    fn byte(&self, nth: u8) -> Result<Expr> {
        let expr = quote! { imm32[#nth as usize] };
        syn::parse2(expr)
    }

    fn len(&self) -> u8 {
        4
    }
}

struct Rel32(Expr);

impl InsField for Rel32 {
    fn parse(input: &ParseBuffer) -> Result<Self> {
        Ok(Self(input.parse()?))
    }

    fn setup(&self) -> Option<Result<Stmt>> {
        let label = &self.0;
        let span = label.__span();
        Some(syn::parse2(quote_spanned! {span =>
            let rel32 = (#label).off32().to_le_bytes();
        }))
    }

    fn byte(&self, nth: u8) -> Result<Expr> {
        let span = self.0.__span();

        syn::parse2(quote_spanned! {span =>
            rel32[#nth as usize]
        })
    }

    fn label(&self) -> Option<Result<Expr>> {
        Some(Ok(self.0.clone()))
    }

    fn len(&self) -> u8 {
        4
    }
}

struct Imm8 {
    val: Expr,
}

impl InsField for Imm8 {
    fn parse(input: &ParseBuffer) -> Result<Self> {
        Ok(Self {
            val: input.parse()?,
        })
    }

    fn setup(&self) -> Option<Result<Stmt>> {
        let val = &self.val;
        let span = self.val.__span();
        let stmt = quote_spanned! {span =>
            let imm8 = (#val).to_le_bytes();
        };
        Some(syn::parse2(stmt))
    }

    fn byte(&self, nth: u8) -> Result<Expr> {
        let span = self.val.__span();

        let expr = quote_spanned! {span=> imm8[#nth as usize] };
        syn::parse2(expr)
    }

    fn len(&self) -> u8 {
        1
    }
}

struct Instruction {
    name: Ident,
    opcodes: Vec<syn::Expr>,
    var_fields: Vec<(Ident, Type)>,
    ins_fields: Vec<Box<dyn InsField>>,
}

fn parse_var_fields(input: &ParseStream) -> Result<Vec<(Ident, Type)>> {
    let mut var_fields = vec![];

    while !input.peek(Token![=>]) {
        let ident = input.parse::<Ident>()?;
        input.parse::<Token![:]>()?;
        let ident_type = input.parse::<Type>()?;
        let _ = input.parse::<Token![,]>();
        var_fields.push((ident, ident_type));
    }

    Ok(var_fields)
}

mod keyword {
    syn::custom_keyword!(ModRM);
    syn::custom_keyword!(SIB);
    syn::custom_keyword!(Imm8);
    syn::custom_keyword!(Imm32);
    syn::custom_keyword!(Label);
    syn::custom_keyword!(Rel32);
    syn::custom_keyword!(Rel8);
}

fn parse_ins_field(input: &ParseBuffer) -> Result<Box<dyn InsField>> {
    let lookahead = input.lookahead1();

    let content;
    if lookahead.peek(keyword::ModRM) {
        input.parse::<keyword::ModRM>()?;
        syn::parenthesized!(content in input);

        Ok(Box::new(ModRM::parse(&content)?))
    } else if lookahead.peek(keyword::Imm8) {
        input.parse::<keyword::Imm8>()?;
        syn::parenthesized!(content in input);

        Ok(Box::new(Imm8::parse(&content)?))
    } else if lookahead.peek(keyword::Imm32) {
        input.parse::<keyword::Imm32>()?;
        syn::parenthesized!(content in input);

        Ok(Box::new(Imm32::parse(&content)?))
    } else if lookahead.peek(keyword::Rel32) {
        input.parse::<keyword::Rel32>()?;
        syn::parenthesized!(content in input);

        Ok(Box::new(Rel32::parse(&content)?))
    } else {
        Err(input.error("REEEEEEEEEEEEEEEEE"))
    }
}

fn parse_ins_fields(input: &ParseBuffer) -> Result<Vec<Box<dyn InsField>>> {
    if input.peek(syn::token::Bracket) {
        let content;
        bracketed!(content in input);

        let mut ins_fields = vec![];
        while !content.is_empty() {
            let field = parse_ins_field(&content)?;
            let _ = content.parse::<Token![,]>();
            ins_fields.push(field);
        }

        Ok(ins_fields)
    } else {
        Ok(vec![parse_ins_field(input)?])
    }
}

impl Parse for Instruction {
    fn parse(input: &ParseBuffer) -> Result<Self> {
        let opcodes = if input.peek(syn::token::Bracket) {
            let mut opcodes = vec![];
            let content;
            bracketed!(content in input);
            while !content.is_empty() {
                opcodes.push(content.parse::<Expr>()?);
                let _ = content.parse::<Token![,]>();
            }
            opcodes
        } else {
            vec![input.parse()?]
        };
        input.parse::<Token![,]>()?;
        let name = input.parse::<Ident>()?;
        let var_fields = if input.peek(Token![,]) {
            let _ = input.parse::<Token![,]>();
            parse_var_fields(&input)?
        } else {
            vec![]
        };
        input.parse::<Token![=>]>()?;
        let ins_fields = parse_ins_fields(&input)?;

        Ok(Instruction {
            name,
            opcodes,
            var_fields,
            ins_fields,
        })
    }
}

struct Instructions(Vec<Instruction>);

impl Parse for Instructions {
    fn parse(input: ParseStream) -> Result<Self> {
        let prefix_name = if input.peek(Ident) {
            let prefix_name = Some(input.parse::<Ident>()?);
            input.parse::<Token![,]>()?;
            prefix_name
        } else {
            None
        };
        let mut instructions = vec![];

        while !input.is_empty() {
            let mut instruction: Instruction = input.parse()?;
            if let Some(prefix_name) = &prefix_name {
                instruction.name = format_ident!("{}{}", prefix_name, instruction.name);
            }
            instructions.push(instruction)
        }

        Ok(Instructions(instructions))
    }
}

#[proc_macro]
pub fn x86(tokens: TokenStream) -> TokenStream {
    let instructions = parse_macro_input!(tokens as Instructions);

    let instructions = instructions.0.iter().map(
        |Instruction {
             name,
             opcodes,
             var_fields,
             ins_fields,
         }| {
            let var_field_names1 = var_fields.iter().map(|(name, _)| name);
            let var_field_names2 = var_fields.iter().map(|(name, _)| name);
            let var_field_types = var_fields.iter().map(|(_, ty)| ty);

            let ins_len =
                opcodes.len() as u32 + ins_fields.iter().map(|f| f.len()).sum::<u8>() as u32;
            let mut ins_field_setups = vec![];
            let mut label_field = None;

            for field in ins_fields {
                if let Some(expr) = field.setup() {
                    let expr = match expr {
                        Ok(expr) => expr,
                        Err(e) => return e.into_compile_error(),
                    };
                    ins_field_setups.push(expr)
                }
                if let Some(expr) = field.label() {
                    label_field = Some(match expr {
                        Ok(expr) => expr,
                        Err(e) => return e.into_compile_error(),
                    })
                }
            }

            let label_field = label_field.map(|label_field| {
                let span = label_field.__span();
                let code = quote_spanned! {span =>
                    fn need_reloff_resolving(&mut self) -> bool { true }

                    fn resolve_reloff(&mut self) -> &mut LabelField {
                        let Self(#(#var_field_names2),*) = self;
                        #label_field
                    }
                };
                code
            });

            let mut ins_field_bytes = vec![];

            for field in ins_fields {
                let field_len = field.len();

                for i in 0..field_len {
                    ins_field_bytes.push(field.byte(i).unwrap())
                }
            }

            quote! {
                struct #name(#(#var_field_types),*);

                impl Encodable for #name {
                    fn encode(&self) -> Vec<u8> {
                        let Self(#(#var_field_names1),*) = self;
                        #(#ins_field_setups)*

                        vec![#(#opcodes,)* #(#ins_field_bytes),*]
                    }
                    fn len(&self) -> u32 { #ins_len }
                    #label_field
                }
            }
        },
    );

    quote! {
        #(#instructions)*
    }
    .into()
}
