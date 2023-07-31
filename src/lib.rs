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


struct SIB {
    scale: Expr,
    index: Expr,
    base: Expr,
}

impl InsField for SIB {
    fn parse(input: &ParseBuffer) -> Result<Self> {
        let scale = input.parse::<Expr>()?;
        input.parse::<Token![,]>()?;
        let index = input.parse::<Expr>()?;
        input.parse::<Token![,]>()?;
        let base = input.parse::<Expr>()?;

        Ok(Self { scale, index, base })
    }
    fn byte(&self, _: u8) -> Result<Expr> {
        let scale = &self.scale;
        let index = &self.index;
        let base = &self.base;

        syn::parse2(quote! {
            let scale = u8::from(#scale);
            let index = u8::from(#index);
            let base = u8::from(#base);
            assert!(scale <= 0b11);
            assert!(index <= 0b111);
            assert!(base <= 0b111);

            (scale << 6) + (index << 3) + base
        })
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
        let nth = nth as usize;
        let expr = quote! { imm32[#nth] };
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
        let nth = nth as usize;

        syn::parse2(quote_spanned! {span =>
            rel32[#nth]
        })
    }

    fn label(&self) -> Option<Result<Expr>> {
        Some(Ok(self.0.clone()))
    }

    fn len(&self) -> u8 {
        4
    }
}

struct Rel8(Expr);

impl InsField for Rel8 {
    fn parse(input: &ParseBuffer) -> Result<Self> {
        Ok(Self(input.parse()?))
    }

    fn setup(&self) -> Option<Result<Stmt>> {
        let label = &self.0;
        let span = label.__span();
        Some(syn::parse2(quote_spanned! {span =>
            let rel8 = (#label).off8().to_le_bytes();
        }))
    }

    fn byte(&self, _: u8) -> Result<Expr> {
        let span = self.0.__span();

        syn::parse2(quote_spanned! {span =>
            rel8[0]
        })
    }

    fn label(&self) -> Option<Result<Expr>> {
        Some(Ok(self.0.clone()))
    }

    fn len(&self) -> u8 {
        1
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

    fn byte(&self, _: u8) -> Result<Expr> {
        let span = self.val.__span();
        let expr = quote_spanned! {span=> imm8[0] };
        syn::parse2(expr)
    }

    fn len(&self) -> u8 {
        1
    }
}

struct Instruction {
    name: Ident,
    prefixes: Vec<Box<dyn InsField>>,
    opcodes: Vec<syn::Expr>,
    var_fields: Vec<(Ident, Type)>,
    ins_fields: Vec<Box<dyn InsField>>,
}

fn parse_var_fields(input: &ParseBuffer) -> Result<Vec<(Ident, Type)>> {
    let mut var_fields = vec![];

    while !input.peek(Token![=>]) && !input.peek(Token![;]) {
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

    syn::custom_keyword!(REX);
    syn::custom_keyword!(VEX3b);
    syn::custom_keyword!(VEX2b);
    syn::custom_keyword!(EVEX);
}

fn parse_ins_field(input: &ParseBuffer) -> Result<Box<dyn InsField>> {
    let lookahead = input.lookahead1();
    let content;

    macro_rules! parse_ins_field {
        ($first:ident, $($idents:ident),*) => {
            if lookahead.peek(keyword::$first) {
                input.parse::<keyword::$first>()?;
                syn::parenthesized!(content in input);

                Ok(Box::new($first::parse(&content)?))
            }
            $(else if lookahead.peek(keyword::$idents) {
                input.parse::<keyword::$idents>()?;
                syn::parenthesized!(content in input);

                Ok(Box::new($idents::parse(&content)?))
            })*
            else {
                Err(input.error("Unknown instruction field"))
            }
        }
    }

    parse_ins_field![ModRM, SIB, Imm32, Imm8, Rel32, Rel8]
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

enum ConstOrExpr<T> {
    Const(T),
    Expr(syn::Expr),
}

impl<T> ConstOrExpr<T> {
    pub fn to_const(&self) -> Option<&T> {
        if let Self::Const(c) = self {
            Some(c)
        } else {
            None
        }
    }
    pub fn to_expr(&self) -> Option<&Expr> {
        if let Self::Expr(e) = self {
            Some(e)
        } else {
            None
        }
    }
}

mod rex {
    use crate::quote;
    use crate::ConstOrExpr::{self, *};
    use crate::InsField;
    use proc_macro::TokenStream;

    use quote::spanned::Spanned;
    use quote::{format_ident, quote_spanned};
    use std::boxed::Box;
    use syn::parse::{Parse, ParseBuffer, ParseStream, Result};
    use syn::{bracketed, parse_macro_input, token, Expr, Ident, Stmt, Token, Type};

    syn::custom_keyword!(W);
    syn::custom_keyword!(R);
    syn::custom_keyword!(X);
    syn::custom_keyword!(B);

    pub struct Rex {
        w: ConstOrExpr<bool>,
        r: ConstOrExpr<bool>,
        x: ConstOrExpr<bool>,
        b: ConstOrExpr<bool>,
    }

    impl InsField for Rex {
        //{REX}.[R].[W].[X].[B].[(R=Expr)].[(B=Expr)].[(X=Expr)].[(W=Expr)]
        //tokens in braces is already consumed
        fn parse(input: &syn::parse::ParseBuffer) -> syn::Result<Self> {
            let mut rex = Self {
                w: Const(false),
                r: Const(false),
                x: Const(false),
                b: Const(false),
            };

            while input.parse::<Token![.]>().is_ok() {
                let lookahead = input.lookahead1();
                if lookahead.peek(W) {
                    input.parse::<W>()?;
                    rex.w = Const(true);
                } else if lookahead.peek(R) {
                    input.parse::<R>()?;
                    rex.r = Const(true);
                } else if lookahead.peek(X) {
                    input.parse::<X>()?;
                    rex.x = Const(true);
                } else if lookahead.peek(B) {
                    input.parse::<B>()?;
                    rex.b = Const(true);
                } else if lookahead.peek(token::Paren) {
                    let content;
                    syn::parenthesized!(content in input);
                    let lookahead = content.lookahead1();

                    let field;

                    if lookahead.peek(W) {
                        content.parse::<W>()?;
                        field = &mut rex.w;
                    } else if lookahead.peek(R) {
                        content.parse::<R>()?;
                        field = &mut rex.r;
                    } else if lookahead.peek(X) {
                        content.parse::<X>()?;
                        field = &mut rex.x;
                    } else if lookahead.peek(B) {
                        content.parse::<B>()?;
                        field = &mut rex.b;
                    } else {
                        return Err(content.error("Unknown REX field"));
                    }

                    content.parse::<Token![=]>()?;
                    *field = Expr(content.parse()?)
                } else {
                    return Err(input.error("Unknown REX field"));
                }
            }

            Ok(rex)
        }
        fn byte(&self, _: u8) -> syn::Result<syn::Expr> {
            let const_w = self.w.to_const().map(|v| u8::from(*v)).unwrap_or(0);
            let const_r = self.r.to_const().map(|v| u8::from(*v)).unwrap_or(0);
            let const_x = self.r.to_const().map(|v| u8::from(*v)).unwrap_or(0);
            let const_b = self.x.to_const().map(|v| u8::from(*v)).unwrap_or(0);

            let const_rex = (0b0100 << 4) + (const_w << 3) + (const_r << 2) + (const_x << 1) + const_b;

            let expr_w = self.w.to_expr().map(|expr_w| quote!{ + ((#expr_w) << 3) });
            let expr_r = self.r.to_expr().map(|expr_r| quote!{ + ((#expr_r) << 2) });
            let expr_x = self.x.to_expr().map(|expr_x| quote!{ + ((#expr_x) << 1) });
            let expr_b = self.b.to_expr().map(|expr_b| quote!{ + (#expr_b) });

            let expr = quote!{
                {
                    const REX: u8 = #const_rex;
                    REX #expr_w #expr_r #expr_x #expr_b
                }
            };

            syn::parse2(expr)
        }
        fn len(&self) -> u8 {
            1
        }
    }
}

mod vex {
    use crate::quote;
    use crate::ConstOrExpr::{self, *};
    use crate::InsField;
    use proc_macro::TokenStream;

    use quote::spanned::Spanned;
    use quote::{format_ident, quote_spanned};
    use std::boxed::Box;
    use syn::parse::{Parse, ParseBuffer, ParseStream, Result};
    use syn::{bracketed, parse_macro_input, token, Expr, Ident, Stmt, Token, Type};

    syn::custom_keyword!(R);
    syn::custom_keyword!(X);
    syn::custom_keyword!(B);
    syn::custom_keyword!(m_mmmm);
    syn::custom_keyword!(W);
    syn::custom_keyword!(vvvv);
    syn::custom_keyword!(L);
    syn::custom_keyword!(pp);
    syn::custom_keyword!(hex0F);
    syn::custom_keyword!(hex0F38);
    syn::custom_keyword!(hex0F3A);
    syn::custom_keyword!(hex66);
    syn::custom_keyword!(hexF3);
    syn::custom_keyword!(hexF2);
    syn::custom_keyword!(bits128);
    syn::custom_keyword!(bits64);
    syn::custom_keyword!(bits256);

    pub struct Vex3b {
        r: ConstOrExpr<bool>,
        x: ConstOrExpr<bool>,
        b: ConstOrExpr<bool>,
        m_mmmm: ConstOrExpr<u8>, //actual size is 4 bits
        w: ConstOrExpr<bool>,
        vvvv: ConstOrExpr<u8>, //actual size is 4 bits
        l: ConstOrExpr<bool>,
        pp: ConstOrExpr<u8>, //actual size is 2 bits
    }

    impl InsField for Vex3b {
        //{VEX3b}.[R].[X].[B].[W].
        //(m-mmmm=[hex0F][hex0F38][hex0F3A]<Expr>).(vvvv=Expr).(L=[bits64][bits128][bits256]<Expr>).(pp=[hex66][hexF3][hexF2]<Expr>).
        //there is no (foo=[bar]) syntax, its written as [bar]
        fn parse(input: &ParseBuffer) -> Result<Self> {
            let mut vex3b = Self {
                r: Const(true),
                x: Const(true),
                b: Const(true),
                w: Const(false), //FIXME: not sure if this is the correct default
                m_mmmm: Const(0b00000), //invalid state
                vvvv: Const(!0),
                l: Const(false),
                pp: Const(0),
            };

            while input.parse::<Token![.]>().is_ok() {
                let lookahead = input.lookahead1();

                if input.parse::<R>().is_ok() {
                    vex3b.r = Const(false);
                } else if input.parse::<X>().is_ok() {
                    vex3b.x = Const(false);
                } else if input.parse::<B>().is_ok() {
                    vex3b.b = Const(false);
                } else if input.parse::<W>().is_ok() {
                    vex3b.w = Const(true);
                //----------------------------------------
                } else if input.parse::<bits128>().is_ok() || input.parse::<bits64>().is_ok() {
                    vex3b.l = Const(false);
                } else if input.parse::<bits256>().is_ok() {
                    vex3b.l = Const(true);
                //----------------------------------------
                } else if input.parse::<hex0F>().is_ok() {
                    vex3b.m_mmmm = Const(1);
                } else if input.parse::<hex0F38>().is_ok() {
                    vex3b.m_mmmm = Const(0b10);
                } else if input.parse::<hex0F3A>().is_ok() {
                    vex3b.m_mmmm = Const(0b11);
                //----------------------------------------
                } else if input.parse::<hex66>().is_ok() {
                    vex3b.pp = Const(0);
                } else if input.parse::<hexF3>().is_ok() {
                    vex3b.pp = Const(0b10);
                } else if input.parse::<hexF2>().is_ok() {
                    vex3b.pp = Const(0b11);
                //----------------------------------------
                } else if lookahead.peek(token::Paren) {
                    let content;
                    syn::parenthesized!(content in input);

                    if content.parse::<m_mmmm>().is_ok() {
                        content.parse::<Token![=]>()?;
                        vex3b.m_mmmm = Expr(content.parse()?);
                    } else if content.parse::<vvvv>().is_ok() {
                        content.parse::<Token![=]>()?;
                        vex3b.vvvv = Expr(content.parse()?);
                    } else if content.parse::<L>().is_ok() {
                        content.parse::<Token![=]>()?;
                        vex3b.l = Expr(content.parse()?)
                    } else if content.parse::<pp>().is_ok() {
                        content.parse::<Token![=]>()?;
                        vex3b.pp = Expr(content.parse()?);
                    } else if content.parse::<R>().is_ok() {
                        content.parse::<Token![=]>()?;
                        vex3b.r = Expr(content.parse()?);
                    } else if content.parse::<X>().is_ok() {
                        content.parse::<Token![=]>()?;
                        vex3b.x = Expr(content.parse()?);
                    } else if content.parse::<B>().is_ok() {
                        content.parse::<Token![=]>()?;
                        vex3b.b = Expr(content.parse()?);
                    } else if content.parse::<W>().is_ok() {
                        content.parse::<Token![=]>()?;
                        vex3b.w = Expr(content.parse()?);
                    } else {
                        return Err(input.error("Unknown VEX (3 byte) field"))
                    }
                } else {
                    return Err(input.error("Unexpected VEX (3 byte) field"))
                }

            }

            if let Const(0) = vex3b.m_mmmm {
                return Err(input.error("VEX.m_mmmm field must be initialised"))
            }

            Ok(vex3b)
        }

        fn byte(&self, nth: u8) -> Result<syn::Expr> {
            if nth == 0 {
                syn::parse2(quote!{0b11000100})
            } else if nth == 1 {
                let const_r = self.r.to_const().copied().map(u8::from).unwrap_or(0);
                let const_x = self.x.to_const().copied().map(u8::from).unwrap_or(0);
                let const_b = self.b.to_const().copied().map(u8::from).unwrap_or(0);
                let const_mmmm = self.m_mmmm.to_const().copied().map(u8::from).unwrap_or(0);

                let vex3b_constbyte2 = (const_r << 7) + (const_x << 6) + (const_b << 5) + const_mmmm;

                let expr_r = self.r.to_expr().map(|r| quote!{ + ((0b1 << 7) & ((#r) << 7))});
                let expr_x = self.x.to_expr().map(|x| quote!{ + ((0b1 << 6) & ((#x) << 6))});
                let expr_b = self.b.to_expr().map(|b| quote!{ + ((0b1 << 5) & ((#b) << 5))});
                let expr_mmmm = self.m_mmmm.to_expr().map(|mmmm| quote!{ + (0b11111 & (#mmmm))});

                syn::parse2(quote!{
                    {
                        const VEX3B_B2: u8 = #vex3b_constbyte2;
                        VEX3B_B2 #expr_r #expr_x #expr_b #expr_mmmm
                    }
                })
            } else if nth == 2 {
                let const_w = self.w.to_const().copied().map(u8::from).unwrap_or(0);
                let const_vvvv = self.vvvv.to_const().copied().map(u8::from).unwrap_or(0);
                let const_l = self.l.to_const().copied().map(u8::from).unwrap_or(0);
                let const_pp = self.pp.to_const().copied().map(u8::from).unwrap_or(0);

                let vex3b_constbyte3 = (const_w << 7) + (const_vvvv << 3) + (const_l << 2) + const_pp;

                let expr_w = self.w.to_expr().map(|w| quote!{ + ((#w) << 7)});
                let expr_vvvv = self.vvvv.to_expr().map(|vvvv| quote!{ + (0b111000 & ((#vvvv) << 3))});
                let expr_l = self.l.to_expr().map(|l| quote!{ + (0b100 & ((#l) << 2))});
                let expr_pp = self.pp.to_expr().map(|pp| quote!{ + (0b11 & (#pp))});

                syn::parse2(quote!{
                    {
                        const VEX3B_B3: u8 = #vex3b_constbyte3;
                        VEX3B_B3 #expr_w #expr_vvvv #expr_l #expr_pp
                    }
                })
            } else {
                panic!("Tried to access {nth}th byte from 3 byte VEX")
            }
        }

        fn len(&self) -> u8 { 3 }
    }

    pub struct Vex2b {
        r: ConstOrExpr<bool>,
        vvvv: ConstOrExpr<u8>, //actual size is 4 bits
        l: ConstOrExpr<bool>,
        pp: ConstOrExpr<u8>, //actual size is 2 bits
    }

    impl InsField for Vex2b {
        //{VEX3b}.[R].[X].[B].[W].
        //(m-mmmm=[hex0F][hex0F38][hex0F3A]<Expr>).(vvvv=Expr).(L=[bits64][bits128][bits256]<Expr>).(pp=[hex66][hexF3][hexF2]<Expr>).
        //there is no (foo=[bar]) syntax, its written as [bar]
        fn parse(input: &ParseBuffer) -> Result<Self> {
            let mut vex2b = Self {
                r: Const(true),
                vvvv: Const(0b1111 & !0),
                l: Const(false),
                pp: Const(0),
            };

            while input.parse::<Token![.]>().is_ok() {
                let lookahead = input.lookahead1();

                if input.parse::<R>().is_ok() {
                    vex2b.r = Const(false);
                //----------------------------------------
                } else if input.parse::<bits128>().is_ok() || input.parse::<bits64>().is_ok() {
                    vex2b.l = Const(false);
                } else if input.parse::<bits256>().is_ok() {
                    vex2b.l = Const(true);
                //----------------------------------------
                } else if input.parse::<hex66>().is_ok() {
                    vex2b.pp = Const(0);
                } else if input.parse::<hexF3>().is_ok() {
                    vex2b.pp = Const(0b10);
                } else if input.parse::<hexF2>().is_ok() {
                    vex2b.pp = Const(0b11);
                //----------------------------------------
                } else if lookahead.peek(token::Paren) {
                    let content;
                    syn::parenthesized!(content in input);
                    if content.parse::<vvvv>().is_ok() {
                        content.parse::<Token![=]>()?;
                        vex2b.vvvv = Expr(content.parse()?);
                    } else if content.parse::<L>().is_ok() {
                        content.parse::<Token![=]>()?;
                        vex2b.l = Expr(content.parse()?)
                    } else if content.parse::<pp>().is_ok() {
                        content.parse::<Token![=]>()?;
                        vex2b.pp = Expr(content.parse()?);
                    } else if content.parse::<R>().is_ok() {
                        content.parse::<Token![=]>()?;
                        vex2b.r = Expr(content.parse()?);
                    } else if content.parse::<X>().is_ok() {
                        content.parse::<Token![=]>()?;
                    } else {
                        return Err(input.error("Unknown VEX (2 byte) field"))
                    }
                } else {
                    return Err(input.error("Unexpected VEX (2 byte) field"))
                }

            }

            Ok(vex2b)
        }

        fn byte(&self, nth: u8) -> Result<syn::Expr> {
            if nth == 0 {
                syn::parse2(quote!{0b11000101})
            } else if nth == 1 {
                let const_r = self.r.to_const().copied().map(u8::from).unwrap_or(0);
                let const_vvvv = self.vvvv.to_const().copied().map(u8::from).unwrap_or(0);
                let const_l = self.l.to_const().copied().map(u8::from).unwrap_or(0);
                let const_pp = self.pp.to_const().copied().map(u8::from).unwrap_or(0);

                let vex2_constbyte2 = (const_r << 7) + (const_vvvv << 3) + (const_l << 2) + const_pp;

                let expr_r = self.r.to_expr().map(|r| quote!{+ ((0b1 << 7) & ((#r) << 7))});
                let expr_vvvv = self.vvvv.to_expr().map(|vvvv| quote!{ + ((0b1111 << 3) & ((#vvvv) << 7)) });
                let expr_l = self.l.to_expr().map(|l| quote!{ + ((0b1 << 2) + 
                ((#l) << 2))});
                let expr_pp = self.pp.to_expr().map(|pp| quote!{ + (0b11 & (#pp)) });

                syn::parse2(quote!{
                    {
                        const VEX2B_B2:u8 = #vex2_constbyte2;
                        VEX2B_B2 #expr_r #expr_vvvv #expr_l #expr_pp
                    }
                })
            } else {
                panic!("Tried to access byte {nth} from 2 byte VEX")
            }
        }

        fn len(&self) -> u8 { 2 }
    }
}

fn parse_prefixes(input: &ParseBuffer) -> Result<Vec<Box<dyn InsField>>> {
    let mut prefixes: Vec<Box<dyn InsField>> = vec!();
    
    while input.peek(Ident) {
        if input.parse::<keyword::REX>().is_ok() {
            prefixes.push(Box::new(rex::Rex::parse(input)?))
        } else if input.parse::<keyword::VEX2b>().is_ok() {
            prefixes.push(Box::new(vex::Vex2b::parse(input)?))
        } else if input.parse::<keyword::VEX3b>().is_ok() {
            prefixes.push(Box::new(vex::Vex3b::parse(input)?))
        } else {
            return Err(input.error("Unknown prefix"))
        }
    }
    Ok(prefixes)
}

impl Parse for Instruction {
    fn parse(input: &ParseBuffer) -> Result<Self> {
        let mut prefixes = vec![];
        let opcodes = if input.peek(syn::token::Bracket) {
            let mut opcodes = vec![];
            let content;
            bracketed!(content in input);

            if content.peek(Ident) {
                prefixes = parse_prefixes(&content)?;
                content.parse::<Token![,]>()?;
            }

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
            parse_var_fields(input)?
        } else {
            vec![]
        };

        let ins_fields = if input.parse::<Token![=>]>().is_ok() {
            parse_ins_fields(input)?
        } else {
            let _ = input.parse::<Token![;]>();
            vec![]
        };

        Ok(Instruction {
            name,
            prefixes,
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
             prefixes,
             opcodes,
             var_fields,
             ins_fields,
         }| {
            let var_field_names = var_fields.iter().map(|(name, _)| name);
            let var_field_names_1 = var_fields.iter().map(|(name, _)| name);
            let var_field_types = var_fields.iter().map(|(_, ty)| ty);

            let ins_len =
                prefixes.iter().map(|p| p.len()).sum::<u8>() as u32 + opcodes.len() as u32 + ins_fields.iter().map(|f| f.len()).sum::<u8>() as u32;
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
                        let Self(#(#var_field_names_1),*) = self;
                        #label_field
                    }
                };
                code
            });

            let mut ins_field_bytes = vec![];
            for field in ins_fields {
                let field_len = field.len();

                for i in 0..field_len {
                    match field.byte(i) {
                        Ok(byte) => ins_field_bytes.push(byte),
                        Err(e) => return e.into_compile_error(),
                    }
                }
            }

            let mut prefix_bytes = vec![];
            for field in prefixes {
                let field_len = field.len();

                for i in 0..field_len {
                    match field.byte(i) {
                        Ok(byte) => prefix_bytes.push(byte),
                        Err(e) => return e.into_compile_error(),
                    }
                }
            }

            quote! {
                struct #name(#(#var_field_types),*);

                impl Encodable for #name {
                    fn encode(&self) -> Vec<u8> {
                        let Self(#(#var_field_names),*) = self;
                        #(#ins_field_setups)*

                        vec![#(#prefix_bytes,)* #(#opcodes,)* #(#ins_field_bytes),*]
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
