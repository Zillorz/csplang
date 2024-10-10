#![allow(unused)]

// A hand written, zero-copy, fast parser for the APCSP programming language
// error handling and more advanced features can be found in 'structured_parser.rs'

// Tokenizing the APCSP language

use std::borrow::Cow;
use std::fmt::{Display, Formatter};

use thiserror::Error;

use crate::inpret::setup;

// Identifier -> A alphanumeric_ string, cannot start with number
// Flow -> any of '()[]{}'
// Seperator -> ',' or '\n'
// Assign '<-'
// Operator -> any of '=' '!=' '>' '>=' '<=' '<' 'MOD' 'NOT' 'AND' 'OR'
// Reserved keywords
// 'IF', 'ELSE', 'REPEAT', 'TIMES', 'UNTIL', 'FOR', 'EACH', 'IN', 'PROCEDURE'
// String -> any expression between two quotes, no escape characters
// Number -> any f64
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Token<'a> {
    Ident(&'a str),
    Flow(char),
    ArgSep,
    LineSep,
    Operator(Operator),
    Assign,
    Boolean(bool),
    String(&'a str),
    Number(i64),
    If,
    Else,
    Repeat,
    Times,
    Until,
    For,
    Each,
    In,
    Procedure,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Operator {
    Eq,
    Neq,
    Gt,
    Ge,
    Lt,
    Le,
    Mod,
    Not,
    And,
    Or,
    Add,
    Sub,
    Mul,
    Div,
}

impl<'a> From<Operator> for Token<'a> {
    fn from(value: Operator) -> Self {
        Token::Operator(value)
    }
}

#[repr(transparent)]
#[derive(Debug)]
struct Parser<'a>(&'a str);

impl<'a> Parser<'a> {
    fn len(&self) -> usize {
        self.0.len()
    }

    fn peek(&self) -> Option<char> {
        self.0.chars().next()
    }

    // Skips all \t and \s
    fn skip_space(&mut self) {
        while self.peek().is_some_and(|t| "\t ".contains(t)) {
            self.take();
        }
    }

    fn take(&mut self) -> Option<char> {
        let char = self.0.chars().next()?;
        self.0 = &self.0[char.len_utf8()..];
        Some(char)
    }

    fn matches(&mut self, m: &str) -> bool {
        let res = self
            .0
            .get(..m.len())
            .is_some_and(|c| c.eq_ignore_ascii_case(m));

        if res {
            self.0 = &self.0[m.len()..];
        }

        res
    }

    fn take_while(&mut self, cond: impl FnMut(&char) -> bool) -> &'a str {
        let len = self.0.chars().take_while(cond).map(|x| x.len_utf8()).sum();
        let (ret, new) = self.0.split_at(len);

        self.0 = new;
        ret
    }
}

macro_rules! mch {
    ($p:expr, $t:expr, $($string:expr => $token:expr),* ) => {
        $(
            let token: &str = $string;
            if (&mut $p).matches(token) {
                (&mut $t).push($token.into());
                continue;
            }
        )*
    };
}

pub fn tokenize<'a>(input: &'a str) -> Vec<Token<'a>> {
    let mut parser = Parser(input);
    let mut tokens = Vec::new();

    while {
        parser.skip_space();
        parser.len() > 0
    } {
        if parser.peek().is_some_and(|x| "(){}[]".contains(x)) {
            tokens.push(Token::Flow(parser.take().unwrap()));
            continue;
        }

        if parser.matches("/*") {
            let mut prev = '?';

            parser.take_while(|&c| {
                let cond = prev == '*' && c == '/';
                prev = c;
                !cond
            });

            parser.take();
            continue;
        } else if parser.matches("//") {
            parser.take_while(|&c| c != '\n');
            parser.take();
            continue;
        }

        mch!(parser, tokens,
            "," => Token::ArgSep,
            // smh, windows
            "\r\n" => Token::LineSep,
            "\n" => Token::LineSep,
            ";" => Token::LineSep,
            "<-" => Token::Assign,

            "=" => Operator::Eq,
            "!=" => Operator::Neq,
            // Ge and Le must be before
            // Gt and Lt because greedy match
            ">=" => Operator::Ge,
            "<=" => Operator::Le,
            ">" => Operator::Gt,
            "<" => Operator::Lt,
            "+" => Operator::Add,
            // "-" => Operator::Sub,
            "*" => Operator::Mul,
            "/" => Operator::Div
        );

        // try to parse a string
        if let Some('"') = parser.peek() {
            parser.take();

            let str = parser.take_while(|&x| x != '"');

            tokens.push(Token::String(str));
            parser.take();
            continue;
        } else if let Some('\'') = parser.peek() {
            parser.take();

            let str = parser.take_while(|&x| x != '\'');

            tokens.push(Token::String(str));
            parser.take();
            continue;
        }
        // try to parse a Identifier
        if parser
            .peek()
            .is_some_and(|x| !"0123456789.-\n\r{}[](),<>=!;'\"".contains(x))
        {
            let psuedo_iden = parser.take_while(|&x| !"\r\n{}[](),<>=!-.\t\"'; ".contains(x));

            let token = match psuedo_iden {
                "IF" => Token::If,
                "ELSE" => Token::Else,
                "REPEAT" => Token::Repeat,
                "TIMES" => Token::Times,
                "UNTIL" => Token::Until,
                "FOR" => Token::For,
                "EACH" => Token::Each,
                "IN" => Token::In,
                "PROCEDURE" => Token::Procedure,
                "TRUE" | "True" | "true" => Token::Boolean(true),
                "FALSE" | "False" | "false" => Token::Boolean(false),
                "MOD" => Operator::Mod.into(),
                "NOT" => Operator::Not.into(),
                "AND" => Operator::And.into(),
                "OR" => Operator::Or.into(),
                _ => Token::Ident(psuedo_iden),
            };
            tokens.push(token);

            continue;
        }

        let res = parser.take_while(|&x| "0123456789-".contains(x));
        if res == "-" {
            tokens.push(Token::Operator(Operator::Sub));
        } else if let Ok(f) = res.parse() {
            tokens.push(Token::Number(f));
        } else {
        }
    }

    tokens
}

type Ref<'a> = Box<Ast<'a>>;

// A much more powerful representation of tokens
#[derive(Clone, Debug, PartialEq)]
pub enum Ast<'a> {
    Assign(Ref<'a>, Ref<'a>),
    Identifier(&'a str),
    Unary(Operator, Ref<'a>),
    Operation(Operator, Ref<'a>, Ref<'a>),
    Block(Vec<Ast<'a>>),
    Literal(Value<'a>),
    ListLiteral(Vec<Ast<'a>>),
    If(Ref<'a>, Vec<Ast<'a>>, Option<Vec<Ast<'a>>>),
    Index(Ref<'a>, Ref<'a>),
    Repeat(Ref<'a>, Vec<Ast<'a>>),
    RepeatUntil(Ref<'a>, Vec<Ast<'a>>),
    For(&'a str, Ref<'a>, Vec<Ast<'a>>),
    Procedure(&'a str, Vec<&'a str>, Vec<Ast<'a>>),
    Call(&'a str, Vec<Ast<'a>>),
    Nop,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value<'a> {
    String(Cow<'a, str>),
    Number(i64),
    Boolean(bool),
    List(Vec<Value<'a>>),
    Undefined,
}

impl<'a> Display for Value<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::String(s) => f.write_str(s),
            Value::Number(s) => s.fmt(f),
            Value::Boolean(s) => s.fmt(f),
            Value::List(s) => {
                f.write_str("[")?;

                for i in 0..s.len() {
                    s[i].fmt(f)?;

                    if i != s.len() - 1 {
                        f.write_str(", ")?;
                    }
                }

                f.write_str("]")
            }
            _ => Err(std::fmt::Error),
        }
    }
}

#[derive(Error, Debug)]
pub enum AstError<'a> {
    #[error("Unexpected starting token: {0:?}")]
    StartingToken(&'a Token<'a>),
    #[error("Unexpected single token: {0:?}")]
    SingleToken(&'a Token<'a>),
    #[error("Unexpected basic token(s): {0:?}")]
    BasicToken(&'a [Token<'a>]),
    #[error("Expecting matching bracket '{0}'")]
    UnmatchedBracket(char),
    #[error("Unexpected function call brackets")]
    UnexpectedBrackets,
    #[error("Expected Token '{0:?}' found '{1:?}'")]
    NotFound(Token<'a>, Option<&'a Token<'a>>),
    #[error("Expected identifier")]
    NoIdentifier,
}

// macro to make error handling easier
#[macro_export]
macro_rules! bail {
    ($err:expr) => {
        return Err($err)
    };
}

macro_rules! bail_eq {
    ($value:expr, $token:expr) => {
        if ($value != Some(&$token)) {
            return Err(AstError::NotFound($token, $value));
        }
    };
}

pub fn create_ast<'a, 'b>(tokens: &'b [Token<'a>]) -> Result<Vec<Ast<'a>>, AstError<'b>> {
    let mut block = Vec::new();
    let mut iterator = PeekableSliceIterator::from(tokens);

    while let Some(token) = iterator.peek() {
        let ast = match token {
            Token::Ident(_) => parse_basic(&mut iterator, Token::LineSep),
            Token::If => parse_if(&mut iterator),
            Token::Repeat => parse_repeat(&mut iterator),
            Token::For => parse_for(&mut iterator),
            Token::Procedure => parse_procedure(&mut iterator),
            Token::LineSep => {
                iterator.next();
                continue;
            }
            token => return Err(AstError::StartingToken(token)),
        };
        block.push(ast?);
    }

    Ok(block)
}

type TokenIter<'a, 'b> = PeekableSliceIterator<'b, Token<'a>>;

// I love lifetimes I love lifetimes I love lifetimes I love lifetimes
// this function used to have a lifetime 'c aswell
fn split_any<'a, 'b, const N: usize>(
    slice: &'b [Token<'a>],
    break_at: [Token<'static>; N],
) -> Option<(&'b Token<'a>, &'b [Token<'a>], &'b [Token<'a>])> {
    // I hate my life 'r'
    let mut nest = 0;
    let pos = slice.iter().rposition(|x| {
        if x == &Token::Flow('(') {
            nest += 1;
        } else if x == &Token::Flow(')') {
            nest -= 1;
        }
        nest == 0 && break_at.contains(x)
    })?;

    Some((slice.get(pos)?, slice.get(..pos)?, slice.get(pos + 1..)?))
}

fn parse_basic_slice<'a, 'b>(mut slice: &'b [Token<'a>]) -> Result<Ast<'a>, AstError<'b>> {
    while slice.first() == Some(&Token::Flow('(')) {
        let mut nest = 0;
        let pos = slice.iter().position(|x| {
            if x == &Token::Flow('(') {
                nest += 1;
            } else if x == &Token::Flow(')') {
                nest -= 1;
            }
            nest == 0
        });

        if pos.is_some_and(|x| x == slice.len() - 1) {
            slice = &slice[1..slice.len() - 1];
        } else {
            break;
        }
    }

    if slice.len() == 0 {
        return Ok(Ast::Nop);
    }
    if slice.len() == 1 {
        return Ok(match slice[0] {
            Token::Ident(iden) => Ast::Identifier(iden),
            Token::Boolean(b) => Ast::Literal(Value::Boolean(b)),
            Token::String(s) => Ast::Literal(Value::String(s.into())),
            Token::Number(n) => Ast::Literal(Value::Number(n)),
            token => return Err(AstError::SingleToken(&slice[0])),
        });
    }

    // support nested comma
    if slice[0] == Token::Flow('[') {
        // make array literal here

        let mut nest = 0;

        let ags: Result<Vec<Ast<'a>>, AstError<'b>> = slice[1..]
            .split(|&x| {
                if x == Token::Flow('(') || x == Token::Flow('[') {
                    nest += 1;
                } else if x == Token::Flow(')') || x == Token::Flow(']') {
                    nest -= 1;
                }

                nest <= 0 && x == Token::ArgSep
            })
            .map(|x| parse_basic_slice(x.strip_suffix(&[Token::Flow(']')]).unwrap_or(x)))
            .collect();

        return Ok(Ast::ListLiteral(ags?));
    }

    // Order of operations (lowest to highest)
    // Assign
    // And
    // Or
    // [Eq
    //  Neq
    //  Gt
    //  Ge
    //  Lt
    //  Le]
    //  [Add, Sub]
    //  [Div, Mul, Mod]
    //  Not
    //  Index
    //  Function Call

    if let Some((Token::Assign, i, v)) = split_any(slice, [Token::Assign]) {
        Ok(Ast::Assign(
            Box::new(parse_basic_slice(i)?),
            Box::new(parse_basic_slice(v)?),
        ))
    } else if let Some((Token::Operator(Operator::And), o, t)) =
        split_any(slice, [Token::Operator(Operator::And)])
    {
        Ok(Ast::Operation(
            Operator::And,
            Box::new(parse_basic_slice(o)?),
            Box::new(parse_basic_slice(t)?),
        ))
    } else if let Some((Token::Operator(Operator::Or), o, t)) =
        split_any(slice, [Token::Operator(Operator::Or)])
    {
        Ok(Ast::Operation(
            Operator::Or,
            Box::new(parse_basic_slice(o)?),
            Box::new(parse_basic_slice(t)?),
        ))
    } else if let Some((Token::Operator(operator), a, b)) = split_any(
        slice,
        [
            Token::Operator(Operator::Eq),
            Token::Operator(Operator::Neq),
            Token::Operator(Operator::Gt),
            Token::Operator(Operator::Ge),
            Token::Operator(Operator::Lt),
            Token::Operator(Operator::Le),
        ],
    ) {
        Ok(Ast::Operation(
            *operator,
            Box::new(parse_basic_slice(a)?),
            Box::new(parse_basic_slice(b)?),
        ))
    } else if let Some((Token::Operator(operator), a, b)) = split_any(
        slice,
        [
            Token::Operator(Operator::Add),
            Token::Operator(Operator::Sub),
        ],
    ) {
        Ok(Ast::Operation(
            *operator,
            Box::new(parse_basic_slice(a)?),
            Box::new(parse_basic_slice(b)?),
        ))
    } else if let Some((Token::Operator(operator), a, b)) = split_any(
        slice,
        [
            Token::Operator(Operator::Div),
            Token::Operator(Operator::Mul),
            Token::Operator(Operator::Mod),
        ],
    ) {
        Ok(Ast::Operation(
            *operator,
            Box::new(parse_basic_slice(a)?),
            Box::new(parse_basic_slice(b)?),
        ))
    } else if slice[0] == Token::Operator(Operator::Not) {
        Ok(Ast::Unary(
            Operator::Not,
            Box::new(parse_basic_slice(&slice[1..])?),
        ))
    } else if slice[1] == Token::Flow('(') {
        let Token::Ident(iden) = slice[0] else {
            bail!(AstError::UnexpectedBrackets)
        };

        let mut nest = 0;

        let ags: Result<Vec<Ast<'a>>, AstError<'b>> = slice[2..]
            .split(|&x| {
                if x == Token::Flow('(') || x == Token::Flow('[') {
                    nest += 1;
                } else if x == Token::Flow(')') || x == Token::Flow(']') {
                    nest -= 1;
                }

                nest <= 0 && x == Token::ArgSep
            })
            .map(|x| parse_basic_slice(x.strip_suffix(&[Token::Flow(')')]).unwrap_or(x)))
            .collect();

        Ok(Ast::Call(iden, ags?))
    } else if slice.contains(&Token::Flow('[')) {
        let mut oidx = 0;
        let mut cidx = 0;

        let mut stack = 0;

        // Regular for loop algorithm 😲
        for i in (0..slice.len()).rev() {
            if slice[i] == Token::Flow(']') {
                if cidx == 0 {
                    cidx = i;
                } else {
                    stack += 1;
                }
            } else if slice[i] == Token::Flow('[') {
                if stack == 0 {
                    oidx = i;
                    break;
                } else {
                    stack -= 1;
                }
            }
        }

        if oidx == 0 || cidx == 0 {
            bail!(AstError::UnmatchedBracket(']'))
        }

        Ok(Ast::Index(
            Box::new(parse_basic_slice(&slice[0..oidx])?),
            Box::new(parse_basic_slice(&slice[oidx + 1..cidx])?),
        ))
    } else {
        bail!(AstError::BasicToken(slice))
    }
}

fn parse_basic<'a, 'b, 'c: 'b + 'a>(
    iterator: &mut TokenIter<'a, 'b>,
    end_token: Token<'c>,
) -> Result<Ast<'a>, AstError<'b>> {
    let tks = iterator
        .as_slice()
        .split(|&x| x == end_token)
        .next()
        .ok_or(AstError::NotFound(end_token, None))?;

    let ast = parse_basic_slice(tks);
    iterator.consume_until(&end_token);

    ast
}

fn parse_block<'a, 'b>(iterator: &mut TokenIter<'a, 'b>) -> Result<Vec<Ast<'a>>, AstError<'b>> {
    iterator.consume_until_not(&Token::LineSep);
    bail_eq!(iterator.next(), Token::Flow('{'));

    let mut nest = 1;

    let tks = iterator
        .as_slice()
        .split(|&x| {
            if x == Token::Flow('{') {
                nest += 1
            } else if x == Token::Flow('}') {
                nest -= 1;
            }

            nest <= 0 && x == Token::Flow('}')
        })
        .next()
        .ok_or(AstError::UnmatchedBracket('}'))?;

    let ast = create_ast(tks);

    iterator.nth(tks.len());
    iterator.consume_until_not(&Token::LineSep);

    ast
}

fn parse_cond<'a, 'b>(iterator: &mut TokenIter<'a, 'b>) -> Result<Ast<'a>, AstError<'b>> {
    bail_eq!(iterator.next(), Token::Flow('('));

    let mut nest = 1;
    let tks = iterator
        .as_slice()
        .split(|&x| {
            if x == Token::Flow('(') {
                nest += 1
            } else if x == Token::Flow(')') {
                nest -= 1;
            }

            nest <= 0 && x == Token::Flow(')')
        })
        .next()
        .ok_or(AstError::UnmatchedBracket(')'))?;

    let ast = parse_basic_slice(&tks);
    iterator.nth(tks.len());

    ast
}

fn parse_if<'a, 'b>(iterator: &mut TokenIter<'a, 'b>) -> Result<Ast<'a>, AstError<'b>> {
    bail_eq!(iterator.next(), Token::If);
    let cond = Box::new(parse_cond(iterator)?);
    let body = parse_block(iterator)?;

    let mut else_block = None;
    if iterator.peek().is_some_and(|&t| t == Token::Else) {
        iterator.next();
        else_block = Some(parse_block(iterator)?);
    }

    Ok(Ast::If(cond, body, else_block))
}

fn parse_repeat<'a, 'b>(iterator: &mut TokenIter<'a, 'b>) -> Result<Ast<'a>, AstError<'b>> {
    bail_eq!(iterator.next(), Token::Repeat);

    if iterator.peek().is_some_and(|&t| t == Token::Until) {
        iterator.next();
        let cond = Box::new(parse_cond(iterator)?);
        let body = parse_block(iterator)?;
        Ok(Ast::RepeatUntil(cond, body))
    } else {
        let times = Box::new(parse_basic(iterator, Token::Times)?);
        let body = parse_block(iterator)?;
        Ok(Ast::Repeat(times, body))
    }
}

fn parse_for<'a, 'b>(iterator: &mut TokenIter<'a, 'b>) -> Result<Ast<'a>, AstError<'b>> {
    bail_eq!(iterator.next(), Token::For);
    bail_eq!(iterator.next(), Token::Each);

    let Some(Token::Ident(item)) = iterator.next() else {
        panic!("Malformed for loop")
    };

    bail_eq!(iterator.next(), Token::In);
    let list = parse_basic(iterator, Token::Flow('{'))?;
    iterator.back();

    let body = parse_block(iterator)?;

    Ok(Ast::For(item, Box::new(list), body))
}

fn parse_procedure<'a, 'b>(iterator: &mut TokenIter<'a, 'b>) -> Result<Ast<'a>, AstError<'b>> {
    bail_eq!(iterator.next(), Token::Procedure);

    let Some(Token::Ident(name)) = iterator.next() else {
        bail!(AstError::NoIdentifier)
    };

    bail_eq!(iterator.next(), Token::Flow('('));
    let tks = iterator
        .as_slice()
        .split(|&x| x == Token::Flow(')'))
        .next()
        .ok_or(AstError::UnmatchedBracket(')'))?;

    let args = tks
        .iter()
        .filter_map(|x| {
            if let Token::Ident(id) = x {
                Some(*id)
            } else {
                None
            }
        })
        .collect();

    iterator.consume_until(&Token::Flow(')'));
    let block = parse_block(iterator)?;

    Ok(Ast::Procedure(name, args, block))
}

struct PeekableSliceIterator<'a, T> {
    slice: &'a [T],
    index: usize,
}

impl<'a, T> PeekableSliceIterator<'a, T> {
    fn as_slice(&self) -> &'a [T] {
        self.slice.get(self.index..).unwrap_or(&[])
    }

    fn back(&mut self) {
        self.index -= 1;
    }

    fn peek(&self) -> Option<&'a T> {
        self.slice.get(self.index)
    }
}

impl<'a, T: PartialEq> PeekableSliceIterator<'a, T> {
    // inclusive
    fn consume_until<'b>(&mut self, value: &'b T) {
        while self.next().is_some_and(|n| n != value) {}
    }

    // exclusive
    fn consume_until_not<'b>(&mut self, value: &'b T) {
        while self.peek().is_some_and(|n| n == value) {
            self.next();
        }
    }
}

impl<'a, T> From<&'a [T]> for PeekableSliceIterator<'a, T> {
    fn from(slice: &'a [T]) -> Self {
        Self { slice, index: 0 }
    }
}

impl<'a, T> Iterator for PeekableSliceIterator<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = self.slice.get(self.index);
        self.index += 1;
        ret
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let r = self.slice.len().saturating_sub(self.index);
        (r, Some(r))
    }
}

impl<'a, T> ExactSizeIterator for PeekableSliceIterator<'a, T> {}
