use crate::bail;
use crate::parser::{Ast, Operator, Value};
use rand::{thread_rng, Rng};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum RuntimeError<'a> {
    #[error("Variable '{0}' does not exist")]
    NotFound(&'a str),
    #[error("Function '{0}' does not exist")]
    NotFoundFunction(&'a str),
    #[error("Cannot override function '{0}'")]
    Override(&'a str),
    #[error("Function '{0}' did not return value")]
    NoValueReturned(&'a str),
    #[error("IF statement expected boolean.")]
    MalformedIf,
    #[error("REPEAT .. TIMES statement expected number.")]
    MalformedRepeat,
    #[error("REPEAT UNTIL statement expected boolean.")]
    MalformedRepeatUntil,
    #[error("FOR EACH statement expected list.")]
    MalformedFor,
    #[error("Unsupported operation '{0:?}' on types '{1:?}' and '{2:?}'")]
    BadOperation(Operator, Type, Type),
    #[error("Unsupported operation '{0:?}' on type '{1:?}'")]
    BadUnary(Operator, Type),
    #[error("Cannot index into type other than List")]
    BadArray,
    #[error("Cannot index with type other than Number")]
    BadIndex,
    #[error("Passed {0} arguments into {1}, expected {2} arguments")]
    TooFewArguments(usize, &'a str, usize),
    #[error("Instruction did not flatten")]
    NothingToFlatten,
    #[error("IO Error: {0}")]
    IoError(#[from] std::io::Error),

    // Only built-in functions have typed args
    #[error("Function {0} expected argument {1} to be type {2:?}")]
    TypedArg(&'a str, usize, Type),
    #[error("Cannot find length of type {0:?}")]
    NoLength(Type),

    #[error("Cannot parse value")]
    Unparsable,

    #[error("Explicit panic (csp)")]
    FunctionPanic,

    #[error("Index {0} out of bounds for list with size {1}")]
    IndexOutOfBounds(i64, usize),
    #[error("Index 0 does not exist (this is a 1-based-indexing language)")]
    IndexZero,
}

#[derive(Debug)]
pub enum Type {
    List,
    Boolean,
    String,
    Number,
    Any,
}

impl<'a, 'b> From<&'b Value<'a>> for Type {
    fn from(value: &'b Value<'a>) -> Self {
        match value {
            Value::List(_) => Type::List,
            Value::String(_) => Type::String,
            Value::Boolean(_) => Type::Boolean,
            Value::Number(_) => Type::Number,
            Value::Undefined => panic!(),
        }
    }
}

impl<'a, 'b> From<&'b mut Value<'a>> for Type {
    fn from(value: &'b mut Value<'a>) -> Self {
        match value {
            Value::List(_) => Type::List,
            Value::String(_) => Type::String,
            Value::Boolean(_) => Type::Boolean,
            Value::Number(_) => Type::Number,
            Value::Undefined => panic!(),
        }
    }
}

impl<'a> From<Value<'a>> for Type {
    fn from(value: Value<'a>) -> Self {
        Type::from(&value)
    }
}

#[derive(Copy, Clone, Eq)]
struct IgnoreCaseStr<'a>(&'a str);

impl<'a> From<&'a str> for IgnoreCaseStr<'a> {
    fn from(value: &'a str) -> Self {
        IgnoreCaseStr(value)
    }
}

impl<'a> PartialEq for IgnoreCaseStr<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq_ignore_ascii_case(other.0)
    }
}

impl<'a> Hash for IgnoreCaseStr<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        if self.0.len() < 14 {
            self.0
                .chars()
                .map(|x| x.to_ascii_lowercase())
                .for_each(|x| {
                    x.hash(state);
                });
        } else {
            self.0.to_ascii_lowercase().hash(state);
        }

        state.finish();
    }
}

struct Environment<'a> {
    variables: HashMap<IgnoreCaseStr<'a>, Value<'a>>,
    functions: HashMap<IgnoreCaseStr<'a>, Function<'a>>,

    flag: bool,
    ret: Value<'a>,
}

#[derive(Clone)]
enum Function<'a> {
    NativeFunction(
        fn(Vec<Ast<'a>>, &mut Environment<'a>) -> Result<Option<Value<'a>>, RuntimeError<'a>>,
    ),
    Function(Vec<&'a str>, Vec<Ast<'a>>),
}

fn run<'a, 'b>(ins: &'b mut [Ast<'a>], env: &mut Environment<'a>) -> Result<(), RuntimeError<'a>> {
    for i in ins {
        if env.flag {
            return Ok(());
        }

        match i {
            Ast::Assign(assignment, value) => {
                let set = flatten(value, env)?.clone();

                let reg = match flatten(assignment, env) {
                    Ok(v) => v,
                    Err(RuntimeError::NotFound(iden)) => {
                        let iden = IgnoreCaseStr(iden);
                        env.variables.insert(iden, Value::Undefined);
                        env.variables.get_mut(&iden).unwrap()
                    }
                    Err(e) => bail!(e),
                };

                *reg = set;
            }
            Ast::If(cond, if_branch, else_branch) => {
                let Value::Boolean(bool) = flatten(cond, env)? else {
                    bail!(RuntimeError::MalformedIf)
                };

                if *bool {
                    run(if_branch, env)?;
                } else if let Some(else_branch) = else_branch {
                    run(else_branch, env)?;
                }
            }
            Ast::Repeat(times, ins) => {
                let Value::Number(times) = *flatten(times, env)? else {
                    bail!(RuntimeError::MalformedRepeat)
                };

                for _ in 0..times {
                    let mut cl = ins.clone();
                    run(&mut cl, env)?;
                }
            }
            Ast::RepeatUntil(cond, ins) => loop {
                let Value::Boolean(cont) = *flatten(cond, env)? else {
                    bail!(RuntimeError::MalformedRepeatUntil)
                };

                if cont {
                    break;
                }
                run(ins, env)?;
            },
            Ast::For(variable, list, ins) => {
                let var = IgnoreCaseStr(variable);
                let old_var = env.variables.remove_entry(&var);

                let Value::List(list) = flatten(list, env)?.clone() else {
                    bail!(RuntimeError::MalformedFor)
                };

                for i in list {
                    env.variables.insert(var, i);
                    run(ins, env)?;
                }

                if let Some(old) = old_var {
                    env.variables.insert(var, old.1);
                } else {
                    env.variables.remove(&var);
                }
            }
            Ast::Call(..) => {
                let res = flatten(i, env);

                if let Err(error) = res {
                    if !matches!(error, RuntimeError::NoValueReturned(_)) {
                        bail!(error)
                    }
                }
            }
            // Procedures cannot be executed, they are function definitions and should be handled in the flatten method
            _ => {}
        }
    }

    Ok(())
}

pub fn setup<'a>(mut ins: Vec<Ast<'a>>) -> Result<(), RuntimeError<'a>> {
    let variables = HashMap::new();
    let functions = HashMap::new();

    let mut env = Environment {
        variables,
        functions,

        flag: false,
        ret: Value::Undefined,
    };
    add_native_functions(&mut env.functions);

    // add user defined functions
    for i in &mut ins {
        if let Ast::Procedure(name, args, ins) = i {
            let name = IgnoreCaseStr(name);

            if !env.functions.contains_key(&name) {
                env.functions
                    .insert(name, Function::Function(args.clone(), ins.clone()));
            } else {
                bail!(RuntimeError::Override(name.0));
            }
        }
    }

    run(&mut ins, &mut env)
}

fn flatten<'a, 'b>(
    exp: &'b mut Ast<'a>,
    env: &'b mut Environment<'a>,
) -> Result<&'b mut Value<'a>, RuntimeError<'a>> {
    match exp {
        Ast::Identifier(iden) => {
            let iden = IgnoreCaseStr(iden);

            env.variables
                .get_mut(&iden)
                .ok_or(RuntimeError::NotFound(iden.0))
        }
        Ast::Operation(op, a, b) => {
            let a = flatten(a, env)?.clone();
            let b = flatten(b, env)?.clone();

            match (a, b) {
                (Value::Number(a), Value::Number(b)) => {
                    let res = match op {
                        // int operators
                        Operator::Add => Value::Number(a + b),
                        Operator::Sub => Value::Number(a - b),
                        Operator::Mul => Value::Number(a * b),
                        Operator::Div => Value::Number(a / b),
                        Operator::Mod => Value::Number(a % b),
                        Operator::Eq => Value::Boolean(a == b),
                        Operator::Neq => Value::Boolean(a != b),
                        Operator::Gt => Value::Boolean(a > b),
                        Operator::Lt => Value::Boolean(a < b),
                        Operator::Ge => Value::Boolean(a >= b),
                        Operator::Le => Value::Boolean(a <= b),
                        _ => {
                            bail!(RuntimeError::BadOperation(*op, Type::Number, Type::Number))
                        }
                    };

                    env.ret = res;
                    Ok(&mut env.ret)
                }
                (Value::Boolean(a), Value::Boolean(b)) => {
                    let res = match op {
                        Operator::And => Value::Boolean(a && b),
                        Operator::Or => Value::Boolean(a || b),
                        Operator::Eq => Value::Boolean(a == b),
                        Operator::Neq => Value::Boolean(a != b),
                        _ => {
                            bail!(RuntimeError::BadOperation(
                                *op,
                                Type::Boolean,
                                Type::Boolean
                            ))
                        }
                    };

                    env.ret = res;
                    Ok(&mut env.ret)
                }
                (Value::String(a), Value::String(b)) => {
                    let bool = match op {
                        Operator::Eq => a == b,
                        Operator::Neq => a != b,
                        _ => {
                            bail!(RuntimeError::BadOperation(*op, Type::String, Type::String))
                        }
                    };

                    env.ret = Value::Boolean(bool);
                    Ok(&mut env.ret)
                }
                (Value::List(a), Value::List(b)) => {
                    let bool = match op {
                        Operator::Eq => a == b,
                        Operator::Neq => a != b,
                        _ => {
                            bail!(RuntimeError::BadOperation(*op, Type::List, Type::List))
                        }
                    };

                    env.ret = Value::Boolean(bool);
                    Ok(&mut env.ret)
                }
                (a, b) => {
                    if *op == Operator::Eq {
                        env.ret = Value::Boolean(false);
                        Ok(&mut env.ret)
                    } else if *op == Operator::Neq {
                        env.ret = Value::Boolean(true);
                        Ok(&mut env.ret)
                    } else {
                        bail!(RuntimeError::BadOperation(*op, a.into(), b.into()))
                    }
                }
            }
        }
        Ast::Unary(op, val) => {
            let mut val = flatten(val.as_mut(), env).unwrap();

            match (op, &mut val) {
                (Operator::Not, Value::Boolean(b)) => {
                    *b = !*b;
                }
                (Operator::Not, Value::Number(n)) => {
                    *n = !*n;
                }
                (op, val) => bail!(RuntimeError::BadUnary(*op, (*val).into())),
            }

            Ok(val)
        }
        Ast::Literal(literal) => Ok(literal),
        Ast::ListLiteral(literal) => {
            let mut values = Vec::with_capacity(literal.capacity());

            for li in literal {
                if matches!(li, Ast::Nop) {
                    continue;
                }

                let li = flatten(li, env)?;
                values.push(li.clone());
            }

            env.ret = Value::List(values);
            Ok(&mut env.ret)
        }
        Ast::Index(li, idx) => {
            let idx = flatten(idx, env)?;
            let Value::Number(idx) = *idx else {
                bail!(RuntimeError::BadIndex);
            };

            let reg = flatten(li, env)?;
            let Value::List(list) = reg else {
                bail!(RuntimeError::BadArray)
            };

            let idx = calc_index(idx, list.len())?;
            Ok(&mut list[idx])
        }
        Ast::Call(func_n, value) => {
            let func = IgnoreCaseStr(func_n);
            let func = env
                .functions
                .get_mut(&func)
                .ok_or(RuntimeError::NotFoundFunction(func_n))?
                .clone();

            let ret = match func {
                Function::NativeFunction(f) => f(value.clone(), env)?,
                Function::Function(args, mut ins) => {
                    let mut s_var = HashMap::new();

                    if args.len() == 1 && args[0] == "vargs" {
                        let list: Result<Vec<Value>, RuntimeError<'a>> =
                            value.iter_mut().map(|v| flatten(v, env).cloned()).collect();

                        s_var.insert("vargs".into(), Value::List(list?));
                    } else {
                        if args.len() > value.len() {
                            bail!(RuntimeError::TooFewArguments(
                                value.len(),
                                func_n,
                                args.len()
                            ))
                        }

                        for (n, v) in args.iter().cloned().zip(value.iter_mut()) {
                            s_var.insert(n.into(), flatten(v, env)?.clone());
                        }
                    }

                    let mut dyn_funcs = vec![];
                    for i in &mut ins {
                        if let Ast::Procedure(name, args, ins) = i {
                            let name = IgnoreCaseStr(name);

                            if !env.functions.contains_key(&name) {
                                dyn_funcs.push(name);
                                env.functions
                                    .insert(name, Function::Function(args.clone(), ins.clone()));
                            } else {
                                bail!(RuntimeError::Override(name.0))
                            }
                        }
                    }

                    std::mem::swap(&mut s_var, &mut env.variables);
                    run(&mut ins, env)?;

                    for func in dyn_funcs {
                        env.functions.remove(&func);
                    }

                    env.variables = s_var;

                    if env.flag {
                        env.flag = false;
                        Some(env.ret.clone())
                    } else {
                        None
                    }
                }
            };

            if let Some(ret) = ret {
                env.ret = ret;
                Ok(&mut env.ret)
            } else {
                Err(RuntimeError::NoValueReturned(func_n))
            }
        }
        Ast::Nop => Err(RuntimeError::NoValueReturned("nop")),
        _ => Err(RuntimeError::NothingToFlatten),
    }
}

fn calc_index(idx: i64, len: usize) -> Result<usize, RuntimeError<'static>> {
    if idx == 0 {
        Err(RuntimeError::IndexZero)
    } else if idx.abs() as usize > len {
        Err(RuntimeError::IndexOutOfBounds(idx, len))
    } else if idx > 0 {
        Ok(idx as usize - 1)
    } else {
        Ok(len + idx as usize)
    }
}

// Macros are fun
macro_rules! count {
    () => {0usize};
    ($_head:tt $($tail:tt)*) => {1usize + count!($($tail)*)};
}

macro_rules! parse_args {
    ($f_name:expr, $env:expr, $it:expr, $($pr:pat, $type:expr),*) => {
        let mut iter = $it.into_iter();

        let count = count!($($pr)*);
        let mut idx = 0;

        $(
            let Some(mut arg) = iter.next() else {
                bail!(
                    RuntimeError::TooFewArguments(idx, $f_name, count)
                );
            };

            #[allow(irrefutable_let_patterns)]
            let $pr = flatten(&mut arg, $env)?.clone() else {
                bail!(
                    RuntimeError::TypedArg($f_name, idx, $type)
                );
            };


            #[allow(unused_assignments)]
            {
                idx += 1;
            }
        )*
    }
}

macro_rules! parse_list_args {
    ($f_name:expr, $env:expr, $it:expr, $list:ident, $($pr:pat, $type:expr),*) => {
        let mut iter = $it.iter_mut();

        let count = count!($($pr)*) + 1;
        let mut idx = 1;

        let Some(list_ast) = iter.next() else {
            bail!(
                RuntimeError::TooFewArguments(0, $f_name, count)
            );
        };

        $(
            let Some(mut arg) = iter.next() else {
                bail!(
                    RuntimeError::TooFewArguments(idx, $f_name, count)
                );
            };

            #[allow(irrefutable_let_patterns)]
            let $pr = flatten(&mut arg, $env)?.clone() else {
                bail!(
                    RuntimeError::TypedArg($f_name, idx, $type)
                );
            };

            #[allow(unused_assignments)]
            {
                idx += 1;
            }
        )*

        let Value::List($list) = flatten(list_ast, $env)? else {
            bail!(
                RuntimeError::TypedArg($f_name, 0, Type::List)
            );
        };
    }
}

fn add_native_functions(functions: &mut HashMap<IgnoreCaseStr, Function>) {
    //paramters after first are custom and not default
    functions.insert(
        "DISPLAY".into(),
        Function::NativeFunction(|args, env| {
            for mut arg in args.into_iter() {
                print!("{}", flatten(&mut arg, env)?);
            }
            println!();
            Ok(None)
        }),
    );

    functions.insert(
        "INPUT".into(),
        Function::NativeFunction(|_, _| {
            let mut string = String::new();
            std::io::stdin().read_line(&mut string)?;
            string = string.trim().to_string();

            Ok(Some(Value::String(string.into())))
        }),
    );

    functions.insert(
        "RANDOM".into(),
        Function::NativeFunction(|args, env| {
            parse_args!(
                "RANDOM",
                env,
                args,
                Value::Number(low),
                Type::Number,
                Value::Number(high),
                Type::Number
            );

            Ok(Some(Value::Number(thread_rng().gen_range(low..=high))))
        }),
    );

    functions.insert(
        "INSERT".into(),
        Function::NativeFunction(|mut args, env| {
            parse_list_args!(
                "INSERT",
                env,
                args,
                list,
                Value::Number(index),
                Type::Number,
                value,
                Type::Any
            );

            list.insert(calc_index(index, list.len())?, value);
            Ok(None)
        }),
    );

    functions.insert(
        "APPEND".into(),
        Function::NativeFunction(|mut args, env| {
            parse_list_args!("APPEND", env, args, list, value, Type::Any);

            list.push(value);
            Ok(None)
        }),
    );

    functions.insert(
        "REMOVE".into(),
        Function::NativeFunction(|mut args, env| {
            parse_list_args!(
                "REMOVE",
                env,
                args,
                list,
                Value::Number(index),
                Type::Number
            );

            list.remove(calc_index(index, list.len())?);
            Ok(None)
        }),
    );

    let len = Function::NativeFunction(|mut args, env| {
        let mut args = args.iter_mut();
        let Some(ast) = args.next() else {
            bail!(RuntimeError::TooFewArguments(0, "LEN / LENGTH", 1));
        };

        let value = flatten(ast, env)?;
        if let Value::List(list) = value {
            Ok(Some(Value::Number(list.len() as i64)))
        } else if let Value::String(string) = value {
            Ok(Some(Value::Number(string.len() as i64)))
        } else {
            bail!(RuntimeError::NoLength(value.clone().into()))
        }
    });
    functions.insert("LENGTH".into(), len.clone());
    functions.insert("LEN".into(), len);

    functions.insert(
        "SUBSTRING".into(),
        Function::NativeFunction(|args, env| {
            parse_args!(
                "SUBSTRING",
                env,
                args,
                Value::String(string),
                Type::String,
                Value::Number(start),
                Type::Number,
                Value::Number(length),
                Type::Number
            );

            let ret = string
                .get(start as usize - 1..(start + length) as usize - 1)
                .ok_or(RuntimeError::IndexOutOfBounds(start + length, string.len()))?
                .to_owned()
                .into();

            Ok(Some(Value::String(ret)))
        }),
    );

    functions.insert(
        "CONCAT".into(),
        Function::NativeFunction(|args, env| {
            parse_args!(
                "CONCAT",
                env,
                args,
                Value::String(string),
                Type::String,
                value,
                Type::Any
            );

            let ret = string.to_string() + &value.to_string();
            Ok(Some(Value::String(ret.into())))
        }),
    );

    functions.insert(
        "REVERSE".into(),
        Function::NativeFunction(|args, env| {
            parse_args!("REVERSE", env, args, Value::String(string), Type::String);

            let rev = string.chars().rev().collect();
            Ok(Some(Value::String(rev)))
        }),
    );

    functions.insert(
        "PARSE".into(),
        Function::NativeFunction(|args, env| {
            parse_args!("PARSE", env, args, Value::String(val), Type::String);

            if val.eq_ignore_ascii_case("true") {
                Ok(Some(Value::Boolean(true)))
            } else if val.eq_ignore_ascii_case("false") {
                Ok(Some(Value::Boolean(false)))
            } else if let Ok(number) = val.parse() {
                Ok(Some(Value::Number(number)))
            } else {
                Err(RuntimeError::Unparsable)
            }
        }),
    );

    // Special Type ID function, helps implement advanced functionality
    // 0 -> Undefined
    // 1 -> String
    // 2 -> int
    // 3 -> bool
    // 4 -> list
    functions.insert(
        "TYPEID".into(),
        Function::NativeFunction(|args, env| {
            parse_args!("TYPEID", env, args, value, Type::Any);

            let id = match value {
                Value::String(_) => 1,
                Value::Number(_) => 2,
                Value::Boolean(_) => 3,
                Value::List(_) => 4,
                Value::Undefined => 0,
            };

            Ok(Some(Value::Number(id)))
        }),
    );

    // Hacky functions, allow CSP code to access elements of runtime
    functions.insert(
        "CREATE_UNDEFINED".into(),
        Function::NativeFunction(|_, _| Ok(Some(Value::Undefined))),
    );

    functions.insert(
        "PANIC".into(),
        Function::NativeFunction(|_, _| Err(RuntimeError::FunctionPanic)),
    );

    functions.insert(
        "CATCH_ERROR".into(),
        Function::NativeFunction(|args, env| {
            let mut iter = args.into_iter();
            let Some(function_identifier) = iter.next() else {
                bail!(RuntimeError::TooFewArguments(0, "CATCH_ERROR", 1));
            };

            let Ast::Identifier(name) = function_identifier else {
                bail!(RuntimeError::TypedArg("CATCH_ERROR", 0, Type::Any));
            };
            let mut func = Ast::Call(name, vec![]);

            let res = flatten(&mut func, env);
            let is_err = res.is_err_and(|x| !matches!(x, RuntimeError::NoValueReturned(_)));

            Ok(Some(Value::Boolean(is_err)))
        }),
    );

    // Special function, pass flag to stop execution
    functions.insert(
        "RETURN".into(),
        Function::NativeFunction(|mut args, env| {
            let mut args = args.iter_mut();
            let value = flatten(args.next().unwrap(), env).unwrap();

            env.ret = value.clone();
            env.flag = true;
            Ok(None)
        }),
    );
}
