use std::fmt;
use std::collections::HashMap;
use std::sync::atomic::{AtomicIsize, ATOMIC_ISIZE_INIT, Ordering};

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Const(TypeConst),
    App(Box<Type>, Vec<Type>),
    Arrow(Vec<Type>, Box<Type>),
    Var(Box<TypeVar>)
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeVar {
    Unbound(i32, i32),
    Linked(Type),
    Generic(i32)
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum TypeConst {
    Int,
    Bool,
    String,
    Float,
    Unit,
    TypeDef(String)
}

impl fmt::Display for TypeConst {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            TypeConst::Int => write!(fmt, "int"),
            TypeConst::Bool => write!(fmt, "bool"),
            TypeConst::String => write!(fmt, "string"),
            TypeConst::Float => write!(fmt, "float"),
            TypeConst::Unit => write!(fmt, "unit"),
            TypeConst::TypeDef(ref s) => write!(fmt, "{}", s)
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let mut names = HashMap::new();
        let mut count = 0;
        fn new_name(count: &mut u8) -> String {
            let cur = *count;
            if cur == 255 {
                return "<more than 255 type variables >:(>".to_string();
            }
            *count += 1;
            let overflow = (cur / 25) as u8;
            if overflow == 0 {
                // just use one char as a type variable
                let mut s = String::new();
                s.push('\'');
                s.push((cur + 97) as char);
                s
            } else {
                // otherwise we need to use a couple chars
                if overflow > 25 {
                }
                let mut s = String::new();
                s.push('\'');
                s.push((overflow + 97) as char);
                s.push(((cur % 25) + 97) as char);
                s
            }
        }
        match self {
            &Type::Const(ref typeconst) => write!(fmt, "{}", typeconst),
            &Type::App(ref base, ref params) => {
                try!(write!(fmt, "{}", base));
                try!(write!(fmt, "["));
                let mut first = true;
                for ref p in params {
                    if !first {
                        try!(write!(fmt, ", "));
                    } else {
                        first = false;
                    }
                    try!(write!(fmt, "{}", p));
                }
                write!(fmt, "]")
            },
            &Type::Arrow(ref params, ref ret) => {
                if params.len() == 1 {
                    write!(fmt, "{} -> {}", params[0], ret)
                } else {
                    try!(write!(fmt, "("));
                    let mut first = true;
                    for ref p in params {
                        if !first {
                            try!(write!(fmt, ", "));
                        } else {
                            first = false;
                        }
                        try!(write!(fmt, "{}", p));
                    }
                    write!(fmt, ") -> {}", ret)
                }
            },
            &Type::Var(box TypeVar::Linked(ref ty)) => write!(fmt, "{}", ty),
            &Type::Var(box TypeVar::Unbound(id, _)) => write!(fmt, "_{}", id),
            &Type::Var(box TypeVar::Generic(id)) => {
                let s = names.entry(id).or_insert(new_name(&mut count));
                write!(fmt, "{}", s)
            }
        }
    }
}

static NEXT_ID: AtomicIsize = ATOMIC_ISIZE_INIT;

fn get_next_id() -> i32 {
    NEXT_ID.fetch_add(1, Ordering::Relaxed) as i32
}

pub fn new_var(level: i32) -> Type {
    Type::Var(Box::new(TypeVar::Unbound(get_next_id(), level)))
}

pub fn unify(left: &mut Type, right: &mut Type) -> Result<(), String> {
    // trivial primitive unification. Int unifies with Int, String with String, etc.
    if left == right {
        return Ok(());
    }

    match (left, right) {
        // the trivial unification - any constant type unifies with any other constant type
        // with the same name.
        (&mut Type::Const(TypeConst::TypeDef(ref s)),
         &mut Type::Const(TypeConst::TypeDef(ref d))) if s == d => Ok(()),
        // unification of type applications. Two type applications such as list[a] and list[b]
        // are unified by first unifying their base types (i.e. list with list) and then unifying
        // their parameters (i.e. a with b). It is an error to unify two type applications if they
        // utilize a different number of type parameters.
        (&mut Type::App(ref mut ty1, ref mut ty1parms),
         &mut Type::App(ref mut ty2, ref mut ty2parms)) => {
             let _ = try!(unify(ty1, ty2));
             if ty1parms.len() != ty2parms.len() {
                 return Err(format!("unexpected number of type parameters: expected {}, got {}", ty1parms.len(), ty2parms.len()))
             }
            for (ref mut p1, ref mut p2) in ty1parms.iter_mut().zip(ty2parms) {
                let _ = try!(unify(p1, p2));
            }
            Ok(())
        },
        // unification of function types. Functions are unified by first unifying their arguments and then
        // unifying their return type.
        (&mut Type::Arrow(ref mut ty1params, ref mut retty1),
         &mut Type::Arrow(ref mut ty2params, ref mut retty2)) => {
            if ty1params.len() != ty2params.len() {
                return Err(format!("unexpected number of parameters: expected {}, got {}", ty1params.len(), ty2params.len()))
            }
            for (ref mut p1, ref mut p2) in ty1params.iter_mut().zip(ty2params) {
                let _ = try!(unify(p1, p2));
            }
            let _ = try!(unify(retty1, retty2));
            Ok(())
        },
        // unification of linked type variables with another type by unifing
        // the type variable's linked type wth the given type.
        (&mut Type::Var(box TypeVar::Linked(ref mut ty1)), ref mut ty2) => {
            let _ = try!(unify(ty1, ty2));
            Ok(())
        },
        (ref mut ty1, &mut Type::Var(box TypeVar::Linked(ref mut ty2))) => {
            let _ = try!(unify(ty1, ty2));
            Ok(())
        },
        // sanity check to ensure that the index of an unbount type variable is unique.
        (&mut Type::Var(box TypeVar::Unbound(id1, _)),
         &mut Type::Var(box TypeVar::Unbound(id2, _))) if id1 == id2 => {
            panic!("should be exactly one instance of type variable")
        },
        // unification of an unbound type variable with a concrete type.
        // We change the unbound type variable to a "linked" type variable
        // that is linked to a specific type. This unification involes an occurs
        // check to avoid unifying any type variable with a structure that contains
        // that type variable. This is particularly important as this step could
        // otherwise allow the unification of a type variable 'a and a function
        // 'a -> 'b, which would result in an infinite type and general unsoundness
        // of the type system.
        (&mut Type::Var(ref mut v @ box TypeVar::Unbound(_, _)), ref mut ty) => {
            if let box TypeVar::Unbound(id, level) = *v {
                let _ = try!(occurs_check(id, level, ty));
            } else {
                unreachable!()
            }
            *v = Box::new(TypeVar::Linked(ty.clone()));
            Ok(())
        },
        (ref mut ty, &mut Type::Var(ref mut v @ box TypeVar::Unbound(_, _))) => {
            if let box TypeVar::Unbound(id, level) = *v {
                let _ = try!(occurs_check(id, level, ty));
            } else {
                unreachable!()
            }
            *v = Box::new(TypeVar::Linked(ty.clone()));
            Ok(())
        }
        // everything else is an error.
        _ => Err(format!("failed to unify"))
    }
}

pub fn occurs_check(id: i32, level: i32, ty: &mut Type) -> Result<(), String> {
    match ty {
        &mut Type::Var(box TypeVar::Linked(ref mut ty)) => occurs_check(id, level, ty),
        &mut Type::Var(box TypeVar::Generic(_)) => unreachable!(),
        &mut Type::Var(ref mut v @ box TypeVar::Unbound(_, _)) => {
            let (other_id, other_level) = if let box TypeVar::Unbound(i, l) = *v {
                (i, l)
            } else {
                unreachable!()
            };
            if other_id == id {
                // this is an infinite type.
                return Err(format!("occurs check failure"));
            }
            if other_level > level {
                *v = Box::new(TypeVar::Unbound(other_id, level));
            }
            Ok(())
        },
        &mut Type::App(ref mut ty, ref mut args) => {
            let _ = try!(occurs_check(id, level, ty));
            for ref mut p_ty in args.iter_mut() {
                let _ = try!(occurs_check(id, level, p_ty));
            }
            Ok(())
        },
        &mut Type::Arrow(ref mut args, ref mut ty) => {
            for ref mut p_ty in args.iter_mut() {
                let _ = try!(occurs_check(id, level, p_ty));
            }
            let _ = try!(occurs_check(id, level, ty));
            Ok(())
        },
        &mut Type::Const(_) => Ok(())
    }
}

pub fn generalize(level: i32, ty: &mut Type) -> Type {
    match ty {
        &mut Type::Var(box TypeVar::Unbound(id, other_level)) if other_level > level => {
            Type::Var(Box::new(TypeVar::Generic(id)))
        }
        &mut Type::App(ref mut base, ref mut args) => {
            Type::App(Box::new(generalize(level, base)),
                      args.iter_mut().map(|x| generalize(level, x)).collect())
        },
        &mut Type::Arrow(ref mut args, ref mut retty) => {
            Type::Arrow(args.iter_mut().map(|x| generalize(level, x)).collect(),
                        Box::new(generalize(level, retty)))
        },
        &mut Type::Var(box TypeVar::Linked(ref mut link_ty)) => generalize(level, link_ty),
        &mut Type::Var(box TypeVar::Generic(_)) |
        &mut Type::Var(box TypeVar::Unbound(_, _)) |
        &mut Type::Const(_) => ty.clone()
    }
}

pub fn instantiate(level: i32, ty: &mut Type) -> Type {
    let mut table = HashMap::new();
    fn instantiate_rec(map: &mut HashMap<i32, Type>, level: i32, ty: &mut Type) -> Type {
        match ty {
            &mut Type::Const(_) => ty.clone(),
            &mut Type::Var(box TypeVar::Linked(ref mut link_ty)) => instantiate_rec(map, level, link_ty),
            &mut Type::Var(box TypeVar::Generic(id)) => map.entry(id).or_insert(new_var(level)).clone(),
            &mut Type::Var(box TypeVar::Unbound(_, _)) => ty.clone(),
            &mut Type::App(ref mut basety, ref mut args) => Type::App(Box::new(instantiate(level, basety)),
                                                                      args.iter_mut().map(|x| instantiate(level, x)).collect()),
            &mut Type::Arrow(ref mut args, ref mut retty) => Type::Arrow(args.iter_mut().map(|x| instantiate(level, x)).collect(),
                                                                         Box::new(instantiate(level, retty)))
        }
    }
    instantiate_rec(&mut table, level, ty)
}
