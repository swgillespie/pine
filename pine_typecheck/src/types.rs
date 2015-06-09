use std::hash::Hash;
use std::collections::{HashSet, HashMap};
use std::sync::atomic::{AtomicIsize, ATOMIC_ISIZE_INIT, Ordering};
use std::fmt;

pub type TypeVar = i32;
pub type Substitution = HashMap<TypeVar, Type>;
pub type TypeEnv = HashMap<String, Scheme>;

#[derive(Debug, Clone)]
pub enum Type {
    Var(TypeVar),
    Const(TypeConst),
    Function(Vec<Type>, Box<Type>)
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeConst {
    Int,
    Bool,
    String,
    Float,
    Unit,
    Custom(String)
}

#[derive(Debug, Clone)]
pub struct Scheme {
    pub vars: Vec<TypeVar>,
    pub ty: Type
}

static NEXT_ID: AtomicIsize = ATOMIC_ISIZE_INIT;

fn get_next_id() -> i32 {
    NEXT_ID.fetch_add(1, Ordering::Relaxed) as i32
}

pub fn new_var() -> Type {
    Type::Var(get_next_id())
}


fn singleton_set<T: Eq + Hash>(elem: T) -> HashSet<T> {
    let mut set = HashSet::new();
    set.insert(elem);
    set
}

pub trait Types {
    fn free_type_variables(&self) -> HashSet<TypeVar>;
    fn apply_subst(&mut self, subst: &Substitution);
}

impl Types for Type {
    fn free_type_variables(&self) -> HashSet<TypeVar> {
        match self {
            &Type::Var(v) => singleton_set(v),
            &Type::Const(_) => HashSet::new(),
            &Type::Function(ref params, ref ret) => params.free_type_variables()
                .union(&ret.free_type_variables())
                .cloned()
                .collect()
        }
    }

    fn apply_subst(&mut self, subst: &Substitution) {
        match self {
            &mut Type::Var(v) => match subst.get(&v) {
                Some(t) => {
                    *self = t.clone()
                },
                None => {}
            },
            &mut Type::Function(ref mut params, ref mut ret) => {
                params.apply_subst(subst);
                ret.apply_subst(subst);
            }
            _ => {}
        }
    }
}

impl<T: Types> Types for Vec<T> {
    fn free_type_variables(&self) -> HashSet<TypeVar> {
        self.iter()
            .fold(HashSet::new(), |s, t| {
                s.union(&t.free_type_variables())
                    .cloned()
                    .collect()
            })
    }

    fn apply_subst(&mut self, subst: &Substitution) {
        for ty in self.iter_mut() {
            ty.apply_subst(subst);
        }
    }
}

impl Types for Scheme {
    fn free_type_variables(&self) -> HashSet<TypeVar> {
        let substituted_types : HashSet<_> = self.vars.iter()
            .cloned()
            .collect();
        self.ty.free_type_variables()
            .difference(&substituted_types)
            .cloned()
            .collect()
    }

    fn apply_subst(&mut self, subst: &Substitution) {
        let mut new_subst = subst.clone();
        for var in &self.vars {
            new_subst.remove(var);
        }
        self.ty.apply_subst(&new_subst);
    }
}

impl Types for TypeEnv {
    fn free_type_variables(&self) -> HashSet<TypeVar> {
        let types: Vec<_> = self.values()
            .cloned()
            .collect();
        types.free_type_variables()
    }

    fn apply_subst(&mut self, subst: &Substitution) {
        for (_, ty) in self.iter_mut() {
            ty.apply_subst(subst);
        }
    }
}

pub fn empty_subst() -> Substitution {
    HashMap::new()
}

pub fn compose_subst(a: &Substitution, b: &Substitution) -> Substitution {
    let mut cloned_a = a.clone();
    for (_, value) in cloned_a.iter_mut() {
        value.apply_subst(a);
    }
    for (key, value) in b.iter() {
        cloned_a.entry(*key).or_insert(value.clone());
    }
    cloned_a
}

pub fn instantiate(scheme: &mut Scheme) {
    let mut subst = HashMap::new();
    for var in scheme.vars.iter() {
        subst.insert(*var, Type::Var(*var));
    }
    scheme.ty.apply_subst(&subst);
}

pub fn unify(ty1: &Type, ty2: &Type) -> Result<Substitution, String> {
    match (ty1, ty2) {
        (&Type::Function(ref param1, ref ret1),
         &Type::Function(ref param2, ref ret2)) => {
            let subst1 = try!(unify_params(param1, param2));
            let subst2 = try!(unify(ret1, ret2));
            Ok(compose_subst(&subst1, &subst2))
        },
        (&Type::Var(v), ref t) => var_bind(v, t),
        (ref t, &Type::Var(v)) => var_bind(v, t),
        (&Type::Const(ref const_a),
         &Type::Const(ref const_b)) if const_a == const_b => Ok(empty_subst()),
        (_, _) => Err(format!("failed to unify type `{}` with type `{}`",
                              ty1, ty2))
    }
}

fn unify_params(params1: &[Type], params2: &[Type]) -> Result<Substitution, String> {
    if params1.len() != params2.len() {
        return Err("failed to unify, incorrect number of params"
                   .to_string());
    }
    let mut subst = empty_subst();
    for (p1, p2) in params1.iter().zip(params2.iter()) {
        let unifying_subst = try!(unify(p1, p2));
        subst = compose_subst(&subst, &unifying_subst)
    }
    Ok(subst)
}

pub fn generalize(env: &TypeEnv, ty: &Type) -> Scheme {
    let actual_vars : Vec<_> = ty.free_type_variables()
        .difference(&env.free_type_variables())
        .cloned()
        .collect();
    Scheme {
        vars: actual_vars,
        ty: ty.clone()
    }
}

pub fn var_bind(var: TypeVar, ty: &Type) -> Result<Substitution, String> {
    if let &Type::Var(u) = ty {
        if u == var {
            Ok(empty_subst())
        } else {
            let mut subst = HashMap::new();
            subst.insert(var, ty.clone());
            Ok(subst)
        }
    } else if ty.free_type_variables().contains(&var) {
        Err("occurs check failure".to_string())
    } else {
        let mut subst = HashMap::new();
        subst.insert(var, ty.clone());
        Ok(subst)
    }
}

impl fmt::Display for Type {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &Type::Const(ref typeconst) => match typeconst {
                &TypeConst::Int => write!(fmt, "int"),
                &TypeConst::Bool => write!(fmt, "bool"),
                &TypeConst::String => write!(fmt, "string"),
                &TypeConst::Float => write!(fmt, "float"),
                &TypeConst::Unit => write!(fmt, "unit"),
                &TypeConst::Custom(ref s) => write!(fmt, "{}", s)
            },
            &Type::Function(ref params, ref ret) => {
                if params.len() == 1 {
                    if let &Type::Function(_, _) = &params[0] {
                        write!(fmt, "({}) -> {}", params[0], ret)
                    } else {
                        write!(fmt, "{} -> {}", params[0], ret)
                    }
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
            &Type::Var(num) => {
                let mut s = String::new();
                if num / 25 > 1 {
                    for _ in (1..num/25) {
                        s.push('\'');
                    }
                }
                s.push(((num as u8 % 25u8) + 97u8) as char);
                write!(fmt, "{}", s)
            }
        }
    }
}
