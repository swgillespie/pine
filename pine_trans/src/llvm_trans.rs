use pine_typecheck::typed_ast::*;
use pine_typecheck::TypedVisitor;
use pine_typecheck::types::{Type, TypeConst};

use llvm_sys::prelude::*;
use llvm_sys::core::*;
use llvm_sys::{LLVMLinkage, LLVMIntPredicate};
use llvm_sys::analysis::{LLVMVerifyModule, LLVMVerifierFailureAction};
use llvm_sys::bit_writer::LLVMWriteBitcodeToFile;

use std::default::Default;
use std::ptr;
use std::ffi::{CString, CStr};
use std::collections::HashMap;

pub struct TranslatedModule(pub LLVMModuleRef);

impl Drop for TranslatedModule {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeModule(self.0)
        }
    }
}

impl TranslatedModule {
    pub fn verify(&self) {
        unsafe {
            let mut ptr = ptr::null_mut();
            let ptr_to_ptr : *mut _ = &mut ptr;
            LLVMVerifyModule(self.0, LLVMVerifierFailureAction::LLVMAbortProcessAction, ptr_to_ptr);
            // ptr is a c_char* to a message given to us by llvm.
            let cstr = CStr::from_ptr(ptr);
            let string = String::from_utf8_lossy(cstr.to_bytes());
            println!("llvm failed to verify module: {}", string);
        }
    }

    pub fn write_to_file(&self, file: &str) {
        unsafe {
            let filename = CString::new(file).unwrap();
            LLVMWriteBitcodeToFile(self.0, filename.as_ptr());
        }
    }
}

#[derive(Copy, Clone)]
pub struct LLVMValue(pub LLVMValueRef);

impl Default for LLVMValue {
    fn default() -> LLVMValue {
        LLVMValue(ptr::null_mut())
    }
}

impl LLVMValue {
    fn unwrap(self) -> LLVMValueRef {
        if self.0.is_null() {
            panic!("null pointer in llvm_trans");
        }
        self.0
    }
}

pub fn translate(functions: &mut TypedCompilationUnit, mod_name: &str) -> TranslatedModule {
    let name = CString::new(mod_name).unwrap();
    let module = unsafe { TranslatedModule(LLVMModuleCreateWithName(name.as_ptr())) };
    let mut visitor = TransVisitor::new(module.0);
    for func in functions.iter_mut() {
        visitor.visit_function(func);
    }
    module
}

pub struct TransVisitor {
    module: LLVMModuleRef,
    builder: LLVMBuilderRef,
    symbols: HashMap<String, LLVMValueRef>
}

impl TransVisitor {
    pub fn new(module: LLVMModuleRef) -> TransVisitor {
        TransVisitor {
            module: module,
            builder: ptr::null_mut(),
            symbols: HashMap::new()
        }
    }

    fn get_current_fn(&self) -> LLVMValueRef {
        unsafe {
            LLVMGetBasicBlockParent(LLVMGetInsertBlock(self.builder))
        }
    }

    fn get_current_block(&self) -> LLVMBasicBlockRef {
        unsafe {
            LLVMGetInsertBlock(self.builder)
        }
    }
}

impl TypedVisitor for TransVisitor {
    type Return = LLVMValue;

    fn visit_function(&mut self, func: &mut TypedFunction) -> LLVMValue {
        let mut param_tys : Vec<_> = func.parameter_types.iter()
            .map(type_to_llvm_type)
            .collect();
        let return_ty = type_to_llvm_type(&func.return_type);
        let fun = unsafe {
            let function_type = LLVMFunctionType(return_ty,
                                                 param_tys.as_mut_ptr(),
                                                 param_tys.len() as u32,
                                                 0);
            let name = CString::new(&*func.name.clone()).unwrap();
            LLVMAddFunction(self.module, name.as_ptr(), function_type)
        };
        self.symbols.insert(func.name.clone(), fun);
        unsafe {
            LLVMSetFunctionCallConv(fun, 1 /*LLVMFastCallConv*/);
            let name = CString::new("entry").unwrap();
            let entry_block = LLVMAppendBasicBlock(fun, name.as_ptr());
            self.builder = LLVMCreateBuilder();
            LLVMPositionBuilderAtEnd(self.builder, entry_block);
            for (idx, param) in func.parameter_names.iter().enumerate() {
                info!(target: "trans", "inserting parameter `{}` into trans environment", param);
                self.symbols.insert(param.clone(), LLVMGetParam(fun, idx as u32));
            }
            let body = self.visit_expression(&mut func.body).unwrap();
            LLVMBuildRet(self.builder, body);
            // TODO - i'm breaking the rules of Rust because I want to get this done :(
            // (potential resource leak of builder)
            LLVMDisposeBuilder(self.builder);
        }
        LLVMValue(fun)
    }

    fn visit_expression(&mut self, expr: &mut TypedExpression) -> LLVMValue {
        match expr.data {
            Expression::Literal(ref mut literal) => self.visit_literal(literal),
            Expression::Identifier(ref mut string) => self.visit_identifier(string),
            Expression::Ref(ref mut expr) => self.visit_ref(expr),
            Expression::IfThenElse(ref mut cond, ref mut true_branch, ref mut false_branch) => self.visit_if_then_else(cond, true_branch, false_branch),
            Expression::FunctionCall(ref mut func, ref mut params) => self.visit_function_call(func, params),
            Expression::Let(ref mut pat, ref mut binding, ref mut expr) => self.visit_let(pat, binding, expr),
            Expression::Assign(ref mut target, ref mut source) => self.visit_assign(target, source),
            Expression::BinaryOperator(ref mut left, ref mut right, ref mut op) => self.visit_binary_op(left, right, op),
            Expression::UnaryOperator(ref mut operand, ref mut op) => self.visit_unary_op(operand, op),
            Expression::TupleCreation(ref mut elements) => self.visit_tuple(elements),
            Expression::Paren(ref mut expr) => self.visit_paren(expr)
        }
    }

    fn visit_literal(&mut self,
                     lit: &mut TypedLiteral) -> LLVMValue {
        // all we have to do here is load an immediate value.
        // the exception is strings. For every string we need to
        // create a global string and load a pointer to it.
        let value = match lit.data {
            Literal::Int(i) => unsafe { LLVMConstInt(LLVMInt32Type(), i as u64, 0) },
            Literal::Bool(b) => unsafe { LLVMConstInt(LLVMInt1Type(), b as u64, 0) },
            // wtf LLVM, why doesn't LLVMConstFloat exist
            Literal::Float(_) => /* unsafe { LLVMConstFloat(LLVMFloatType(), f, 0)}; */ unimplemented!(),
            Literal::String(ref s) => unsafe {
                // this is where things get interesting. First, we create a new global
                let name = CString::new("strlit").unwrap();
                // TODO - &*s.clone() looks a little iffy
                let data = CString::new(&*s.clone()).unwrap();
                let global = LLVMAddGlobal(self.module, LLVMArrayType(LLVMInt8Type(), s.len() as u32), name.as_ptr());
                // set its linkage to internal
                LLVMSetLinkage(global, LLVMLinkage::LLVMInternalLinkage);
                // mark it as a global constant
                LLVMSetGlobalConstant(global, 1);
                // set the initializer to be the value of the string literal
                LLVMSetInitializer(global, LLVMConstString(data.as_ptr(), s.len() as u32, 1));
                // return the global.
                global
            },
            // by convention, unit is boolean false
            Literal::Unit => unsafe { LLVMConstInt(LLVMInt1Type(), 0, 0) }
        };
        LLVMValue(value)
    }

    fn visit_identifier(&mut self,
                        ident: &mut TypedIdentifier) -> LLVMValue {
        info!(target: "trans", "querying environment for `{}`", ident.data);
        let value = *self.symbols.get(&ident.data).unwrap();
        if let Type::Function(_, _) = ident.ty {
            info!(target: "trans", "ident is a function, skipping the load");
            LLVMValue(value)
        } else {
            info!(target: "trans", "ident is not a function, loading an alloca");
            let load = unsafe {
                let name = CString::new(&*ident.data).unwrap();
                LLVMBuildLoad(self.builder, value, name.as_ptr())
            };
            LLVMValue(load)
        }
    }

    fn visit_ref(&mut self,
                 _: &mut TypedExpression) -> LLVMValue {
        unimplemented!()
    }

    fn visit_if_then_else(&mut self,
                          cond: &mut TypedExpression,
                          true_branch: &mut TypedExpression,
                          false_branch: &mut Option<Box<TypedExpression>>) -> LLVMValue {
        // TODO - short circuit evaluation
        let cond_reg = self.visit_expression(cond).unwrap();
        let mut then_block = unsafe {
            let name = CString::new("then").unwrap();
            LLVMAppendBasicBlock(self.get_current_fn(),
                                 name.as_ptr())
        };
        let end_block = unsafe {
            let name = CString::new("end_if").unwrap();
            LLVMAppendBasicBlock(ptr::null_mut(),
                                 name.as_ptr())
        };

        // if this if statement has a false branch, then it can yield
        // a value and we'll need to build a phi node for it.
        if let &mut Some(ref mut expr) = false_branch {
            let mut else_block = unsafe {
                let name = CString::new("else").unwrap();
                LLVMAppendBasicBlock(ptr::null_mut(),
                                     name.as_ptr())
            };
            unsafe {
                LLVMBuildCondBr(self.builder, cond_reg, then_block, else_block);
                // codegen the true block.
                LLVMPositionBuilderAtEnd(self.builder, then_block);
                let then_result = self.visit_expression(true_branch).unwrap();
                LLVMBuildBr(self.builder, end_block);
                then_block = self.get_current_block();
                // next, codegen the false block.
                LLVMPositionBuilderAtEnd(self.builder, else_block);
                let else_result = self.visit_expression(expr).unwrap();
                LLVMBuildBr(self.builder, end_block);
                else_block = self.get_current_block();
                // finally, codegen the end_if block.
                // this needs a phi node.
                LLVMPositionBuilderAtEnd(self.builder, end_block);
                let expr_ty = type_to_llvm_type(&true_branch.ty);
                let phi = LLVMBuildPhi(self.builder, expr_ty,
                                       CString::new("phi").unwrap().as_ptr());
                let mut incoming_values = &mut [then_result, else_result];
                let mut incoming_blocks = &mut [then_block, else_block];
                LLVMAddIncoming(phi,
                                incoming_values.as_mut_ptr(),
                                incoming_blocks.as_mut_ptr(),
                                2);
                // all done - return the phi node as the value of
                // this subexpression.
                LLVMValue(phi)
            }
        } else {
            // otherwise we don't have an else block and the true block
            // will guaranteed to have type `unit`.
            unsafe {
                LLVMBuildCondBr(self.builder, cond_reg, then_block, end_block);
                LLVMPositionBuilderAtEnd(self.builder, then_block);
                // guaranteed to be void, by type resolution
                let _ = self.visit_expression(true_branch).unwrap();
                LLVMBuildBr(self.builder, end_block);
                LLVMPositionBuilderAtEnd(self.builder, end_block);
                // nothing to do here except create a new void value
                // and return it.
                LLVMValue(LLVMConstInt(LLVMInt1Type(), 0, 0))
            }
        }
    }

    fn visit_function_call(&mut self,
                           func: &mut TypedExpression,
                           params: &mut [TypedExpression]) -> LLVMValue {
        let func_value = self.visit_expression(func).unwrap();
        let mut args_values : Vec<_> = params.iter_mut()
            .map(|f| self.visit_expression(f).unwrap())
            .collect();
        let value = unsafe {
            let call = LLVMBuildCall(self.builder,
                                     func_value,
                                     args_values.as_mut_ptr(),
                                     args_values.len() as u32,
                                     ptr::null_mut());
            // `tail` is a hint to LLVM to make this a tail-call
            // if it can. We (the frontend) don't do anything to
            // help LLVM out, so it'll only do TCO in a limited
            // capacity.
            LLVMSetTailCall(call, 1);
            // by default, Pine uses the llvm `fastcall` convention
            // to assist with TCO. Sometimes it needs to call extern
            // functions, which use the C calling convention.
            // We need to be sure to use the right calling convention
            // when invoking functions.
            let cconv = LLVMGetFunctionCallConv(func_value);
            LLVMSetInstructionCallConv(call, cconv);
            call
        };
        LLVMValue(value)
    }

    fn visit_let(&mut self,
                 pat: &mut Pattern,
                 binding: &mut TypedExpression,
                 expr: &mut TypedExpression) -> LLVMValue {
        // TODO - right now patterns can only be identifiers.
        let ident = match *pat {
            Pattern::Ident(ref i) => i.clone(),
            _ => panic!("only identifier patterns are supported")
        };
        // also TODO - by default we put all of our `let`s into
        // `alloca`s. This isn't entirely correct, though,
        // since lambdas may capture let-bound variables and cause them
        // to outlive this stack frame. In the future, when lambdas
        // are implemented, we will need to do escape analysis to
        // determine when it is safe to put a `let` binding on the
        // stack vs heap-allocating it.
        let alloca = unsafe {
            let name = CString::new(&*ident.clone()).unwrap();
            LLVMBuildAlloca(self.builder,
                            type_to_llvm_type(&binding.ty),
                            name.as_ptr())
        };
        // insert this symbol into our env so we know what local to use
        // when invoking the `let`-bound identifier.
        self.symbols.insert(ident.clone(), alloca);
        // next we codegen the binding.
        let binding_reg = self.visit_expression(binding).unwrap();
        // store the binding into the local we just created
        unsafe {
            LLVMBuildStore(self.builder, binding_reg, alloca);
        }
        // codegen the body expression.
        self.visit_expression(expr)
    }

    fn visit_assign(&mut self,
                    _: &mut TypedExpression,
                    _: &mut TypedExpression) -> LLVMValue {
        unimplemented!()
    }

    fn visit_binary_op(&mut self,
                       left: &mut TypedExpression,
                       right: &mut TypedExpression,
                       op: &Binop) -> LLVMValue {
        // TODO - short circuit evaluation of booleans
        let left_reg = self.visit_expression(left).unwrap();
        let right_reg = self.visit_expression(right).unwrap();
        let value = match *op {
            Binop::IntegerPlus => unsafe {
                LLVMBuildAdd(self.builder, left_reg, right_reg, ptr::null_mut())
            },
            Binop::IntegerMinus => unsafe {
                LLVMBuildSub(self.builder, left_reg, right_reg, ptr::null_mut())
            },
            Binop::IntegerMul => unsafe {
                LLVMBuildMul(self.builder, left_reg, right_reg, ptr::null_mut())
            },
            Binop::IntegerDiv => unsafe {
                LLVMBuildSDiv(self.builder, left_reg, right_reg, ptr::null_mut())
            },
            Binop::IntegerGeq => unsafe {
                LLVMBuildICmp(self.builder, LLVMIntPredicate::LLVMIntSGE, left_reg, right_reg, ptr::null_mut())
            },
            Binop::IntegerLeq => unsafe {
                LLVMBuildICmp(self.builder, LLVMIntPredicate::LLVMIntSLE, left_reg, right_reg, ptr::null_mut())
            },
            Binop::IntegerGT => unsafe {
                LLVMBuildICmp(self.builder, LLVMIntPredicate::LLVMIntSGT, left_reg, right_reg, ptr::null_mut())
            },
            Binop::IntegerLT => unsafe {
                LLVMBuildICmp(self.builder, LLVMIntPredicate::LLVMIntSLT, left_reg, right_reg, ptr::null_mut())
            },
            Binop::PointerEq => unsafe {
                LLVMBuildICmp(self.builder, LLVMIntPredicate::LLVMIntEQ, left_reg, right_reg, ptr::null_mut())
            },
            Binop::PointerNeq => unsafe {
                LLVMBuildICmp(self.builder, LLVMIntPredicate::LLVMIntNE, left_reg, right_reg, ptr::null_mut())
            },
            Binop::BooleanAnd => unsafe {
                LLVMBuildAnd(self.builder, left_reg, right_reg, ptr::null_mut())
            },
            Binop::BooleanOr => unsafe {
                LLVMBuildOr(self.builder, left_reg, right_reg, ptr::null_mut())
            }
        };
        LLVMValue(value)
    }

    fn visit_unary_op(&mut self,
                      operand: &mut TypedExpression,
                      op: &Unop) -> LLVMValue {
        let op_reg = self.visit_expression(operand).unwrap();
        let value = match *op {
            Unop::PointerDereference => unimplemented!(),
            Unop::BooleanNot => unsafe {
                LLVMBuildNot(self.builder, op_reg, ptr::null_mut())
            },
            Unop::IntegerNegate => unsafe {
                LLVMBuildSub(self.builder, LLVMConstInt(LLVMInt32Type(), 0, 0), op_reg, ptr::null_mut())
            }
        };
        LLVMValue(value)
    }

    fn visit_tuple(&mut self,
                   _: &mut [TypedExpression]) -> LLVMValue {
        unimplemented!()
    }

    fn visit_paren(&mut self,
                   expr: &mut TypedExpression) -> LLVMValue {
        self.visit_expression(expr)
    }

    fn visit_pattern(&mut self,
                     _: &Pattern) -> LLVMValue {
        unimplemented!()
    }
}

fn type_to_llvm_type(ty: &Type) -> LLVMTypeRef {
    match *ty {
        Type::Var(_) => panic!("type variables shouldn't exist in trans"),
        Type::Const(ref c) => match *c {
            TypeConst::Int => unsafe { LLVMInt32Type() },
            TypeConst::Bool => unsafe { LLVMInt1Type() },
            TypeConst::String => unsafe { LLVMPointerType(LLVMInt8Type(), 0) },
            TypeConst::Float => unsafe { LLVMFloatType() },
            // Unit is not like LLVM's void. LLVM's void has zero
            // members, where Unit has one member: `unit`.
            // TODO this kinda sucks, what does Rust do for ()?
            TypeConst::Unit => unsafe { LLVMInt1Type() },
            TypeConst::Custom(_) => panic!("todo - custom types")
        },
        Type::Function(ref args, ref ret) => {
            let return_ty = type_to_llvm_type(ret);
            let mut param_tys : Vec<_> = args.iter()
                .map(type_to_llvm_type)
                .collect();
            let fn_ty = unsafe {
                let param_tys_ptr = param_tys.as_mut_ptr();
                LLVMFunctionType(return_ty,
                                 param_tys_ptr,
                                 param_tys.len() as u32,
                                 0)
            };
            // TODO - right now function values are represented as
            // a function pointer, but in the future they will need
            // to be a struct containing a function pointer and a
            // closure.
            // Keeping it simple for now.
            unsafe { LLVMPointerType(fn_ty, 0) }
        }
    }
}
