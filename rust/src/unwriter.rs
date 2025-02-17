//
// This software is licensed under BSD0 (public domain).
// Therefore, this software belongs to humanity.
// See COPYING for more info.
//
use crate::ast::*;

pub fn unwrite(file : AstFile) {
    for path in file.get_imports() {
        print!("import ");
        for c in path.chars() {
            if c == '/' { print!("."); }
            else { print!("{}", c); }
        }
        println!(";");
    }

    for s in file.get_structs() {
        unwrite_structure(s);
    }
    
    for c in file.get_consts() {
        print!("const {} : ", c.get_name());
        unwrite_data_type(&c.get_data_type());
        print!(" := ");
        unwrite_expression(c.get_expression(), false);
        println!(";");
    }
    
    for func in file.get_functions() {
        unwrite_function(func);
    }
}

fn unwrite_structure(s : &AstStruct) {
    println!("struct {} is", s.get_name());
    for item in s.get_items() {
        print!("    {} : ", item.get_name());
        unwrite_data_type(&item.get_data_type());
        print!(" := ");
        unwrite_expression(item.get_expression(), true);
        println!(";");
    }
    println!("end");
}

fn unwrite_function(func : &AstFunction) {
    print!("func {}", func.get_name());
    let args = func.get_args();
    if args.len() > 0 {
        print!("(");
        let mut index : usize = 0;
        for arg in args {
            print!("{} : ", arg.get_name());
            unwrite_data_type(&arg.get_data_type());
            if index + 1 < args.len() {
                print!(", ");
            }
            index += 1;
        }
        print!(")");
    }
    
    if func.get_data_type() != DataType::Void {
        print!(" -> ");
        unwrite_data_type(&func.get_data_type());
    }
    println!(" is");
    
    for c in func.get_consts() {
        print!("    ");
        print!("const {} : ", c.get_name());
        unwrite_data_type(&c.get_data_type());
        print!(" := ");
        unwrite_expression(c.get_expression(), false);
        println!(";");
    }
    
    unwrite_block(func.get_block(), 0);
    println!("end");
}

fn unwrite_block(block : &AstStatement, indent : i32) {
    for stmt in block.get_statements() {
        unwrite_statement(&stmt, indent+4);
    }
    
    /*for _i in 0 .. indent {
        print!(" ");
    }
    println!("end");*/
}

fn unwrite_statement(stmt : &AstStatement, indent : i32) {
    for _i in 0 .. indent {
        print!(" ");
    }
    
    match stmt.get_type() {
        AstType::Return => {
            print!("return");
            let expr = stmt.get_expression();
            if expr.get_type() != AstType::None {
                print!(" ");
                unwrite_expression(expr, false);
            } 
            println!(";");
        },
    
        AstType::VarDec => {
            print!("var {} : ", stmt.get_name());
            unwrite_data_type(&stmt.get_data_type());
            print!(" := ");
            unwrite_expression(stmt.get_expression(), true);
            println!(";");
        },
        
        AstType::ArrayDec => {
            print!("var {} : ", stmt.get_name());
            unwrite_data_type(&stmt.get_data_type());
            print!("[");
            unwrite_expression(stmt.get_expression(), false);
            println!("];");
        },
        
        AstType::StructDec => {
            print!("struct {} : ", stmt.get_name());
            unwrite_expression(stmt.get_expression(), false);
            println!(";");
        },
        
        AstType::ExprStmt => {
            unwrite_expression(stmt.get_expression(), false);
            println!(";");
        },
    
        AstType::CallStmt => {
            print!("{}", stmt.get_name());
            //if stmt.expr.ast_type != AstType::ExprList {
                print!("(");
                unwrite_expression(stmt.get_expression(), false);
                print!(")");
            /*} else {
                unwrite_expression(&stmt.expr);
            }*/
            println!(";");
        },
        
        AstType::While => {
            print!("while ");
            unwrite_expression(stmt.get_expression(), false);
            println!(" do");
            
            unwrite_block(stmt.get_block(), indent);
            for _i in 0 .. indent { print!(" "); }
            println!("end");
        },
        
        AstType::If | AstType::Elif => {
            if stmt.get_type() == AstType::Elif { print!("elif "); }
            else { print!("if "); }
            unwrite_expression(stmt.get_expression(), false);
            println!(" then");
            
            unwrite_block(stmt.get_block(), indent);
            
            for br in stmt.get_branches() {
                unwrite_statement(br, indent);
            }
            
            if stmt.get_type() == AstType::If {
                for _i in 0 .. indent { print!(" "); }
                println!("end");
            }
        },
        
        AstType::Else => {
            println!("else");
            unwrite_block(stmt.get_block(), indent);
        },
        
        AstType::Break => println!("break;"),
        AstType::Continue => println!("continue;"),
        
        _ => { println!(""); },
    }
}

fn unwrite_expression(expr : &AstExpression, ignore_lval : bool) {
    match expr.get_type() {
        //
        // Binary operators
        //
        AstType::Assign => {
            if !ignore_lval {
                unwrite_expression(expr.get_lval(), false);
                print!(" := ");
            }
            unwrite_expression(expr.get_rval(), false);
        },
        
        AstType::Add | AstType::Sub
        | AstType::Mul | AstType::Div | AstType::Mod
        | AstType::And | AstType::Or | AstType::Xor 
        | AstType::Eq | AstType::Ne
        | AstType::Gt | AstType::Ge | AstType::Lt | AstType::Le 
        | AstType::LGAnd | AstType::LGOr => {
            unwrite_expression(expr.get_lval(), false);
            match expr.get_type() {
                AstType::Add => print!(" + "),
                AstType::Sub => print!(" - "),
                AstType::Mul => print!(" * "),
                AstType::Div => print!(" / "),
                AstType::Mod => print!(" % "),
                AstType::And => print!(" & "),
                AstType::Or => print!(" | "),
                AstType::Xor => print!(" ^ "),
                
                AstType::Eq => print!(" = "),
                AstType::Ne => print!(" != "),
                AstType::Gt => print!(" > "),
                AstType::Ge => print!(" >= "),
                AstType::Lt => print!(" < "),
                AstType::Le => print!(" <= "),
                
                AstType::LGAnd => print!(" && "),
                AstType::LGOr => print!(" || "),
                
                _ => {},
            }
            unwrite_expression(expr.get_rval(), false);
        },
        
        //
        // Literals and primary expressions
        //
        AstType::Id => print!("{}", expr.get_name()),
        AstType::IntLiteral => print!("{}", expr.get_int()),
        AstType::StringLiteral => print!("{:?}", expr.get_string()),
        AstType::CharLiteral => print!("{:?}", expr.get_char()),
        AstType::BoolLiteral(val) => print!("{}", val),
        
        AstType::ArrayAcc => {
            print!("{}[", expr.get_name());
            unwrite_expression(expr.get_arg(), false);
            print!("]");
        },
        
        AstType::StructAcc => {
            print!("{}.", expr.get_name());
            unwrite_expression(expr.get_arg(), false);
        },
        
        //
        // Generic expressions
        //
        AstType::ExprList => {
            let mut index : usize = 0;
            for item in expr.get_list() {
                unwrite_expression(&item, false);
                if index + 1 < expr.get_list_size() {
                    print!(", ");
                }
                index += 1;
            }
        },
        
        AstType::Call => {
            print!("{}(", expr.get_name());
            unwrite_expression(expr.get_arg(), false);
            print!(")");
        },
        
        _ => {},
    }
}

fn unwrite_data_type(data_type : &DataType) {
    match &data_type {
        DataType::Void => print!("void"),
        DataType::I8 => print!("i8"),
        DataType::U8 => print!("u8"),
        DataType::I16 => print!("i16"),
        DataType::U16 => print!("u16"),
        DataType::I32 => print!("i32"),
        DataType::U32 => print!("u32"),
        DataType::I64 => print!("i64"),
        DataType::U64 => print!("u64"),
        DataType::String => print!("string"),
        DataType::Char => print!("char"),
        DataType::Bool => print!("bool"),
    }
}

