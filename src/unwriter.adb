with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Unwriter is

--
-- Forward declarations
--
procedure unwrite_block(block : AstBlock);
procedure unwrite_statement(stmt : AstStatement; indent : integer);
procedure unwrite_expression(expr : AstExpression; print_lval : boolean := true);
procedure unwrite_data_type(data_type : DataType);

--
-- The entry point of the unwriter
--
procedure unwrite(file : AstFile) is
begin
    -- Start with functions
    for func of file.funcs loop
        Put_Line("func " & To_String(func.name) & " is");
        unwrite_block(func.block);
        Put_Line("end");
    end loop;
end unwrite;

--
-- Uwrites a statement block
--
procedure unwrite_block(block : AstBlock) is
begin
    for stmt of block.statements loop
        unwrite_statement(stmt, 4);
    end loop;
end unwrite_block;

--
-- Unwrites a statement
--
procedure unwrite_statement(stmt : AstStatement; indent : integer) is
begin
    for i in 0 .. indent loop Put(" "); end loop;

    case stmt.ast_type is
        when AST_Var =>
            Put("var " & To_String(stmt.name) & " : ");
            unwrite_data_type(stmt.data_type);
            unwrite_expression(stmt.expr, false);
            Put_Line(";");
            
        when AST_Call_Stmt =>
            Put(To_String(stmt.name) & "(");
            unwrite_expression(stmt.expr);
            Put_Line(");");
        
        when AST_Return =>
            Put("return");
            if Has_Expression(stmt) then
                Put(" ");
                unwrite_expression(stmt.expr);
            end if;
            Put_Line(";");
        
        when others => null;
    end case;
end unwrite_statement;

--
-- Unwrites an expression
--
procedure unwrite_expression(expr : AstExpression; print_lval : boolean := true) is
begin
    case expr.ast_type is
        -- Literals and identifiers
        when AST_Int => Put(expr.int_value, 0);
        when AST_String => Put('"' & To_String(expr.string_value) & '"');
        when AST_Id => Put(To_String(expr.string_value));
        
        -- Operators
        when AST_Assign |
             AST_Add | AST_Sub | AST_Mul | AST_Div =>
            if print_lval then
                unwrite_expression(expr.lval.all);
            end if;
            case expr.ast_type is
                when AST_Assign => Put(" := ");
                when AST_Add => Put(" + ");
                when AST_Sub => Put(" - ");
                when AST_Mul => Put(" * ");
                when Ast_Div => Put(" / ");
                
                when others => null;
            end case;
            unwrite_expression(expr.rval.all);
        
        -- Other
        when others => null;
    end case;
end unwrite_expression;

--
-- Unwrites a data type
--
procedure unwrite_data_type(data_type : DataType) is
begin
    case data_type is
        when I8 => Put("i8");
        when U8 => Put("u8");
        
        when I16 => Put("i16");
        when U16 => Put("u16");
        
        when I32 => Put("i32");
        when U32 => Put("u32");
        
        when I64 => Put("i64");
        when U64 => Put("u64");
        
        when Char => Put("char");
        when Str => Put("string");
        when Bool => Put("bool");
        
        when others => Put("void");
    end case;
end unwrite_data_type;

end Unwriter; -- End package

