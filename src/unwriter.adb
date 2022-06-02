with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Unwriter is

--
-- Forward declarations
--
procedure unwrite_block(block : AstBlock);
procedure unwrite_statement(stmt : AstStatement; indent : integer);
procedure unwrite_expression(expr : AstExpression);

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
procedure unwrite_expression(expr : AstExpression) is
begin
    case expr.ast_type is
        -- Literals and identifiers
        when AST_Int => Put(expr.int_value, 0);
        when AST_Id => Put(To_String(expr.string_value));
        
        -- Operators
        when AST_Add | AST_Sub | AST_Mul | AST_Div =>
            unwrite_expression(expr.lval.all);
            case expr.ast_type is
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

end Unwriter; -- End package

