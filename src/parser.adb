with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers;        use Ada.Containers;
with Ada.Containers.Vectors;

with Ast; use Ast;
with Lex; use Lex;

package body Parser is

--
-- Global declarations
--
package AstExprStack is new Ada.Containers.Vectors
    ( Index_Type => Natural,
      Element_Type => AstExpression);

--
-- Forward declarations
--
procedure Parse_Function(file : in out AstFile);
procedure Parse_Block(block : in out AstBlock);
function Parse_Expression(end_token : TokenType) return AstExpression;

--
-- The main section of the AST parser
--
function Parse(name : string) return AstFile is
    file : AstFile := Create_Ast_File(name);
    t : Token;
begin
    Lex_Init(name);
    
    -- Parse the global scope
    t := Lex_Get_Next;
    while t.token_type /= T_Eof loop
        case t.token_type is
            when T_Func => Parse_Function(file);
            
            when others =>
                Put_Line("Error: Invalid token in global scope.");
                Put_Line(TokenType'Image(t.token_type));
        end case;
        
        t := Lex_Get_Next;
    end loop;
    
    -- Return our finished product
    Lex_Close;
    return file;
end Parse;

--
-- The function parser
--
procedure Parse_Function(file : in out AstFile) is
    t : Token;
    func_name : Unbounded_String;
    func : AstFunction;
begin
    -- Start with the function name
    t := Lex_Get_Next;
    func_name := t.string_value;
    if t.token_type /= T_Id then
        Put_Line("Error: Expected function name.");
        Put_Line(TokenType'Image(t.token_type));
        return;
    end if;
    
    -- Arguments
    -- TODO
    
    -- Next token should be "is"
    t := Lex_Get_Next;
    if t.token_type /= T_Is then
        Put_Line("Error: Expected is.");
        Put_Line(TokenType'Image(t.token_type));
        return;
    end if;
    
    -- Construct the AST function and build the block
    func := Create_Ast_Function(To_String(func_name));
    Parse_Block(func.block);
    Add_Function(file, func);
end Parse_Function;

--
-- The statement block parser
--
procedure Parse_Block(block : in out AstBlock) is
    t : Token;
    stmt : AstStatement;
    expr : AstExpression;
begin
    t := Lex_Get_Next;
    while t.token_type /= T_End and t.token_type /= T_Eof loop
        case t.token_type is
            when T_Return =>
                stmt := Create_Ast_Statement(AST_Return);
                expr := Parse_Expression(T_SemiColon);
                if expr.ast_type /= AST_None then
                    Set_Expression(stmt, expr);
                end if;
                Add_Statement(block, stmt);
            
            when others =>
                Put_Line("Error: Invalid token in statement.");
                Put_Line(TokenType'Image(t.token_type));
        end case;
        
        t := Lex_Get_Next;
    end loop;
end Parse_Block;

--
-- The expression parser
--
function Parse_Expression(end_token : TokenType) return AstExpression is
    t : Token;
    expr : AstExpression;
    stack, op_stack : AstExprStack.Vector;
    
    procedure Process_Stack is
        lval, rval, op : AstExpression;
    begin
        while op_stack.Length > 0 loop
            op := op_stack.Last_Element;
            op_stack.Delete_Last;
            
            rval := stack.Last_Element;
            stack.Delete_Last;
            
            lval := stack.Last_Element;
            stack.Delete_Last;
            
            Create_Binary_Op(op, lval, rval);
            stack.Append(op);
        end loop;
    end Process_Stack;
begin
    t := Lex_Get_Next;
    while t.token_type /= end_token and t.token_type /= T_Eof loop
        case t.token_type is
            when T_Int =>
                expr := Create_Ast_Expression(AST_Int);
                expr.int_value := t.int_value;
                stack.Append(expr);
                
            when T_Id =>
                expr := Create_Ast_Expression(AST_Id);
                expr.string_value := t.string_value;
                stack.Append(expr);
                
            -- Operators
            when T_Add => op_stack.Append(Create_Ast_Expression(AST_Add));
            when T_Sub => op_stack.Append(Create_Ast_Expression(AST_Sub));
            when T_Mul => op_stack.Append(Create_Ast_Expression(AST_Mul));
            when T_Div => op_stack.Append(Create_Ast_Expression(AST_Div));
            
            -- Unknown
            when others =>
                Put_Line("Error: Invalid token in expression.");
                Put_Line(TokenType'Image(t.token_type));
        end case;
        
        t := Lex_Get_Next;
    end loop;
    
    Process_Stack;

    if stack.Length = 0 then
        return Create_Ast_Expression(AST_None);
    end if;
    
    return stack.Last_Element;
end Parse_Expression;

end Parser; -- End body of Parser

