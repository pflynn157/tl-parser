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
procedure Parse_Data_Type(data_type : in out DataType);

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
    
    -- A helper function for parsing variable declarations
    procedure Parse_Var_Dec is
        name : Unbounded_String;
        data_type : DataType := Void;
        lval : AstExpression;
    begin
        -- Collect variable information and perform basic syntax checks
        t := Lex_Get_Next;
        name := t.string_value;
        if t.token_type /= T_Id then
            Put_Line("Error: Invalid token in variable declaration: Expected name");
            Put_Line(TokenType'Image(t.token_type));
            return;
        end if;
        
        t := Lex_Get_Next;
        if t.token_type /= T_Colon then
            Put_Line("Error: Expected colon.");
            Put_Line(TokenType'Image(t.token_type));
            return;
        end if;
        
        Parse_Data_Type(data_type);
        
        -- Now, parse the expression
        expr := Parse_Expression(T_SemiColon);
        
        lval := Create_Ast_Expression(AST_Id);
        lval.string_value := name;
        Create_Binary_Op(expr, lval, expr.rval.all);
        
        -- Create the statement
        stmt := Create_Ast_Statement(AST_Var);
        Set_Name(stmt, name);
        Set_Data_Type(stmt, data_type);
        Set_Expression(stmt, expr);
        Add_Statement(block, stmt);
    end Parse_Var_Dec;
    
    -- Main Parse body
begin
    t := Lex_Get_Next;
    while t.token_type /= T_End and t.token_type /= T_Eof loop
        case t.token_type is
            when T_Var => Parse_Var_Dec;
        
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
            
            if op.ast_type = AST_Assign then
                lval := Create_Ast_Expression(AST_None);
            else
                lval := stack.Last_Element;
                stack.Delete_Last;
            end if;
            
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
            when T_Assign => op_stack.Append(Create_Ast_Expression(AST_Assign));
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

--
-- Parses a data type token from the token stream
--
procedure Parse_Data_Type(data_type : in out DataType) is
    t : Token := Lex_Get_Next;
begin
    case t.token_type is
        when T_I8 => data_type := I8;
        when T_U8 => data_type := U8;
        
        when T_I16 => data_type := I16;
        when T_U16 => data_type := U16;
        
        when T_I32 => data_type := I32;
        when T_U32 => data_type := U32;
        
        when T_I64 => data_type := I64;
        when T_U64 => data_type := U64;
        
        when T_Char => data_type := Char;
        when T_String => data_type := Str;
        when T_Bool => data_type := Bool;
        
        when others =>
            Put_Line("Error: Invalid data type.");
            Put_Line(TokenType'Image(t.token_type));
            data_type := Void;
    end case;
end Parse_Data_Type;

end Parser; -- End body of Parser

