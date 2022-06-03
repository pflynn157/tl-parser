with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers;        use Ada.Containers;
with Ada.Containers.Vectors;

package body Ast is

--
-- Variable class functions
--
function Has_Expression(stmt : AstStatement) return boolean is
begin
    if stmt.expr.ast_type = AST_None then
        return false;
    end if;
    return true;
end Has_Expression;

-- This function is local to the next two functions
function Create_Expr_Obj(item : AstExpression) return AstExprObj is
    item_obj : AstExprObj := new AstExpression'(
        ast_type => item.ast_type,
        lval => item.lval,
        rval => item.rval,
        int_value => item.int_value,
        string_value => item.string_value,
        char_value => item.char_value,
        list => item.list,
        list_size => item.list_size
    );
begin
    return item_obj;
end Create_Expr_Obj;

procedure Create_Binary_Op(op : in out AstExpression; lval, rval : AstExpression) is
    lval_obj : AstExprObj := Create_Expr_Obj(lval);
    rval_obj : AstExprObj := Create_Expr_Obj(rval);
begin
    op.lval := lval_obj;
    op.rval := rval_obj;
end Create_Binary_Op;

procedure Add_List_Item(op : in out AstExpression; item : AstExpression) is
    item_obj : AstExprObj := Create_Expr_Obj(item);
begin
    op.list(op.list_size) := item_obj;
    op.list_size := op.list_size + 1;
end Add_List_Item;

procedure Set_Expression(stmt : in out AstStatement; expr : AstExpression) is
begin
    stmt.expr := expr;
end Set_Expression;

procedure Set_Name(stmt : in out AstStatement; name : Unbounded_String) is
begin
    stmt.name := name;
end Set_Name;

procedure Set_Data_Type(stmt : in out AstStatement; data_type : DataType) is
begin
    stmt.data_type := data_type;
end Set_Data_Type;

procedure Add_Branch(block : in out AstBlock; stmt : AstStatement) is
begin
    block.branches.Append(stmt);
end Add_Branch;

procedure Add_Statement(block : in out AstBlock; stmt : AstStatement) is
begin
    block.statements.Append(stmt);
end Add_Statement;

procedure Add_Statement(func : in out AstFunction; stmt : AstStatement) is
begin
    Add_Statement(func.block, stmt);
end Add_Statement;

procedure Add_Function(Self: in out AstFile; ast_func : AstFunction) is
begin
    Self.funcs.Append(ast_func);
end Add_Function;

--
-- The main debug function
--
procedure Print_Ast(file : AstFile) is

    procedure Print(block : AstBlock; indent : integer);
    procedure Print(expr : AstExpression);
    
    procedure Print(expr : AstExprObj) is
        expr2 : AstExpression := expr.all;
    begin
        Print(expr2);
    end Print;
    
    procedure Print(expr : AstExpression) is
    begin
        case expr.ast_type is
            when AST_Int => Put(expr.int_value, 0);
            when AST_String => Put('"' & To_String(expr.string_value) & '"');
            when AST_Char => Put("CHAR(" & expr.char_value & ")");
            when AST_Id => Put(To_String(expr.string_value));
            
            when AST_True => Put("TRUE");
            when AST_False => Put("FALSE");
            
            when AST_Expr_List =>
                Put("{");
                for i in 0 .. (expr.list_size - 1) loop
                    Print(expr.list(i));
                    if i < (expr.list_size - 1) then
                        Put(", ");
                    end if;
                end loop;
                Put("}");
            
            when AST_Assign |
                 AST_Add | AST_Sub | AST_Mul | AST_Div | AST_Mod |
                 AST_And | AST_Or | AST_Xor |
                 AST_Eq | AST_Ne | AST_Gt | AST_Ge | AST_Lt | AST_Le =>
                Put("(");
                Print(expr.lval);
                if expr.ast_type = AST_Assign then Put(" := ");
                elsif expr.ast_type = AST_Add then Put(" + ");
                elsif expr.ast_type = AST_Sub then Put(" - ");
                elsif expr.ast_type = AST_Mul then Put(" * ");
                elsif expr.ast_type = AST_Div then Put(" / ");
                elsif expr.ast_type = AST_Mod then Put(" % ");
                elsif expr.ast_type = AST_And then Put(" & ");
                elsif expr.ast_type = AST_Or then Put(" | ");
                elsif expr.ast_type = AST_Xor then Put(" ^ ");
                elsif expr.ast_type = AST_Eq then Put(" = ");
                elsif expr.ast_type = AST_Ne then Put(" != ");
                elsif expr.ast_type = AST_Gt then Put(" > ");
                elsif expr.ast_type = AST_Ge then Put(" >= ");
                elsif expr.ast_type = AST_Lt then Put(" < ");
                elsif expr.ast_type = AST_Le then Put(" <= ");
                end if;
                Print(expr.rval);
                Put(")");
            
            when others => Put(AstType'Image(expr.ast_type));
        end case;
    end Print;
    
    procedure Print(stmt : AstStatement; indent : integer := 0) is
    begin
        Put(AstType'Image(stmt.ast_type) & " " & To_String(stmt.name) & " " & DataType'Image(stmt.data_type) & " ");
        if stmt.expr.ast_type /= AST_None then
            Print(stmt.expr);
        end if;
        if stmt.ast_type = AST_While or stmt.ast_type = AST_Else then
            New_Line;
            Print(stmt.block.all, indent + 4);
        elsif stmt.ast_type = AST_If or stmt.ast_type = AST_Elif then
            New_Line;
            Print(stmt.block.all, indent + 4);
            for br of stmt.block.all.branches loop
                for i in 0 .. indent loop Put(" "); end loop;
                Print(br, indent);
            end loop;
        else
            New_Line;
        end if;
    end Print;
    
    procedure Print(block : AstBlock; indent : integer) is
    begin
        for stmt of block.statements loop
            for i in 0 .. indent loop Put(" "); end loop;
            Print(stmt, indent);
        end loop;
    end Print;
    
    procedure Print(func : AstFunction) is
    begin
        Put_Line("FUNC " & To_String(func.name) & " is");
        Print(func.block, 2);
        Put_Line("end");
        New_Line;
    end Print;
    
begin
    Put_Line("FILE: " & To_String(file.name));
    New_Line;
    for func of file.funcs loop
        Print(func);
    end loop;
end Print_Ast;

--
-- Helper functions
--

-- Creates an AST file
function Create_Ast_File(name : string) return AstFile is
    file : AstFile;
begin
    file.name := To_Unbounded_String(name);
    return file;
end Create_Ast_File;

-- Creates an AST function
function Create_Ast_Function(name : string) return AstFunction is
    func : AstFunction;
begin
    func.name := To_Unbounded_String(name);
    return func;
end Create_Ast_Function;

-- Creates an AST statement
function Create_Ast_Statement(ast_type : AstType) return AstStatement is
    stmt : AstStatement;
begin
    stmt.ast_type := ast_type;
    stmt.data_type := Void;
    return stmt;
end Create_Ast_Statement;

-- Creates an AST expression
function Create_Ast_Expression(ast_type : AstType) return AstExpression is
    expr : AstExpression;
begin
    expr.ast_type := ast_type;
    expr.list_size := 0;
    return expr;
end Create_Ast_Expression;

end Ast; -- End package body

