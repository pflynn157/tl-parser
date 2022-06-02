with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package AST is

    --
    -- AST types
    --
    type AstType is (
        AST_None,
        
        -- Statements
        AST_Var,
        AST_Return,
        AST_Call_Stmt,
        
        -- Expressions
        -- Operators
        AST_Assign,
        AST_Add,
        AST_Sub,
        AST_Mul,
        AST_Div,
        
        -- Literals
        AST_Id,
        AST_Int,
        AST_String
    );
    
    type DataType is (
        Void,
        I8, U8,
        I16, U16,
        I32, U32,
        I64, U64,
        Char, Str,
        Bool
    );

    --
    -- Expression representation
    --
    type AstExpression;
    type AstExprObj is access AstExpression;
    type AstExpression is record
        ast_type : AstType := AST_None;
        lval, rval : AstExprObj;
        
        int_value : integer := 0;
        string_value : Unbounded_String;
    end record;

    --
    -- Statement representation
    --
    type AstStatement is record
        ast_type : AstType := AST_None;
        expr : AstExpression;
        data_type : DataType := Void;
        name : Unbounded_String;
    end record;
    
    package AstStmtVector is new Ada.Containers.Vectors
        ( Index_Type => Natural,
          Element_Type => AstStatement);
          
    --
    -- Block representation
    --
    type AstBlock is record
        statements : AstStmtVector.Vector;
    end record;

    --
    -- Function representation
    --
    type AstFunction is record
        name : Unbounded_String;
        block : AstBlock;
    end record;

    package AstFuncVector is new Ada.Containers.Vectors
        ( Index_Type => Natural,
          Element_Type => AstFunction);

    --
    -- The file representation
    --
    type AstFile is record
        name : Unbounded_String;
        funcs : AstFuncVector.Vector;
    end record;
    
    --
    -- Functions
    --
    function Has_Expression(stmt : AstStatement) return boolean;
    procedure Create_Binary_Op(op : in out AstExpression; lval, rval : AstExpression);
    procedure Set_Expression(stmt : in out AstStatement; expr : AstExpression);
    procedure Set_Name(stmt : in out AstStatement; name : Unbounded_String);
    procedure Set_Data_Type(stmt : in out AstStatement; data_type : DataType);
    procedure Add_Statement(block : in out AstBlock; stmt : AstStatement);
    procedure Add_Statement(func : in out AstFunction; stmt : AstStatement);
    procedure Add_Function(Self: in out AstFile; ast_func : AstFunction);
    procedure Print_Ast(file : AstFile);
    
    -- Helper functions
    function Create_Ast_File(name : string) return AstFile;
    function Create_Ast_Function(name : string) return AstFunction;
    function Create_Ast_Statement(ast_type : AstType) return AstStatement;
    function Create_Ast_Expression(ast_type : AstType) return AstExpression;

end AST; -- End AST definition

