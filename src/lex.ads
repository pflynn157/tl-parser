with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Lex is
    -- Types
    type TokenType is (
        T_None,
        T_Eof,
        
        -- Keywords
        T_Func,
        T_Is,
        T_End,
        T_Return,
        T_Var,
        
        -- Type keywords
        T_I8, T_U8,
        T_I16, T_U16,
        T_I32, T_U32,
        T_I64, T_U64,
        T_Char, T_String,
        T_Bool,
        
        -- Symbols
        T_LParen, T_RParen,
        T_SemiColon,
        T_Colon,
        T_Assign,
        T_Add,
        T_Sub,
        T_Mul,
        T_Div,
        T_Mod,
        T_And,
        T_Or,
        T_Xor,
        
        T_Eq, T_Ne,
        T_Gt, T_Ge,
        T_Lt, T_Le,
        
        -- Literals
        T_Id,
        T_Int,
        T_StringL,
        T_CharL,
        T_True,
        T_False
    );
    
    -- The structure
    type Token is record
        token_type : TokenType := T_None;
        string_value : Unbounded_String;
        int_value : integer := 0;
        char_value : character := ' ';
    end record;
    
    -- Functions
    procedure Lex_Init(Path : String);
    procedure Lex_Close;
    function Lex_Get_Next return Token;
end Lex;
