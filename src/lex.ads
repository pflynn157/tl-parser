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
        
        -- Symbols
        T_SemiColon,
        T_Add,
        T_Sub,
        T_Mul,
        T_Div,
        
        -- Literals
        T_Id,
        T_Int
    );
    
    -- The structure
    type Token is record
        token_type : TokenType := T_None;
        string_value : Unbounded_String;
        int_value : integer := 0;
    end record;
    
    -- Functions
    procedure Lex_Init(Path : String);
    procedure Lex_Close;
    function Lex_Get_Next return Token;
end Lex;
