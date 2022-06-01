with Ada.Text_IO; use Ada.Text_IO;

with Ast; use Ast;
with Parser; use Parser;
with Lex; use Lex;

procedure Main is
    
    procedure Lex_Test is
        t : Token;
    begin
        Lex_Init("first.tl");
        t := Lex_Get_Next;
        while t.token_type /= T_Eof loop
            Put_Line(TokenType'Image(t.token_type));
            t := Lex_Get_Next;
        end loop;
        Lex_Close;
    end Lex_Test;
    
    ast_file : AstFile := Parse("first.tl");
begin
    --Put_Line("========");
    --Lex_Test;
    --Put_Line("========");
    
    Print_Ast(ast_file);
end Main;

