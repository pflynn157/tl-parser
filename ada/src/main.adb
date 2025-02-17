--
-- This software is licensed under BSD0 (public domain).
-- Therefore, this software belongs to humanity.
-- See COPYING for more info.
--
with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ast; use ast;
with parser; use parser;
with lex; use lex;
with unwriter; use unwriter;

procedure Main is

    -- Command line control variables
    output_lex, output_ast : boolean := false;
    input_file : Unbounded_String;
    
    procedure Lex_Test is
        t : Token;
    begin
        Lex_Init(To_String(input_file));
        t := Lex_Get_Next;
        while t.token_type /= T_Eof loop
            Put_Line(TokenType'Image(t.token_type));
            t := Lex_Get_Next;
        end loop;
        Lex_Close;
    end Lex_Test;
    
    -- The ast file
    ast_file : AstFile;
begin
    if Argument_Count >= 1 then
        for i in 1 .. Argument_Count loop
            if Argument(i) = "--lex" then
                output_lex := true;
            elsif Argument(i) = "--ast" then
                output_ast := true;
            else
                input_file := To_Unbounded_String(Argument(i));
            end if;
        end loop;
    end if;
    
    if not output_lex then
        ast_file := Parse(To_String(input_file));
    end if;

    -- Print as dictated
    if output_lex then
        Put_Line("========");
        Lex_Test;
        Put_Line("========");
    elsif output_ast then
        Print_Ast(ast_file);
    else
        unwrite(ast_file);
    end if;
end Main;

