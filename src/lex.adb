with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Containers;         use Ada.Containers;
with Ada.Containers.Vectors;

package body Lex is

--
-- Global package variables
--
F : File_Type;

package TokenStack is new Ada.Containers.Vectors
    ( Index_Type => Natural,
      Element_Type => Token);
      
token_stack : TokenStack.Vector;

--
-- Init's the lexical analyzer
--
procedure Lex_Init(Path : string) is
begin
    Open(F, In_File, Path);
end Lex_Init;

procedure Lex_Close is
begin
    Close(F);
end Lex_Close;

--
-- The token getter function
--
function Lex_Get_Next return Token is
    t : Token := (T_None, To_Unbounded_String(""), 0);
    buffer : Unbounded_String := To_Unbounded_String("");
    c : character := ' ';
    
    -- A helper function to see if we have a symbol
    function Is_Symbol return boolean is
    begin
        case c is
            when ';' => return true;
            when '+' | '-' | '*' | '/' => return true;
            
            when others => return false;
        end case;
    end Is_Symbol;
    
    -- A helper function to see if we have an integer literal
    function Is_Int return Boolean is
        Dummy : Integer;
    begin
        Dummy := Integer'Value(To_String(buffer));
        return True;
    exception
        when others => return False;
    end Is_Int;
    
    -- A helper function to get the symbol based on the character
    procedure Get_Symbol is
    begin
        case c is
            when ';' => t.token_type := T_SemiColon;
            when '+' => t.token_type := T_Add;
            when '-' => t.token_type := T_Sub;
            when '*' => t.token_type := T_Mul;
            when '/' => t.token_type := T_Div;
            
            when others => null;
        end case;
    end Get_Symbol;
    
    -- A helper function to get the keyword based on the buffer
    procedure Get_Keyword is
    begin
        if buffer = "func" then t.token_type := T_Func;
        elsif buffer = "is" then t.token_type := T_Is;
        elsif buffer = "end" then t.token_type := T_End;
        elsif buffer = "return" then t.token_type := T_Return;
        else t.token_type := T_None;
        end if;
    end Get_Keyword;
    
    -- A helper function to proccess the buffer
    procedure Process_Buffer is
    begin
        Get_Keyword;
        
        if t.token_type = T_None then
            if Is_Int then
                t.token_type := T_Int;
                t.int_value := Integer'Value(To_String(buffer));
            else
                t.token_type := T_Id;
                t.string_value := buffer;
            end if;
        end if;
        
        buffer := To_Unbounded_String("");
    end Process_Buffer;
begin
    if token_stack.Length > 0 then
        t := token_stack.Last_Element;
        token_stack.Delete_Last;
        return t;
    end if;

    if End_Of_File(F) then
        if Length(buffer) > 0 then
            Process_Buffer;
        else
            t.token_type := T_Eof;
        end if;
        
        return t;
    end if;
    
    while not End_Of_File(F) loop
        Get_Immediate(F, c);
        if c = ' ' or c = LF or Is_Symbol then
            if Is_Symbol then
                Get_Symbol;
                if Length(buffer) = 0 then
                    return t;
                else
                    token_stack.Append(t);
                end if;
            end if;
            
            if Length(buffer) > 0 then
                Process_Buffer;
                return t;
            end if;
        else
            Append(buffer, c);
        end if;
    end loop;
    
    Process_Buffer;
    return t;
end Lex_Get_Next;

end Lex; -- End Lex package body

