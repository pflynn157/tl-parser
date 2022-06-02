FILE: test/parser/003.tl

FUNC sayHello is
   AST_CALL_STMT println VOID "Hello!"
end

FUNC main is
   AST_CALL_STMT sayHello VOID 
end

