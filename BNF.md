## c-minus BNF

> **说明**
> 暂时没有数组定义
> type_specifier可能会有问题
> 可能要先去除左递归？
> stmt = expr ;
> expression ： 是表达式，有值的
> 匹配注释
```
"/*"(([^\*]*(\*[^\/])?)*)"*/" COMMENT
```

> **TOKEN**
> TSEMICOLON TCOMMA
> TINT TVOID
> TLPAREN TRPAREN TLBRACE TRBRACE
> TIF TELSE TWHILE
> TECQ TCNE TCLT TCLE TCGT TCGE
> TPLUS | TMINUS
----
> **type**
> 1. ident : ID type_specifier myvoid var
> 2. expr : NUM expression simple_expression additive_expression term factor
> 3. varvec : params param_list
> 4. exprvec : args
> 5. block : program stmts declaration_list  
> 6. stmt : stmt declaration var_declaration com_declaration fun_declaration statement expression_stmt selection_stmt iteration_stmt return_stmt param
> 7. token : comparion addop mulop

> **BNF**
```

program : declaration_list { programBlock = $1; }
;

declaration_list : declaration_list declaration { $1->statements.push_back($<stmt>2);} 
| declaration { $$ = new NBlock(); $$->statements.push_back($<stmt>1);}
;

declaration : var_declaration
    | fun_declaration 
    | com_declaration
;

var_declaration : type_specifier ID TSEMICOLON { $$ = new NVariableDeclaration($1, *$2); }
;

type_specifier : TINT { $$ = new NIdentifier($1); }
    | TVOID { $$ = new NIdentifier($1); }
    ;

fun_declaration : type_specifier ID TLPAREN params TRPAREN TLBRACE stmts TRBRACE { $$ = new NFunctionDeclaration($1, *$2, *$4, *$7); delete $4;}
    ;

params : param_list
    | myvoid { $$ = new VariableList(); $$->push_back($<param>1);}
    ;

myvoid : TVOID { $$ = new NIdentifier($1); }
    ;

param_list : param_list TCOMMA param { $$ = $1; $$->push_back($<param>3); }
    | param { $$ = new VariableList(); $$->push_back($<param>1;)}
    ;

param : type_specifier ID { $$ = new NVariableDeclaration(*$1,*$2); }
    ;

stmts : stmt { $$ = new NBlock(); $$->statements.push_back($<stmt>1); }
    | stmts stmt { $1->statements.push_back($<stmt>2); }
    ;

stmt : var_declaration 
    | com_declaration
    | expression_stmt 
    | TLBRACE stmt TRBRACE { $$ = $2;}
    | selection_stmt
    | iteration_stmt
    | return_stmt
    | /*空*/ { $$ = new NStatement();}
    ;

expression_stmt : expression TSEMICOLON { $$ = new NExpressionStatement(*$1); }
    | TSEMICOLON { $$ = new NStatement(); }
    ;

selection_stmt : TIF TLPAREN expression TRPAREN statement { $$ = new NSelectionStatement(*$3,*$5); }
    | TIF TLPAREN expression TRPAREN statement TELSE statement { $$ = new NSelectionStatement(*$3,*$5,*$7); } 
    ;

iteration_stmt : TWHILE TLPAREN expression TRPAREN statement { $$ = new NIterationStatement(*$3,*$5); }
    ;

return_stmt : TRETURN TSEMICOLON { $$ = new NReturnStatement(); }
    | TRETURN expression TSEMICOLON {$$ = new NReturnStatement($<expression>2); }
    ;

expression : var TEQUAL expression { $$ = new NAssignment(*<ident>1,*$3); }
    | simple_expression
    ;

var : ID {$<ident>$ = $1; }
    ;

simple_expression : additive_expression comparision additive_expression { $$ = new NBinaryOperator(*$1, $2, *$3); }
    | additive_expression
    ;

additive_expression : additive_expression addop term
{ $$ = new NBinaryOperator(*$1,$2,*$3); }
    | term
    ;

addop : TPLUS | TMINUS ; 

term : term mulop factor {$$ = NBinaryOperator(*$1,$2,*$3);}
    | factor
    ;

factor : TLPAREN expression TRPAREN { $$ = $2; }
    | var
    | ID TLPAREN args TRPAREN {  $$ = new NMethodCall(*$1, *$3); delete $3; }
    | NUM
    ;

args : args TCOMMA expression { $1->push_back($3); }
    | /*空*/ { $$ = new ExpressionList(); }
    | expression { $$ = new ExpressionList(); $$->push_back($1); }
    ;
comparion : TECQ | TCNE | TCLT | TCLE | TCGT | TCGE ;

com_declaration : COMMENT { $$ = new NBlock(); }
    ;

```



