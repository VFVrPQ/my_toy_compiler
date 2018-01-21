%{
	#include "node.h"
        #include <cstdio>
        #include <cstdlib>
	NBlock *programBlock; /* the top level root node of our final AST */

    extern int yylineno;
	extern int yylex();
	void yyerror(const char *s) { std::printf("Error: %s in line %d\n", s,yylineno);std::exit(1); }
%}

/* Represents the many different ways we can access our data */
%union {
	Node *node;
	NBlock *block;
	NExpression *expr;
	NStatement *stmt;
	NIdentifier *ident;
	NVariableDeclaration *var_decl;
	std::vector<NVariableDeclaration*> *varvec;
	std::vector<NExpression*> *exprvec;
	std::string *string;
	int token;
}

/* Define our terminal symbols (tokens). This should
   match our tokens.l lex file. We also define the node type
   they represent.
 */
%token <string> TIDENTIFIER TINTEGER TDOUBLE TCOMMENT TINT
%token <token> TCEQ TCNE TCLT TCLE TCGT TCGE TEQUAL
%token <token> TLPAREN TRPAREN TLBRACE TRBRACE TCOMMA TDOT TSEMICOLON
%token <token> TPLUS TMINUS TMUL TDIV
%token <token> TRETURN TEXTERN TIF TELSE TWHILE

/* Define the type of node our nonterminal symbols represent.
   The types refer to the %union declaration above. Ex: when
   we call an ident (defined by union type ident) we are really
   calling an (NIdentifier*). It makes the compiler happy.
 */
%type <ident> ident type_specifier
%type <block> program declaration_list stmts
%type <varvec> params param_list
%type <exprvec> args
%type <stmt>  declaration var_declaration extern_declaration com_declaration fun_declaration
%type <stmt> stmt expression_stmt return_stmt param selection_stmt iteration_stmt
%type <expr> expression numeric
%type <token> comparison

/* Operator precedence for mathematical operators */
%right TEQUAL
%left TCEQ TCNE TCLT TCLE TCGT TCGE
%left TPLUS TMINUS
%left TMUL TDIV

%start program

%%

program: declaration_list { programBlock = $1; }
	;
		
declaration_list: declaration_list declaration { $1->statements.push_back($<stmt>2); $$ = $1; } 
	| declaration { $$ = new NBlock(); $$->statements.push_back($<stmt>1); }
    ;
    
declaration: var_declaration 
    | extern_declaration
    | com_declaration
    | fun_declaration
    ;

var_declaration: type_specifier ident TSEMICOLON { $$ = new NVariableDeclaration(*$<ident>1, *$2); }
	| type_specifier ident {std::printf("missing ';' in line %d",yylineno);std::exit(0);}
	;

extern_declaration: TEXTERN type_specifier ident TLPAREN params TRPAREN TSEMICOLON { $$ = new NExternDeclaration(*$2, *$3, *$5); }
	|TEXTERN type_specifier ident TLPAREN params TRPAREN { std::printf("missing ';' in line %d",yylineno);std::exit(0);}
    ;

com_declaration: TCOMMENT { $$ = new NStatement(); }
    ;

fun_declaration: type_specifier ident TLPAREN params TRPAREN TLBRACE stmts TRBRACE { $$ = new NFunctionDeclaration(*$1, *$2, *$4, *$7); }
	;

params: param_list
    | /*blank*/ { $$ = new VariableList(); }
    ;
    

param_list: param_list TCOMMA param { $$ = $1; $$->push_back($<var_decl>3); }
    | param { $$ = new VariableList(); $$->push_back($<var_decl>1); }
    ;

param: type_specifier ident { $$ = new NVariableDeclaration(*$1,*$2); }
    ;

stmts: stmt { $$ = new NBlock(); $$->statements.push_back($<stmt>1); }
    | stmts stmt { $1->statements.push_back($<stmt>2); $$ = $1; }
    | TLBRACE stmts TRBRACE { $$ = $2; }
    | stmts TLBRACE stmts TRBRACE { 
        $$ = $1; 
        for (int i=0;i<($3->statements).size();i++){
            $$->statements.push_back($3->statements[i]);
        }
        delete $3;
    }
    ;


stmt: var_declaration 
    | expression_stmt 
    | return_stmt
    | selection_stmt
    | iteration_stmt
    | com_declaration
    | /*空*/ { $$ = new NStatement();}
    ;

expression_stmt: expression TSEMICOLON { $$ = new NExpressionStatement(*$1); }
    | TSEMICOLON { $$ = new NStatement(); }
    ;

selection_stmt: TIF TLPAREN expression TRPAREN TLBRACE stmts TRBRACE TELSE TLBRACE stmts TRBRACE { $$ = new NSelectionStatement(*$3,*$6,*$10); } 
    | TIF TLPAREN expression TRPAREN TLBRACE stmts TRBRACE {printf("select_stmt2\n");$$ = new NSelectionStatement(*$3,*$6); }
    ;

iteration_stmt: TWHILE TLPAREN expression TRPAREN TLBRACE stmts TRBRACE { $$ = new NIterationStatement(*$3,*$<block>6); }
    ;

return_stmt: TRETURN TSEMICOLON { $$ = new NReturnStatement(); }
    | TRETURN expression TSEMICOLON {$$ = new NReturnStatement(*$2); }
    ;

expression: ident TEQUAL expression { $$ = new NAssignment(*$1, *$3); }
    | ident TLPAREN args TRPAREN {  $$ = new NMethodCall(*$1, *$3); }
    | ident { $<ident>$ = $1; }
	| numeric 
	| expression comparison expression { $$ = new NBinaryOperator(*$1, $2, *$3); }
    | expression TPLUS expression { $$ = new NBinaryOperator(*$1, $2, *$3); }
    | expression TMINUS expression { $$ = new NBinaryOperator(*$1, $2, *$3); }
    | expression TMUL expression { $$ = new NBinaryOperator(*$1, $2, *$3); }
    | expression TDIV expression { $$ = new NBinaryOperator(*$1, $2, *$3); }
	| TLPAREN expression TRPAREN { $$ = $2; }
    | TMINUS expression { $$ = new NBinaryOperator(0, $1, *$2); }
	;
    
numeric: TINTEGER { $$ = new NInteger(atol($1->c_str())); }
	;

args: args TCOMMA expression { $1->push_back($3); }
    | /*空*/ { $$ = new ExpressionList(); }
    | expression { $$ = new ExpressionList(); $$->push_back($1); }
    ;

comparison: TCEQ | TCNE | TCLT | TCLE | TCGT | TCGE 
	;

type_specifier: TIDENTIFIER { $$ = new NIdentifier(*$1); }
    ;

ident: TIDENTIFIER { $$ = new NIdentifier(*$1); }
    ;


%%
