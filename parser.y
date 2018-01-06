%{
	#include "node.h"
        #include <cstdio>
        #include <cstdlib>
	NBlock *programBlock; /* the top level root node of our final AST */

	extern int yylex();
	void yyerror(const char *s) { std::printf("Error: %s\n", s);std::exit(1); }
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
%token <string> TIDENTIFIER TINTEGER TDOUBLE TCOMMENT TINT TVOID
%token <token> TCEQ TCNE TCLT TCLE TCGT TCGE TEQUAL
%token <token> TLPAREN TRPAREN TLBRACE TRBRACE TCOMMA TDOT TSEMICOLON
%token <token> TPLUS TMINUS TMUL TDIV
%token <token> TRETURN TIF TELSE TWHILE

/* Define the type of node our nonterminal symbols represent.
   The types refer to the %union declaration above. Ex: when
   we call an ident (defined by union type ident) we are really
   calling an (NIdentifier*). It makes the compiler happy.
 */
%type <ident> ident type_specifier myvoid 
%type <expr> expression numeric 
%type <varvec> params param_list
%type <exprvec> args
%type <block> program stmts declaration_list
%type <stmt> stmt declaration var_declaration com_declaration fun_declaration expression_stmt selection_stmt iteration_stmt return_stmt param
%type <token> comparison

/* Operator precedence for mathematical operators */
%left TPLUS TMINUS
%left TMUL TDIV


%start program

%%

program: declaration_list { programBlock = $1; }
	;
		
declaration_list: declaration_list declaration { $1->statements.push_back($<stmt>2);} 
	| declaration { $$ = new NBlock(); $$->statements.push_back($<stmt>1);}
	;

declaration: var_declaration
    | fun_declaration 
    | com_declaration
	;

var_declaration: type_specifier ident TSEMICOLON { $$ = new NVariableDeclaration(*$1, *$2); }
	;

type_specifier: TINT { $$ = new NIdentifier(*$1); }
    | TVOID { $$ = new NIdentifier(*$1); }
    ;

fun_declaration: type_specifier ident TLPAREN params TRPAREN TLBRACE stmts TRBRACE { $$ = new NFunctionDeclaration(*$1, *$2, *$4, *$7); }
    ;

params: param_list
    | myvoid { $$ = new VariableList(); $$->push_back($<var_decl>1);}
    ;

myvoid: TVOID { $$ = new NIdentifier(*$1); }
    ;

param_list: param_list TCOMMA param { $$ = $1; $$->push_back($<var_decl>3); }
    | param { $$ = new VariableList(); $$->push_back($<var_decl>1); }
    ;

param: type_specifier ident { $$ = new NVariableDeclaration(*$1,*$2); }
    ;

stmts: stmt { $$ = new NBlock(); $$->statements.push_back($<stmt>1); }
    | stmts stmt { $1->statements.push_back($<stmt>2); }
	| stmts com_declaration { $$ = $1; } 
    ;

stmt: var_declaration 
    | expression_stmt 
    | TLBRACE stmt TRBRACE { $$ = $2; }
    | selection_stmt
    | iteration_stmt
    | return_stmt
    | /*空*/ { $$ = new NStatement();}
    ;

expression_stmt: expression TSEMICOLON { $$ = new NExpressionStatement(*$1); }
    | TSEMICOLON { $$ = new NStatement(); }
    ;

selection_stmt: TIF TLPAREN expression TRPAREN stmt { $$ = new NSelectionStatement(*$3,*$5); }
    | TIF TLPAREN expression TRPAREN stmt TELSE stmt { $$ = new NSelectionStatement(*$3,*$5,*$7); } 
    ;

iteration_stmt: TWHILE TLPAREN expression TRPAREN stmt { $$ = new NIterationStatement(*$3,*$5); }
    ;

return_stmt: TRETURN TSEMICOLON { $$ = new NReturnStatement(); }
    | TRETURN expression TSEMICOLON {$$ = new NReturnStatement(*$2); }
    ;

expression: ident TEQUAL expression { $$ = new NAssignment(*$1,*$3); }
    | ident TLPAREN args TRPAREN {  $$ = new NMethodCall(*$1, *$3); }
    | ident { $<ident>$ = $1; }
	| numeric
	| expression comparison expression { $$ = new NBinaryOperator(*$1, $2, *$3); }
	| TLPAREN expression TRPAREN { $$ = $2; }
	;

args: args TCOMMA expression { $1->push_back($3); }
    | /*空*/ { $$ = new ExpressionList(); }
    | expression { $$ = new ExpressionList(); $$->push_back($1); }
    ;

comparison: TCEQ | TCNE | TCLT | TCLE | TCGT | TCGE 
	| TPLUS | TMINUS | TMUL | TDIV
	;

ident: TIDENTIFIER { $$ = new NIdentifier(*$1); delete $1; }
    ;

numeric: TINTEGER { $$ = new NInteger(atol($1->c_str())); }
	;

com_declaration: TCOMMENT { $$ = new NStatement(); }
    ;
%%
