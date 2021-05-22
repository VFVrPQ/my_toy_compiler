%{
#define MAXTOKENSIZE 50
typedef int TokenType;
typedef struct
{
  TokenType tok;
  char * tokString;
} TokenStruct;

typedef enum {IfK,WhileK,AssignK,ReturnK,CallK,
VarDeclK,FunDeclK,OpK,ConstK,IdK} NodeKind;

typedef enum {Void,Integer,Boolean} ExpType;
#define MAXCHILDREN 3
typedef struct treeNode
{ struct treeNode * child[MAXCHILDREN];
  struct treeNode * sibling;
  int lineno;
  NodeKind kind;
  union { TokenType op;
    int val;
    char * name; } attr;
  ExpType type;
 } TreeNode;
%}

%{
#include <string>
#include "node.h"
#include "parser.hpp"

#define SAVE_TOKEN  yylval.string = new std::string(yytext, yyleng)
#define TOKEN(t)    (yylval.token = t)
#define SAVE_LINE   yylval.yylineno = yylineno

%}

%option noyywrap
%option yylineno
%%

[ \t\r\n]					      ;
"extern"                        return TOKEN(TEXTERN);
"return"				        return TOKEN(TRETURN);
"else"                          return TOKEN(TELSE);
"if"                            return TOKEN(TIF);
"while"                         return TOKEN(TWHILE);

[a-zA-Z_][a-zA-Z0-9_]*          printf("lineno=%d\n",yylineno);SAVE_TOKEN; return TIDENTIFIER;
[0-9]+\.[0-9]* 			        SAVE_TOKEN; return TDOUBLE;
[0-9]+					        SAVE_TOKEN; return TINTEGER;
"/*"(([^\*]*(\*[^\/])?)*)"*/"   SAVE_TOKEN; return TCOMMENT;

"="						        return TOKEN(TEQUAL);
"=="				          	return TOKEN(TCEQ);
"!="			          		return TOKEN(TCNE);
"<"				          		return TOKEN(TCLT);
"<="	          				return TOKEN(TCLE);
">"				          		return TOKEN(TCGT);
">="					        return TOKEN(TCGE);

"("	          					return TOKEN(TLPAREN);
")"					          	return TOKEN(TRPAREN);
"{"         				    return TOKEN(TLBRACE);
"}"					          	return TOKEN(TRBRACE);

"."         					return TOKEN(TDOT);
","				          		return TOKEN(TCOMMA);
";"                             return TOKEN(TSEMICOLON);

"+"				          		return TOKEN(TPLUS);
"-"		          				return TOKEN(TMINUS);
"*"		          				return TOKEN(TMUL);
"/"				          		return TOKEN(TDIV);

.               printf("Unknown token:$$$%s$$$:%d\n",yytext,yylineno); yyterminate();

%%
