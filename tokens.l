%{
#include <string>
#include "node.h"
#include "parser.hpp"

#define SAVE_TOKEN  yylval.string = new std::string(yytext, yyleng)
#define TOKEN(t)    (yylval.token = t)
#define SAVE_LINE   yylval.yylineno = yylineno

int lineno = 0;
%}

%option noyywrap
%option yylineno
%%

[ \t\r]					        ;
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
"\n"       {lineno++;};

"/*"             { char c; 
                  char p='\0';
                  do
                  { c = getchar();
                    if (c == EOF) break;
                    if (c == '\n') lineno++;
                    if (p == '*' && c == '/') break;
                    p = c;
                  } while (1);
                }
                
.               printf("Unknown token:$$$%s$$$:%d\n",yytext,lineno); yyterminate();

%%
