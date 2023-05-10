%{
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include "y.tab.h"
%}
%option noyywrap
%option yylineno
%%
     /*Keywords Lexemes*/
"bool"		{yylval.string = strdup(yytext);return BOOL;}
"char"		{yylval.string = strdup(yytext);return CHAR;}
"int"		      {yylval.string = strdup(yytext);return INT;}
"real"		{yylval.string = strdup(yytext);return REAL;}
"string"		{yylval.string = strdup(yytext);return STRING;}
"int*"		{yylval.string = strdup(yytext);return INT_P;}
"char*"		{yylval.string = strdup(yytext);return CHAR_P;}
"real*"		{yylval.string = strdup(yytext);return REAL_P;}
"if"			{yylval.string = strdup(yytext);return IF;}
"else"		{yylval.string = strdup(yytext);return ELSE;}
"while"		{yylval.string = strdup(yytext);return WHILE;}
"for"			{yylval.string = strdup(yytext);return FOR;}
"return"		{yylval.string = strdup(yytext);return RETURN;}
"void"		{yylval.string = strdup(yytext);return VOID;}
"do"			{yylval.string = strdup(yytext);return DO;}
"var"			{yylval.string = strdup(yytext);return VAR;}
"null"		{yylval.string = strdup(yytext);return NONE;}
      /*Operator Lexemes*/
"&&"		{yylval.string = strdup(yytext);return AND;}
"/"		{yylval.string = strdup(yytext);return DIV;}
"="		{yylval.string = strdup(yytext);return ASS;}
"=="		{yylval.string = strdup(yytext);return EQ;}
">"		{yylval.string = strdup(yytext);return BIGGER;}
">="		{yylval.string = strdup(yytext);return BIGGER_EQ;}
"<"		{yylval.string = strdup(yytext);return SMALLER;}
"<="		{yylval.string = strdup(yytext);return SMALLER_EQ;}
"-"		{yylval.string = strdup(yytext);return SUB;}
"!"		{yylval.string = strdup(yytext);return NOT;}
"!="		{yylval.string = strdup(yytext);return NOT_EQ;}
"||"		{yylval.string = strdup(yytext);return OR;}
"+"		{yylval.string = strdup(yytext);return ADD;}
"*"		{yylval.string = strdup(yytext);return MUL;}
"&"		{yylval.string = strdup(yytext);return ADDRESS;}
      /*Literal Lexemes*/
"true"		 		{yylval.string = strdup(yytext);return TRUE;}
"false"				{yylval.string = strdup(yytext);return FALSE;}
[a-zA-Z]+[_0-9a-zA-Z]*		{yylval.string = strdup(yytext);return ID;}
([+-]?[0-9]+"."[0-9]+) {yylval.string = strdup(yytext);return REALS;}
([+-]?[0-9]*"."?[0-9]+["E""e"][+-]?[0-9]+) {yylval.string = strdup(yytext);return EXPONENT;}
0|[1-9][0-9]*  {yylval.string = strdup(yytext);return INTEGERS;}
[+-]?0[xX][0-9A-F]+ {yylval.string = strdup(yytext);return HEX;}
\'(\\.|[^"\\])\'			{yylval.string = strdup(yytext);return CHAR_SIGN;}
\"(\\.|[^"\\])*\"			{yylval.string = strdup(yytext);return STRING_PHRASE;}
    /*Other Lexemes*/
";"		{yylval.string = strdup(yytext);return ENDLINE;}
"{"         {yylval.string = strdup(yytext);return SBLOCK;}
"}"         {yylval.string = strdup(yytext);return EBLOCK;}
"/*"((("*"[^/])?)|[^*])*"*/"
"\n"		{;}
[ \t]		{;}
.		return yytext[0];
%%
