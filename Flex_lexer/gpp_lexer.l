%{ 
    /* DEFINITION PART */
    #include <stdio.h>
    #include <stdlib.h>
    #include <ctype.h>
    #include <string.h>
%}

COMMENT ";;".*
VAL [1-9][0-9]*|"0"
IDENTIFIER [a-zA-Z][a-zA-Z0-9]*
FLOAT_P [0-9]+"."[0-9]*
EXIT "(exit)"

%% 
    /* RULES PART */
{EXIT} {fprintf(yyout, "OP_OP\n");
    fprintf(yyout, "KW_EXIT\n");
    fprintf(yyout, "OP_CP\n");
    return 0;}
{COMMENT} {fprintf(yyout, "COMMENT\n");}
and {fprintf(yyout,"KW_AND\n");}
or {fprintf(yyout,"KW_OR\n");}
not {fprintf(yyout,"KW_NOT\n");}
equal {fprintf(yyout,"KW_EQUAL\n");}
less {fprintf(yyout,"KW_LESS\n");}
nil {fprintf(yyout,"KW_NIL\n");}
list {fprintf(yyout,"KW_LIST\n");}
append {fprintf(yyout,"KW_APPEND\n");}
concat {fprintf(yyout,"KW_CONCAT\n");}
set {fprintf(yyout,"KW_SET\n");}
deffun {fprintf(yyout,"KW_DEFFUN\n");}
for {fprintf(yyout,"KW_FOR\n");}
if {fprintf(yyout,"KW_IF\n");}
exit {fprintf(yyout,"KW_EXIT\n");}
load {fprintf(yyout,"KW_LOAD\n");}
disp {fprintf(yyout,"KW_DISP\n");}
true {fprintf(yyout,"KW_TRUE\n");}
false {fprintf(yyout,"KW_FALSE\n");}
"+" {fprintf(yyout,"OP_PLUS\n");}
"-" {fprintf(yyout,"OP_MINUS\n");}
"/" {fprintf(yyout,"OP_DIV\n");}
"*" {fprintf(yyout,"OP_MULT\n");}
"(" {fprintf(yyout,"OP_OP\n");}
")" {fprintf(yyout,"OP_CP\n");}
"**" {fprintf(yyout,"OP_DBLMULT\n");}
"`" {}
"'" {}
"\"" {
static int  temp = 0;   // Open curly bracket, close curl bracket tracing
if(temp == 0){fprintf(yyout,"OP_OC\n"); temp = 1;}else{fprintf(yyout,"OP_CC\n");temp = 0;}}
"," {fprintf(yyout,"OP_COMMA\n");}
{VAL}|{FLOAT_P} {fprintf(yyout,"VALUE\n");}
{IDENTIFIER} {fprintf(yyout,"IDENTIFIER\n");}
[0-9][0-9a-zA-Z]* {fprintf(yyout,"ERROR\n");}
" "|"\t" { } 
.	 {fprintf(yyout,"ERROR\n");} 
^\n                     {exit(1);}
"\n"                    {printf("\n> ");}

%% 

int yywrap(){} 

int main(int argc, char **argv)
{
    FILE *fp;
    extern FILE *yyout;
    yyout = fopen("parsed_cpp.txt", "w");
    if(1 < argc) {
        printf("\n> ");
        fp = fopen(argv[1], "r");
        yyin = fp;
        yylex();
    }

    printf("\n> ");
    yylex();
    fclose(yyout);
    fclose(fp);

    return 0;
}