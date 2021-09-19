
%{ 

	int quotes = 0;

%} 
  

%% 
[aA][nN][dD] {return 0;}
[oO][rR] {return 1;}
[nN][oO][tT] {return 2;}
[eE][qQ][uU][aA][lL] {return 3;}
[lL][eE][sS][sS] {return 4;}
[nN][iI][lL] {return 5;}
[lL][iI][sS][tT] {return 6;}
[aA][pP][pP][eE][nN][dD] {return 7;}
[cC][oO][nN][cC][aA][tT] {return 8;}
[sS][eE][tT] {return 9;}
[dD][eE][fF][fF][uU][nN] {return 10;}
[fF][oO][rR] {return 11;}
[iI][fF] {return 12;}
[eE][xX][iI][tT] {return 13;}
[lL][oO][aA][dD] {return 14;}
[dD][iI][sS][pP] {return 15;}
[tT][rR][uU][eE] {return 16;}
[fF][aA][lL][sS][eE] {return 17;}

[+] {return 18;}
[-] {return 19;}
[/] {return 20;}
[*] {return 21;}
[(] { return 22;} 
[)] { return 23;}
[*][*] {return 24;}
["] {	if(quotes % 2 == 0){
			quotes++;
			return 25;	
		}else{
			quotes++;
			return 26;
		}  
	}

[,] {return 27;}

[;][;][.]* {return 28;}

[0]|[1-9][0-9]* {return 29;}

[A-Za-z_][a-zA-Z0-9_]* {return 30;}


[ \t]+   {return 31;} 
\n 	   {return 32;}
<<EOF>>		{return 33;}



[0-9]+[A-Za-z_]+ {return 34;}
[0][0-9]+ {return 34;}

. {return 34;}
%% 



void lexer(FILE *wp){


	char* token_names[35] = {
		"KW_AND","KW_OR","KW_NOT","KW_EQUAL","KW_LESS","KW_NIL","KW_LIST","KW_APPEND","KW_CONCAT",
		"KW_SET","KW_DEFFUN","KW_FOR","KW_IF","KW_EXIT","KW_LOAD","KW_DISP","KW_TRUE","KW_FALSE",
		"OP_PLUS","OP_MINUS","OP_DIV","OP_MULT","OP_OP","OP_CP","OP_DBLMULT","OP_OC","OP_CC","OP_COMMA",
		"COMMENT","VALUE","IDENTIFIER","Space","Newline","EOF","ERROR"
	}; 

	int token_num;
	int prev_token = 0;
	
	do{
		token_num = yylex();
		if(token_num < 31){

			
			fprintf(wp, "%s\n",token_names[token_num]);	
						

			prev_token = token_num;
		
			if(token_num == 28){
				while(token_num != 32 && token_num != 33){
					token_num = yylex();
				}
			}	
		}
		else if(token_num == 34){
			fprintf(wp, "SYNTAX_ERROR %s cannot be tokenized\n",yytext);	
			
		}
		
	}while(token_num>=0 && token_num <33 || token_num == 34);

}
  

int yywrap(){} 
int main(int argc, char *argv[]){ 
  	
  	FILE *wp = fopen("parsed_cpp.txt","w");
	
	if(argc > 2){
		printf("Wrong input");
		return 0;
	}
	if(argc == 2){ // file mode
	
		FILE *fp;
		fp = fopen(argv[1],"r");
		
		if(fp == NULL){
			printf("ERROR file cannot found");
			exit(0);
		}
		yyin = fp;

		lexer(wp);
		fclose(fp);
	}
	else{
	
		char interpreter[1024];
		while(1){

			printf("> ");
			int a = scanf("%[^\n]", interpreter);
			getchar();
			if(a == 0){
				fclose(wp);
				return 0;	
			} 
			yy_scan_string(interpreter);
			lexer(wp);

			
		}

	}

	

	return 0; 
} 