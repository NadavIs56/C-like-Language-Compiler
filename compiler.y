%{
#include<stdio.h>
#include<string.h>
#include <stdlib.h>

int yylex();
int yyerror();
extern int yylineno;
extern char* yytext;

typedef struct node
{
	char *token;
	struct node *left;
	struct node *right;
	char *type;
	int visit;
	char *code;
	char *var;
	char* begin;
	char* after;
	char* next_block;
	int location;
}node;
node* makenode(char *token, node *left, node *right, char* type);
char* GetNodeType(node* n);
void printtree(node *tree, int tab);

typedef struct LinkedList
{
	char *id;
	char *group;
	char *type;
	int parms_num;
	struct LinkedList *next;
	struct LinkedList *prev;
	struct LinkedList *scope;
}LinkedList;
LinkedList* BuildVarNode(char* i, char* g, char* t, LinkedList *sc);
LinkedList* BuildFuncNode(char* i, char* g, char* t, int p, LinkedList *sc);
void AddNode(LinkedList* tale, LinkedList* node);
void DeleteList(LinkedList* lst);
int IsInList(LinkedList* lst, char* i);

typedef struct Stack
{
	int maxsize;
	int top;
	struct LinkedList **scopes;
}Stack;
Stack* NewStack();
int IsEmpty(Stack* st);
int IsFull(Stack* st);
void Push(Stack* st, LinkedList* lst);
LinkedList* Peek(Stack* st);
LinkedList* GetByIndex(Stack* st, int index);
LinkedList* Pop(Stack* st);
int IsInStack(Stack* st, char* word);

void BuildSymbolTable(node* root);
void RunOnTree(node* node, LinkedList* global);
void PrintList(LinkedList* lst);
void PrintNode(LinkedList* node);
void GetParameters(node* node, char* v_type, LinkedList* scope);
void PrintStack(Stack* st);
void GetDeclarations(LinkedList* scope, node* node, char* v_type);
void CheckAbs(node* node);
void CheckStringDec(node* node);
char* UpdateOperatorType(node* node);
char* UpdateAssOperator(node* node);
void CheckAssignments(node* node);
char* CheckBool(node* node);
void CheckFunc(LinkedList* scope, node* node, char* word);
void CheckFuncCall(node* node, LinkedList* scope);
void CheckFuncCallAss(node* node, LinkedList* scope);
int CheckFuncCallParmsNum(node* node, LinkedList* scope, int count, LinkedList* func);
void CheckMain(LinkedList* scope);
void CheckGlobal(LinkedList* scope, char* word);

void PrintTree(node* node, int tab);
void PrintFunc(node* node, int tab);
void PrintArgs(node* node, int tab);
void PrintBody(node* node, int tab);
void PrintDeclarations(node* node, int tab);
void PrintAssignment(node* node, int tab);
void PrintExp(node* node, int tab);
void PrintIf(node* node, int tab);
void PrintWhile(node* node, int tab);
void PrintFor(node* node, int tab);
void PrintDoWhile(node* node, int tab);
void PrintFuncCall(node* node, int tab);
void UpdateVisit(node* node);

char* FreshVar();
char* FreshLable();
void SetLable(node* root, node* node, int flag);
void SetNext(node* node, char* loc, int flag);
void SetParmsVar(node* node);
void SetVar(node* node);
void SetCode(node* node);
int CalaParams(char* func_name);
void Params(node* node);
char* ShortCircuit(node* node, char* truel, char* falsel);

void itoa(int n, char s[]);
void reverse(char s[]);

Stack* st;
LinkedList* scope;
LinkedList* global;
int freshvar = 0;
int freshlable = 1;
char* after;
%}

%union{
	char *string;	
	struct node *node;
	struct LinkedList *List;
	struct Stack *Stack;
}

%token<string> CHAR BOOL INT REAL STRING INT_P CHAR_P REAL_P IF ELSE WHILE FOR VAR RETURN
%token<string> VOID DO AND DIV ASS EQ BIGGER BIGGER_EQ SMALLER SMALLER_EQ SUB NOT NOT_EQ OR
%token<string> ADD MUL ADDRESS TRUE FALSE REALS INTEGERS CHAR_SIGN STRING_PHRASE ENDLINE
%token<string> ID HEX EXPONENT NONE SBLOCK EBLOCK

%left OR 
%left AND NOT 
%left BIGGER BIGGER_EQ SMALLER SMALLER_EQ ADDRESS EQ NOT_EQ
%left ADD, SUB
%left MUL, DIV

%type<node> s exp expr value func_call func_call2 func_call_tag func_call_parms
%type<node> var_type ass ass_type var_dec var_dectag str_dec var_dectag2 str_dectag var_dec_rec
%type<node> for_loop for_looptag while_loop while_tag cond cond2 cond3
%type<node> do_loop stm stm_block func func_body func_parms func_parmstag func_parms_rec
%type<node> return stm_block_tag2 code_block block block_tag main return2 string_ass
%%
s: main { $$ = makenode("CODE", $1, NULL, ""); BuildSymbolTable($$);}
main: func main { $$ = makenode("NEW_SCOPE", $1, $2, "");}|
			        {$$ = makenode("", NULL, NULL, "");}

stm: while_loop {$$ = $1;}| 
	  do_loop {$$ = $1;}|
     cond {$$ = $1;}| 
	  ass {$$ = $1;}| 
  	  var_dec {$$ = makenode("DECLARATIONS", $1, NULL, "");}| 
	  for_loop {$$ = $1;}|
     func_call {$$ = $1;}|
	 return {$$ = $1;}

stm_block: while_loop {$$ = makenode("SBT_WHILE",NULL,$1,"");}| 
	        do_loop {$$ = makenode("SBT_DO",NULL,$1,"");}|
           cond {$$ = makenode("SBT_COND",NULL,$1,"");}| 
	        for_loop {$$ = makenode("SBT_FOR",NULL,$1,"");}| 
           func_call {$$ = makenode("SBT_FUNC",NULL,$1,"");} 
stm_block_tag2: stm_block {$$ = makenode("",$1, NULL, "");}| 
					 ass {$$ = makenode("", $1, NULL, "");}

var_type: CHAR {$$ = makenode("char", NULL, NULL, "");}| 
          BOOL  {$$ = makenode("bool", NULL, NULL, "");}| 
          INT {$$ = makenode("int", NULL, NULL, "");}| 
		  REAL {$$ = makenode("real", NULL, NULL, "");}| 
          INT_P {$$ = makenode("int*", NULL, NULL, "");}| 
		  CHAR_P {$$ = makenode("char*", NULL, NULL, "");}| 
	      REAL_P {$$ = makenode("real*", NULL, NULL, "");}

value: ID {$$ = makenode($1, NULL, NULL, "id");}| 
	   ID '[' exp ']' {$$ = makenode($1, $3, NULL, "id");}|
       INTEGERS {$$ = makenode($1, NULL, NULL, "int");}| 
       REALS {$$ = makenode($1, NULL, NULL, "real");}| 
	   HEX {$$ = makenode($1, NULL, NULL, "int");}|
       EXPONENT {$$ = makenode($1, NULL, NULL, "real");}| 
       CHAR_SIGN {$$ = makenode($1, NULL, NULL, "char");}| 
       TRUE {$$ = makenode($1, NULL, NULL, "bool");}| 
       FALSE {$$ = makenode($1, NULL, NULL, "bool");}|
	   STRING_PHRASE {$$ = makenode($1, NULL, NULL, "string");}|
	   NONE {$$ = makenode($1, NULL, NULL, "none");}

code_block: SBLOCK block return EBLOCK {$$ = makenode("{{", makenode("", $2, $3, ""), makenode("}}", NULL, NULL, ""), "BLOCK");}
block: block_tag {$$ = makenode("B", $1, NULL, "");} |
		 var_dec_rec block_tag {$$ = makenode("", makenode("DECLARATIONS", $1, NULL, ""), $2, "");} |
		 code_block block_tag {$$ = makenode( "", makenode( "{{", $1, makenode("}}", NULL, NULL, ""), "BLOCK"), $2, "");} |
		 var_dec_rec {$$ = makenode("", makenode("DECLARATIONS", $1, NULL, ""), NULL, "");} |
						 {$$ = makenode("", NULL, NULL, "");}

block_tag: stm_block_tag2 block_tag {$$ = makenode("BT", $1, $2, "");} | 
							stm_block_tag2 {$$ = makenode("LBT", $1, NULL, "");} | 
							code_block {$$ = makenode("", $1, NULL, "");} | 
							SBLOCK EBLOCK {$$ = makenode("{{", makenode("}}", NULL, NULL, ""), NULL, "BLOCK");} 

ass: ID ASS ass_type ENDLINE {$$ = makenode($2, makenode($1,NULL,NULL,"var"), $3, "ASSIGNMENT");}| 
	  ID '[' exp ']' ASS exp ENDLINE {$$ = makenode($5, makenode($1,$3,NULL,"var"), $6, "CHAR_ASSIGNMENT");} |
	  MUL ID ASS ass_type ENDLINE {$$ = makenode($3, makenode("*",makenode($2,NULL,NULL,"pointer"),NULL,""), $4, "ASSIGNMENT");}
ass_type: exp {$$ = $1;} 

var_dec: VAR var_dectag {$$=makenode( "", $2, NULL, ""); }|
		 STRING str_dec {$$=makenode( "", $2, NULL, ""); }|
         func {$$ = $1;} 
var_dectag: var_type ID ASS exp var_dectag2{$$=makenode(GetNodeType($1), makenode($2, NULL, makenode($3, $4, NULL, ""), "var"), $5,"DEC_ASS");} |
				var_type ID var_dectag2 {$$=makenode( GetNodeType($1), makenode( $2, NULL, NULL, "var"), $3, ""); }
str_dec: ID '[' exp ']' ASS string_ass str_dectag {$$=makenode("string",makenode($1,$3,makenode($5,makenode("",$6,NULL,""),NULL,""),"var"),$7,"DEC_STR");} |
			ID '[' exp ']' str_dectag{$$=makenode("string",makenode($1,$3,NULL,"var"),$5, "");}
var_dectag2: ',' ID ASS exp var_dectag2{$$=makenode( "", makenode( $2, NULL, makenode($3, $4, NULL, ""), "var"), $5, ""); } |
				 ',' ID var_dectag2{$$=makenode( "", makenode($2 , NULL, NULL, "var"), $3, ""); } |
			    ENDLINE {$$=makenode( "", NULL, NULL, ""); }
str_dectag: ','ID '[' exp ']' ASS string_ass str_dectag{$$=makenode("",makenode($2,$4,makenode($6,makenode("", $7, NULL, ""),NULL,""),"var"),$8, "");}| 
				 ',' ID '[' exp ']' str_dectag {$$=makenode( "", makenode( $2, $4, NULL, "var"), $6, ""); }|
				 ENDLINE {$$=makenode( "", NULL, NULL, ""); }
var_dec_rec: var_dec var_dec_rec {$$=makenode( "", $1, $2, "");} |
             var_dec {$$=makenode( "", $1, NULL, "");} 
string_ass: STRING_PHRASE {$$ = makenode($1,NULL,NULL,"string");} | 
			ID {$$ = makenode($1,NULL,NULL,"id");}

exp: expr {$$ = makenode("", $1, NULL, "EXP");}
expr: '(' expr ')' {$$ = makenode( "(", $2, makenode(")", NULL, NULL, ""), "2EXP");}|     
	   expr ADD expr {$$ = makenode($2, $1, $3, "2EXP");}|
      expr SUB expr {$$ = makenode($2, $1, $3, "2EXP");}|
      expr MUL expr {$$ = makenode($2, $1, $3, "2EXP");}|
      expr DIV expr {$$ = makenode($2, $1, $3, "2EXP");}|
	   expr BIGGER expr {$$ = makenode($2, $1, $3, "2EXP");}|
	   expr BIGGER_EQ expr {$$ = makenode($2, $1, $3, "2EXP");}|
	   expr SMALLER expr {$$ = makenode($2, $1, $3, "2EXP");}|
	   expr SMALLER_EQ expr {$$ = makenode($2, $1, $3, "2EXP");}|
	   expr EQ expr {$$ = makenode($2, $1, $3, "2EXP");}|
		expr NOT_EQ expr {$$ = makenode($2, $1, $3, "2EXP");}|
	   expr OR expr {$$ = makenode($2, $1, $3, "2EXP");}|
	   expr AND expr {$$ = makenode($2, $1, $3, "2EXP");}|
	   NOT expr {$$ = makenode($1, NULL, $2, "1EXP");}|
	   ADDRESS expr {$$ = makenode($1, NULL, $2, "ADDRESS");}|
		MUL expr {$$ = makenode($1, NULL, $2, "1EXP");}|
	   ADD expr {$$ = makenode($1, NULL, $2, "1EXP");}|
	   SUB expr {$$ = makenode($1, NULL, $2, "1EXP");}|
		'|' ID '|' {$$ = makenode("|", makenode($2,makenode("|",NULL,NULL,""),NULL, ""), NULL, "ABS");}|
      value {$$ = $1;}|
	   func_call2 {$$ = $1;}

cond: IF '(' exp ')' code_block cond2 {$$ = makenode( "IF", makenode( "", $3, $5, ""), $6,"");} |
		IF '(' exp ')' stm cond3 {$$ = makenode( "IF", makenode( "", $3, $5, ""), $6,"");}
cond2: ELSE code_block {$$ = makenode("ELSE", NULL, $2, "");}| 
		 {$$ = makenode("NO_ELSE", NULL, NULL, "");}
cond3: ELSE stm {$$ = makenode("ELSE", NULL, $2, "");}|
		 {$$ = makenode("NO_ELSE", NULL, NULL, "");}

func: var_type ID '(' func_parms_rec ')' SBLOCK func_body return2 EBLOCK
		  {$$=makenode("FUNC", makenode($2, makenode("PARAMETERS", $4, NULL, ""), 
		  makenode("RETURN_TYPE", makenode(GetNodeType($1),NULL,NULL,""), NULL,""),""),
	     makenode($6, makenode("", $7, $8, ""), makenode($9, NULL, NULL, ""),""),"");} | 
      VOID ID '(' func_parms_rec ')' SBLOCK func_body EBLOCK
		  {$$=makenode("FUNC",makenode($2, makenode("PARAMETERS", $4, NULL, ""),
		  makenode("RETURN_TYPE",makenode($1,NULL,NULL,""), NULL,""),""),
		  makenode($6, makenode("", $7, NULL, ""), makenode($8,NULL,NULL,"END_FUNC"),""),"");}
func_body: func block {$$ = makenode("", $1, $2, "");}| 
			  block {$$ = $1;}
func_parms: var_type ID func_parmstag {$$ = makenode(GetNodeType($1), makenode( $2, NULL, NULL, "parm"), $3, ""); }

func_parmstag: ',' ID func_parmstag {$$ = makenode( "", makenode($2 , NULL, NULL, "parm"), $3, ""); } |
			      { $$ = makenode( "", NULL, NULL, ""); }
func_parms_rec: func_parms_rec ENDLINE func_parms { $$ = makenode( "", $1, $3, ""); } |
                func_parms {$$ = makenode( "", $1, NULL, ""); } |
                {$$ = makenode( "", NULL, NULL, ""); }
return: RETURN exp ENDLINE {$$ = makenode("RETURN_STM", $2, NULL, "");}|
		  {$$ = makenode( "", NULL, NULL, "");}
return2: RETURN exp ENDLINE  {$$ = makenode("RETURN_STM", $2, NULL, "END_FUNC");}

func_call: ID ASS func_call2 ENDLINE{$$ = makenode($2, makenode($1,NULL,NULL,""), $3, "FUNC_CALL_ASS");}| 
			  func_call2 ENDLINE{$$ = $1;}
func_call2: ID '(' func_call_parms ')'  {$$ = makenode( $1, NULL, $3, "FUNC_CALL");}
func_call_parms: exp func_call_tag {$$ = makenode("", $1, $2, "FUNC_CALL_PARMS");}| 
					  {$$ = makenode("", NULL, NULL, "");}
func_call_tag: ',' exp func_call_tag {$$ = makenode(", ", $2, $3, "FUNC_CALL_PARMS");}| 
					{$$ = makenode("", NULL, NULL, "");}


for_loop: FOR '(' ID ASS INTEGERS ENDLINE exp ENDLINE ID ASS exp ')' for_looptag
			 {$$=makenode("FOR",
			 				makenode("for_init", 
							 makenode($4, makenode($3, NULL, NULL,""), makenode($5,NULL,NULL,""), ""), 
							 makenode("",makenode("for_exp",NULL,$7, ""), makenode("for_update", NULL, makenode($10, makenode($9,NULL,NULL,""),$11,""),""),""),""),$13,"");}
for_looptag: code_block {$$ = $1;}| 
				 stm {$$ = $1;}

while_loop: WHILE '(' exp ')' while_tag  {$$=makenode("WHILE", $3, $5, "");}
while_tag: code_block {$$ = $1;}| 
		   stm {$$ = $1;}

do_loop: DO code_block WHILE '(' exp ')' ENDLINE
			{$$=makenode("DO_WHILE", $2, $5, "");}
%%

int main()
{
	return yyparse();
}
node* makenode(char* token, node* left, node* right, char* type)
{
	node* newnode = (node*)malloc(sizeof(node));
	char* newtoken = (char*)malloc(sizeof(char) * 30);
	char* newtype = (char*)malloc(sizeof(char) * 30);
	char* newvar = (char*)malloc(sizeof(char) * 30);
	char* newcode = (char*)malloc(sizeof(char) * 1000);
	char* newbegin = (char*)malloc(sizeof(char) * 30);
	char* newafter = (char*)malloc(sizeof(char) * 30); 
	char* newnext_block = (char*)malloc(sizeof(char) * 30); 
	strcpy(newtoken, token);
	strcpy(newtype, type);
	strcpy(newbegin, "        ");
	strcpy(newafter, "");
	newnode->visit = 0;
	newnode->token = newtoken;
	newnode->left = left;
	newnode->right = right;
	newnode->type = newtype;
	newnode->code = newcode;
	newnode->var = newvar;
	newnode->begin = newbegin;
	newnode->after = newafter;
	newnode->next_block = newnext_block;
	newnode->location = 0;
	return newnode;
}

char* GetNodeType(node* n)
{
	return n->token;
}

LinkedList* BuildVarNode(char* i, char* g, char* t, LinkedList *sc)
{
	LinkedList* node = (LinkedList*)malloc(sizeof(LinkedList));
	node->id = (char*)malloc(sizeof(char) * 30);
	strcpy(node->id, i);
	node->group = (char*)malloc(sizeof(char) * 30);
	strcpy(node->group, g);
	node->type = (char*)malloc(sizeof(char) * 30);
	strcpy(node->type, t);
	node->next = NULL;
	node->prev = NULL;
	node->scope = sc;
	return node;
}

LinkedList* BuildFuncNode(char* i, char* g, char* t, int p, LinkedList *sc)
{
	LinkedList* node = (LinkedList*)malloc(sizeof(LinkedList));
	node->id = (char*)malloc(sizeof(char) * 30);
	strcpy(node->id, i);
	node->group = (char*)malloc(sizeof(char) * 30);
	strcpy(node->group, g);
	node->type = (char*)malloc(sizeof(char) * 30);
	strcpy(node->type, t);
	node->parms_num = p;
	node->next = NULL;
	node->prev = NULL;
	node->scope = sc;
	return node;
}

void AddNode(LinkedList* tale, LinkedList* node)
{
	tale->next = node;
	node->prev = tale;
}

void DeleteList(LinkedList* lst)
{
	LinkedList* temp = lst;
	
	if (temp->next != NULL)
		DeleteList(temp->next);
	
	free(temp->id);
	free(temp->group);
	free(temp->type);
}

int IsInList(LinkedList* lst, char* i)
{
	LinkedList* temp = lst;

	while(temp->next != NULL)
	{
		if (strcmp(temp->id, i) == 0)
		{
			return 1;
		}
		temp = temp->next;
	}
	return 0;
}

Stack* NewStack()
{
	Stack* st = (Stack*)malloc(sizeof(Stack));
	st->maxsize = 30;
	st->top = -1;
	st->scopes = (LinkedList**)malloc(sizeof(LinkedList*) * st->maxsize);
	
	return st;
}

int IsEmpty(Stack* st)
{
	return st->top == - 1;
}

int IsFull(Stack* st)
{
	return st->top == st->maxsize - 1;
}

void Push(Stack* st, LinkedList* lst)
{
	if(IsFull(st))
	{
		printf("Too much scopes - Stack is full\n");
		exit(EXIT_FAILURE);
	}
	st->top = st->top + 1;
	st->scopes[st->top] = lst;
}

LinkedList* Peek(Stack* st)
{
	if(IsEmpty(st))
	{
		printf("No scopes - Stack is Empty\n");
		exit(EXIT_FAILURE);
	}

	return st->scopes[st->top];
}

LinkedList* GetByIndex(Stack* st, int index)
{
	if(IsEmpty(st))
	{
		printf("No scopes - Stack is Empty\n");
		exit(EXIT_FAILURE);
	}

	if(index >= 0)
		return st->scopes[index];
}

LinkedList* Pop(Stack* st)
{
	if(IsEmpty(st))
	{
		printf("No scopes - Stack is Empty\n");
		exit(EXIT_FAILURE);
	}
    st->top = st->top - 1;
	return st->scopes[st->top + 1];
}

int IsInStack(Stack* st, char* word)
{
	int index = st->top;
	LinkedList* temp;
	while(index != -1)
	{
		LinkedList* temp = GetByIndex(st, index);
		if(IsInList(temp, word))
			return 1;
		index -= 1;
	}
	return 0;
}

void BuildSymbolTable(node* root)
{
	st = NewStack();
	global = BuildFuncNode( "global", "func", "global", 0, NULL);
	Push(st, global);
	RunOnTree(root, global);
	CheckMain(scope);

	printf("CODE COMPILED SUCCESSFULLY.\n\n");

	/* printf("(CODE\n");
	UpdateVisit(root);
	PrintTree(root, 0);
	printf(")\n"); */

	UpdateVisit(root);
	SetLable(root, root, 0); 
	SetVar(root);
	UpdateVisit(root);
	SetCode(root); 
	printf("%s\n", root->code);
}

void RunOnTree(node* node, LinkedList* scope)
{
	if(strcmp(node->token, "NEW_SCOPE") == 0)
	{
		scope = BuildFuncNode( "global", "func", "global", 0, NULL);
	}
	else if (strcmp(node->token, "FUNC") == 0 && node->visit == 0)
	{   
		LinkedList* func_node;
		
		if(strcmp(scope->id, "global") == 0)
		{
			scope = BuildFuncNode(node->left->token, "func", node->left->right->left->token, 0, global);
			if(strcmp(node->left->token, "main") != 0)
			{
				CheckFunc(scope, node, "");
				CheckGlobal(scope, node->left->token);
			}
		}
		else
		{	
			func_node = BuildFuncNode(node->left->token, "func", node->left->right->left->token, 0, scope);	
			char* word = (char*)malloc(sizeof(char) * 30);
			strcpy(word, node->left->token);
			CheckFunc(func_node, node, word);
			while(scope->next != NULL)
				scope = scope->next;
			AddNode(scope, func_node);
			scope = func_node;
		}
	}
	else if (strcmp(node->token, "PARAMETERS") == 0 && node->visit == 0)
    {
		while(scope->next != NULL)
			scope = scope->next;
		while(scope->prev != NULL && strcmp(scope->group, "func)") != 0)
			scope = scope->prev;

        GetParameters(node, "", scope);
    }
	else if((strcmp(node->token, "{") == 0 || strcmp(node->token, "{{") == 0) && node->visit == 0) 
	{
		if(strcmp(node->token, "{{") == 0)
		{
			LinkedList* func_node;
			if(strcmp(scope->id, "global") == 0)
				scope = BuildFuncNode("block", "block", "block", 0, global);
			else
			{
				func_node = BuildFuncNode("block", "block", "block", 0, scope);
				while(scope->next != NULL)
					scope = scope->next;
				AddNode(scope, func_node);
				scope = func_node;
			}
		}

		while(scope -> next != NULL)		/* save the last scope */
			scope = scope->next;
		while(scope->prev != NULL && strcmp(scope->group, "func") != 0 && strcmp(scope->group, "block") != 0)
			scope = scope->prev;
		Push(st, scope);					/* push the last scope */
	}	
	else if((strcmp(node->token, "}") == 0 || strcmp(node->token, "}}") == 0) && node->visit == 0)
	{
		int flag = 0;
		LinkedList* temp = scope->scope;
		if(strcmp(scope->scope->id, "global") != 0)
		{
			flag = 1;
			Pop(st);
		}
		while(scope->next != NULL)
			scope = scope->next;
		while(scope->prev != NULL && strcmp(scope->group, "func") != 0 && strcmp(scope->group, "block") != 0)
			scope = scope->prev;	
		strcpy(scope->group, "closed");

		if(flag == 1)
			scope = temp;		
		else
			scope = global;
	}
    else if (strcmp(node->token, "DECLARATIONS") == 0 && node->visit == 0)
    {
        GetDeclarations(scope, node, "");
    }
	else if (strcmp(node->token, "IF") == 0 && node->visit == 0)
	{
		CheckBool(node->left->left->left);
	}
	else if(strcmp(node->token, "WHILE") == 0 && node->visit == 0)
	{
		CheckBool(node->left->left);
	}
	else if (strcmp(node->type, "ABS") == 0 && node->visit == 0)
	{
		CheckAbs(node);
	}
	else if (strcmp(node->type, "FUNC_CALL") == 0 && node->visit == 0)
	{
		CheckFuncCall(node, scope);
	}
	else if (strcmp(node->type, "FUNC_CALL_ASS") == 0 && node->visit == 0)
	{
		CheckFuncCallAss(node, scope);
	}
	else if (strcmp(node->token, "RETURN_STM") == 0 && node->visit == 0)
	{
		char* return_value = (char*)malloc(sizeof(char) * 30);
		if(strcmp(node->left->left->type, "2EXP") == 0)
			strcpy(return_value, UpdateAssOperator(node->left->left));
		else
			strcpy(return_value, node->left->left->type);
		
		LinkedList* func = scope;
		while(func->next != NULL)
		{
			func = func->next;
		}
		while(func != NULL && strcmp(func->group, "func") != 0 && strcmp(func->group, "block") != 0)
			func = func->prev;

		if (strcmp(return_value, "id") == 0)
		{
			int flag = 0;
			LinkedList* search_id = func;
			LinkedList* track = func;
			while(track != NULL)
			{
				while(search_id != NULL && flag == 0)
				{
					if(strcmp(search_id->id, node->left->left->token) == 0 && (strcmp(search_id->group, "var") == 0 || strcmp(search_id->group, "parm") == 0))
					{
						strcpy(return_value,  search_id->type);
						flag = 1;
					}
					search_id = search_id->next;
				}
				if(track == NULL || flag == 1)
					break;
				search_id = track;
				track = track->scope;
			}
			if(flag == 0)
			{
				printf("line: %d - the returned variable (%s) doesn't exist in this scope.\n", yylineno, node->left->left->token);
				exit(0);
			}
		}
		if(strcmp(return_value, "string") == 0)
		{
			printf("line: %d - the returned value cann't be string\n", yylineno);
			exit(0);
		}
		if(strcmp(func->type, return_value) != 0)
		{
			printf("line: %d - the returned value (%s) doesn't match the function return type (%s)\n", yylineno, return_value, func->type);
			exit(0);
		}
		free(return_value);
	}
	else if (strcmp(node->type, "ASSIGNMENT") == 0 || strcmp(node->type, "CHAR_ASSIGNMENT") == 0 && node->visit == 0)	
	{
		CheckAssignments(node);
	}

	node->visit = 1;

    if(node->left != NULL && node->left->visit != 1)
        RunOnTree(node->left, scope);
    if(node->right != NULL && node->right->visit != 1)
        RunOnTree(node->right, scope);

}

void PrintList(LinkedList* lst)
{
	while(lst != NULL)
	{
		PrintNode(lst);
		if(lst->next != NULL)
		{
			lst = lst->next;
			printf("-> ");
		}
		else
			lst = NULL;
	}
}

void PrintNode(LinkedList* node)
{
    if(strcmp(node->group, "func") == 0)
        printf("%s %s %s (%d) ", node->group, node->type, node->id, node->parms_num);
    else
	    printf("%s %s %s ", node->group, node->type, node->id);
}

void PrintStack(Stack* st)
{
	int index = st->top;
	while(index != -1)
    {
		PrintList(GetByIndex(st, index));
		index = index - 1;
		printf("\n");
    }
}

void GetParameters(node* node, char* v_type, LinkedList* scope)
{	

	char* var_type = (char*)malloc(sizeof(char) * 30);
	strcpy(var_type, v_type);
	if (strcmp(node->token, "int") == 0 || strcmp(node->token, "int*") == 0 ||  
		 strcmp(node->token, "real") == 0 || strcmp(node->token, "real*") == 0 || 
		 strcmp(node->token, "char") == 0 || strcmp(node->token, "char*") == 0)
	{
		strcpy(var_type, node->token);
	} 

    else if (strcmp(node->type, "parm") == 0)
	{
		while(scope->next != NULL)
			scope = scope->next;
		while(scope->prev != NULL && strcmp(scope->group, "func") != 0)
			scope = scope->prev;
			
		LinkedList* temp = scope;
		LinkedList* var_lst = BuildVarNode(node->token, "parm", var_type, scope);
        scope->parms_num += 1;
		while(temp->next != NULL)
		{	
			if(strcmp(temp->id, node->token) == 0 && temp->scope == scope)
			{
				printf("line: %d - there is one or more variables with the same name at the same scope. (%s)\n", yylineno, node->token);
				exit(0);
			}

			temp = temp->next;
		}

		AddNode(temp, var_lst);
	}

	if(node->left != NULL && node->left->visit != 1)
		GetParameters(node->left, var_type, scope);
	if(node->right != NULL && node->right->visit != 1)
		GetParameters(node->right, var_type, scope);	

	node->visit = 1;
	free(var_type);
}

void GetDeclarations(LinkedList* scope, node* node, char* v_type)
{
	if(strcmp(node->token, "FUNC") == 0)
		RunOnTree(node, scope);
	LinkedList* temp = scope;

    char* var_type = (char*)malloc(sizeof(char) * 30);
	strcpy(var_type, v_type);
	if (strcmp(node->token, "int") == 0 || strcmp(node->token, "int*") == 0 ||  
		 strcmp(node->token, "real") == 0 || strcmp(node->token, "real*") == 0 || 
		 strcmp(node->token, "char") == 0 || strcmp(node->token, "char*") == 0 || 
		 strcmp(node->token, "string") == 0 || strcmp(node->token, "bool") == 0)
	{
		strcpy(var_type, node->token);
	} 

	if(strcmp(var_type, "string") == 0)
		CheckStringDec(node);
	
	if (strcmp(node->type, "var") == 0)
	{	
		while(temp->next != NULL)
		{	
			if(strcmp(temp->id, node->token) == 0)
			{
				printf("line: %d - the variable (%s) already exist in the same scope.\n", yylineno, node->token);
				exit(0);
			}
			temp = temp->next;
		}
		if(node->right && strcmp(node->right->token, "=") == 0 && strcmp(UpdateAssOperator(node->right->left->left), var_type) != 0)
		{
			printf("line: %d - %s type can't assigned to %s type.\n", yylineno, UpdateAssOperator(node->right->left->left), var_type);
			exit(0);
		}
		LinkedList* var_lst = BuildVarNode(node->token, "var", var_type, scope);

		AddNode(temp, var_lst);
	}

	node->visit = 1;
    if(node->left != NULL && node->left->visit != 1)
		GetDeclarations(scope, node->left, var_type);
	if(node->right != NULL && node->right->visit != 1)
		GetDeclarations(scope, node->right, var_type);	

	free(var_type);
}

void CheckAbs(node* node)
{
	char* save_id = (char*)malloc(sizeof(char) * 30);
	strcpy(save_id, node->left->token);
	LinkedList* temp = Peek(st);

	while(temp->next != NULL)
		temp = temp->next;
	while(strcmp(temp->group, "func") != 0 && strcmp(temp->group, "block") != 0)
		temp = temp->prev;	

	LinkedList* track = temp;
	int flag_track = 0;

	while(track != NULL && flag_track == 0)
	{
		while(temp != NULL)
		{
			if(strcmp(temp->id, save_id) == 0 && (strcmp(temp->group, "var") == 0 || strcmp(temp->group, "parm") == 0) && strcmp(temp->type, "string") == 0)
			{
				free(save_id);
				return;
			}
			else if (strcmp(temp->id, save_id) == 0 && (strcmp(temp->group, "func") == 0 || strcmp(temp->group, "block") == 0))
			{
				free(save_id);
				printf("line: %d - %s is function and not variable.\n", yylineno, temp->id);
				exit(0);
			}
			else if (strcmp(temp->id, save_id) == 0 && strcmp(temp->type, "string") != 0 )
			{
				free(save_id);
				printf("line: %d - %s must be string, got %s.\n", yylineno, temp->id, temp->type);
				exit(0);
			}
			temp = temp->next;
		}
		if(track == NULL)
			break;
		else if(flag_track == 0)
		{
			temp = track;
			track = track->scope;
		}
	}
	printf("line: %d - %s undeclared.\n", yylineno, save_id);
	free(save_id);
	exit(0);
}

void CheckStringDec(node* node)
{
	if (node->left && strcmp(node->left->type, "var") == 0)
	{ 
		char* index = (char*)malloc(sizeof(char) * 30); 
		strcpy(index, node->left->left->left->type);
		LinkedList* track = Peek(st);
		if(strcmp(index, "id") == 0)
		{
			int flag = 0;
			LinkedList* search_id = Peek(st);
			while(track != NULL)
			{
				while(search_id != NULL && flag == 0)
				{

					if(node->left->left->left && strcmp(search_id->id, node->left->left->left->token) == 0 && strcmp(search_id->group, "var") == 0)
					{
						strcpy(index, search_id->type);
						flag = 1;
					}
					search_id = search_id->next;
				}
				if(track == NULL)
					break;
				search_id = track;
				track = track->scope;
			}
			if(flag == 0)
			{
				free(index);
				printf("line: %d - the variable (%s) inside the [ ] doesn't exist in this scope.\n", yylineno, node->left->left->left->token);
				exit(0);
			}
		} 
		if (strcmp(index, "int") != 0 )
		{	if(strcmp(node->left->left->left->type, "ABS") == 0)	
				CheckAbs(node->left->left->left);
			else
			{
				printf("line: %d - the value of the index inside the [ ] must be int, got %s.\n", yylineno, index);
				free(index);
				exit(0);
			}
			
		}
	} 
}

char* UpdateOperatorType(node* node)
{	
	char* left, *right;
	if (strcmp(node->type, "id") == 0)
	{
		int flag = 0;
		LinkedList* temp = Peek(st);
		LinkedList* scope = temp;
		char* var_type = (char*)malloc(sizeof(char) * 30);
		while(scope != NULL)
		{
			while(temp != NULL && flag == 0)
			{
				if(strcmp(temp->id, node->token) == 0)
				{
					strcpy(var_type, temp->type);
					flag = 1;
				}
				temp = temp->next;	
			}
			if(flag == 1 || scope == NULL)
				break;
			temp = scope->scope;
			scope = scope->scope;
		}
		if(flag == 0)
		{
			printf("line: %d - the variable (%s) doesn't exist in this scope.\n", yylineno, node->token);
			free(var_type);
			exit(0);
		}
		return var_type;
	}
	else if(strcmp(node->type, "ABS") == 0)
		return "int";
	else if (strcmp(node->type, "int") == 0 || strcmp(node->type, "real") == 0 || strcmp(node->type, "int*") == 0 || strcmp(node->type, "real*") == 0
			|| strcmp(node->type, "char") == 0 || strcmp(node->type, "char*") == 0 || strcmp(node->type, "bool") == 0 || strcmp(node->type, "string") == 0)
	{
		return node->type;
	}
	else if(strcmp(node->type, "2EXP") == 0 && strcmp(node->token, "(") == 0)
	{
		return UpdateOperatorType(node->left->left);
	}
	else if(strcmp(node->type, "2EXP") == 0)
	{
				left = (char*)malloc(sizeof(char) * 30);
		if(node->left != NULL)
			strcpy(left, UpdateOperatorType(node->left));
		right = (char*)malloc(sizeof(char) * 30);
		if(node->right != NULL)
			strcpy(right, UpdateOperatorType(node->right));
	}
	else if(strcmp(node->token, "+") == 0 || strcmp(node->token, "-") == 0 || strcmp(node->token, "*") == 0 || strcmp(node->token, "/") == 0)
	{
		left = (char*)malloc(sizeof(char) * 30);
		if(node->left != NULL)
			strcpy(left, UpdateOperatorType(node->left));
		right = (char*)malloc(sizeof(char) * 30);
		if(node->right != NULL)
			strcpy(right, UpdateOperatorType(node->right));
	}
	if(left && right)
	{
		if(strcmp(left, "int") == 0 && strcmp(right, "int") == 0)
			return "int";
		else if(strcmp(left, "int") == 0 && strcmp(right, "real") == 0)
			return "real";
		else if(strcmp(left, "real") == 0 && strcmp(right, "int") == 0)
			return "real";
		else if(strcmp(left, "real") == 0 && strcmp(right, "real") == 0)
			return "real";
		else
		{
			printf("line: %d - %s can't %s by %s.\n", yylineno, left, node-> token, right); 
			exit(0);
		}
	}
}

char* UpdateAssOperator(node* node)
{	
	char* ass_type = (char*)malloc(sizeof(char) * 30);
	if (strcmp(node->token, "+") == 0 || strcmp(node->token, "-") == 0 || strcmp(node->token, "*") == 0 || strcmp(node->token, "/") == 0)
		return UpdateOperatorType(node);
	if (strcmp(node->token, "<") == 0 || strcmp(node->token, "<=") == 0 || strcmp(node->token, ">") == 0 || strcmp(node->token, ">=") == 0)
		return "bool";
	else if (strcmp(node->type, "id") == 0)
	{
		int flag = 0;
		LinkedList* temp = Peek(st);
		char* var_type = (char*)malloc(sizeof(char) * 30);
		while(temp != NULL && flag == 0)
		{
			if(strcmp(temp->id, node->token) == 0)
			{
				strcpy(ass_type, temp->type);
				flag = 1;
			}
			temp = temp->next;	
		}
		if(flag == 0)
		{
			printf("line: %d - the variable (%s) doesn't exist in this scope.\n", yylineno, node->token);
			free(ass_type);
			exit(0);
		}
		return ass_type;
	}
	else if(strcmp(node->type, "2EXP") == 0)
	{
		UpdateOperatorType(node);
	}
	else if (strcmp(node->type, "char") == 0 || strcmp(node->type, "string") == 0 || strcmp(node->type, "int") == 0 || strcmp(node->type, "real") == 0 || strcmp(node->type, "bool") == 0)
	{
		return node->type;
	}
}

char* CheckBool(node* node)
{
	int flag = 0;
	char* left_tag = NULL, *right_tag = NULL;
	char* not_operator = NULL;
	char* left = NULL, *right = NULL;
	char* eq_left = NULL, *eq_right = NULL;
	if (strcmp(node->type, "id") == 0)
	{
		int flag = 0;
		LinkedList* temp = Peek(st);
		LinkedList* track = temp->scope;
		char* var_type = (char*)malloc(sizeof(char) * 30);
		while(track != NULL)
		{
			while(temp != NULL && flag == 0)
			{
				if(strcmp(temp->id, node->token) == 0)
				{
					strcpy(var_type, temp->type);
					flag = 1;
				}

				temp = temp->next;	
			}
			if(track == NULL)
				break;
			temp = track;
			track = track->scope;
		}
		
		if(flag == 0)
		{
			printf("line: %d - the variable (%s) doesn't exist in this scope.\n", yylineno, node->token);
			free(var_type);
			exit(0);
		}
		return var_type;
	}
	else if(strcmp(node->type, "2EXP") == 0)
	{
		return CheckBool(node->left);
	}
	else if (strcmp(node->type, "ADDRESS") == 0)
	{
		return CheckBool(node->right);
	}
	else if(strcmp(node->type, "1EXP") == 0 && strcmp(node->token, "*") == 0)
	{
		return CheckBool(node->right);
	}
	else if (strcmp(node->type, "int") == 0 || strcmp(node->type, "real") == 0 || strcmp(node->type, "int*") == 0 || strcmp(node->type, "real*") == 0
			|| strcmp(node->type, "char") == 0 || strcmp(node->type, "char*") == 0 || strcmp(node->type, "bool") == 0 || strcmp(node->type, "string") == 0)
	{
		return node->type;
	}
	else if(strcmp(node->type, "ABS") == 0)
	{
		return "int";
	}
	else if (strcmp(node->token, "+") == 0 || strcmp(node->token, "-") == 0 || strcmp(node->token, "*") == 0 || strcmp(node->token, "/") == 0)
	{
		return UpdateOperatorType(node);
	}
	if(strcmp(node->token, "<") == 0 || strcmp(node->token, "<=") == 0 || strcmp(node->token, ">") == 0 || strcmp(node->token, ">=") == 0)
	{
		
		left = (char*)malloc(sizeof(char) * 30);
		if(node->left != NULL)
			strcpy(left, CheckBool(node->left));
		right = (char*)malloc(sizeof(char) * 30);
		if(node->right != NULL)
			strcpy(right, CheckBool(node->right));
	}
	else if(strcmp(node->token, "||") == 0 || strcmp(node->token, "&&") == 0)
	{
		left_tag = (char*)malloc(sizeof(char) * 30);
		if(node->left != NULL)
			strcpy(left_tag, CheckBool(node->left));
		right_tag = (char*)malloc(sizeof(char) * 30);
		if(node->right != NULL)
			strcpy(right_tag, CheckBool(node->right));
	}
	else if(strcmp(node->token, "==") == 0 || strcmp(node->token, "!=") == 0)
	{
		eq_left = (char*)malloc(sizeof(char) * 30);
		if(node->left != NULL)
			strcpy(eq_left, CheckBool(node->left));
		eq_right = (char*)malloc(sizeof(char) * 30);
		if(node->right != NULL)
			strcpy(eq_right, CheckBool(node->right));
	}
	else if(strcmp(node->token, "!") == 0)
	{
		not_operator = (char*)malloc(sizeof(char) * 30);
		if(node->right != NULL)
			strcpy(not_operator, CheckBool(node->right));
	}
	if(left != NULL && right != NULL)
	{
		if(strcmp(left, "int") == 0 && strcmp(right, "int") == 0)
			return "bool";
		else if(strcmp(left, "int") == 0 && strcmp(right, "real") == 0)
			return "bool";
		else if(strcmp(left, "real") == 0 && strcmp(right, "int") == 0)
			return "bool";
		else if(strcmp(left, "real") == 0 && strcmp(right, "real") == 0)
			return "bool";
		else
			flag = 1;
	}
	if(eq_left != NULL && eq_right != NULL)
	{
		if(strcmp(eq_left, eq_right) == 0)
			return "bool";
		else if( (strcmp(eq_left, "int") == 0 && strcmp(eq_right, "int*") == 0) || 
				 (strcmp(eq_left, "char") == 0 && strcmp(eq_right, "char*") == 0)|| 
				 (strcmp(eq_left, "real") == 0 && strcmp(eq_right, "real*") == 0) )
				 return "bool";
		else if( (strcmp(eq_left, "int*") == 0 && strcmp(eq_right, "int") == 0) || 
				 (strcmp(eq_left, "char*") == 0 && strcmp(eq_right, "char") == 0)|| 
				 (strcmp(eq_left, "real*") == 0 && strcmp(eq_right, "real") == 0) )
				 return "bool";
		else
		{
			printf("line: %d - \"==\" and \"!=\" operator's operands must be same type.\n", yylineno);
			exit(0);
		}
	}

	if(not_operator != NULL)
	{
		if(strcmp(not_operator, "bool") == 0)
			return "bool";
		else
		{
			printf("line: %d - \"!\" operator operates only on booleans.\n", yylineno);
			free(not_operator);
			exit(0);
		}
	}
	if(left_tag != NULL && right_tag != NULL)
	{
		if(strcmp(left_tag, "bool") == 0 && strcmp(right_tag, "bool") == 0)
			return "bool";
		else if (flag == 0)
		{
			printf("line: %d - %s can't %s by %s.\n", yylineno, left_tag, node-> token, right_tag); 
			free(right_tag);
			free(left_tag);
			exit(0);
		}
	}
	if(flag == 1)
	{
		printf("line: %d - %s can't %s by %s.\n", yylineno, left, node-> token, right); 
		free(left);
		free(right);
		exit(0);
	}
}

void CheckAssignments(node* node)
{	
	char* exp_value = (char*)malloc(sizeof(char) * 30);
	
	if(strcmp(node->type, "CHAR_ASSIGNMENT") == 0)
	{
		strcpy(exp_value, node->left->left->left->type);
		node->left->left->left->visit = 1;
		if(strcmp(exp_value, "id") == 0)
		{
			int flag = 0;
			LinkedList* search_id = Peek(st);

			while(search_id != NULL && flag == 0)
			{
				if(strcmp(search_id->id, node->left->left->left->token) == 0 && strcmp(search_id->group, "var") == 0)
				{
					strcpy(exp_value, search_id->type);
					flag = 1;
				}
				search_id = search_id->next;
			}
			if(flag == 0)
			{
				free(exp_value);
				printf("line: %d - the variable (%s) inside the [ ] doesn't exist in this scope.\n", yylineno, node->left->left->left->token);
				exit(0);
			}
		}
		if(strcmp(exp_value, "int") != 0)
		{
			printf("line: %d - the value of the index inside the [ ] must be int, got %s.\n", yylineno, exp_value);
			free(exp_value);
			exit(0);
		}
		int flag = 0, flag1 = 0;
		LinkedList* search_id = Peek(st);
		char* left_var_type = (char*)malloc(sizeof(char) * 30);
		char* right_var_type = (char*)malloc(sizeof(char) * 30);
		if(strcmp(node->right->left->type, "id") != 0)
		{
			flag1 = 2;
			strcpy(right_var_type, node->right->left->type);
		}
		while(search_id != NULL || flag == 0 || flag1 == 0)
		{
			if(strcmp(search_id->id, node->left->token) == 0 && strcmp(search_id->group, "var") == 0 && flag == 0)
			{	
				strcpy(left_var_type, search_id->type);
				flag = 1;
			}
			if(strcmp(search_id->id, node->right->left->token) == 0 && strcmp(search_id->group, "var") == 0 && flag1 == 0)
			{	
				strcpy(right_var_type, search_id->type);
				flag1 = 1;
			}
			if(search_id->next != NULL)
				search_id = search_id->next;
			else
				break;
		}
		if(flag == 0)
		{
			printf("line: %d - the variable %s don't exist in this scope.\n", yylineno, node->left->token);
			free(left_var_type);
			free(right_var_type);
			exit(0);
		}
		else
		{
			if(flag1 == 0)
			{
				printf("line: %d - the variable %s don't exist in this scope.\n", yylineno, node->right->left->token);
				free(left_var_type);
				free(right_var_type);
				exit(0);
			}
			if(strcmp(right_var_type, "char") == 0)
			{
				if (node->right->left->left != NULL)
				{
					printf("line: %d - char[] is not defined in the language.\n", yylineno);
					free(left_var_type);
					free(right_var_type);
					exit(0);
				}
			}
			else if(strcmp(right_var_type, "string") != 0)
			{
				printf("line: %d - can't assign %s to char.\n", yylineno, right_var_type);
				free(left_var_type);
				free(right_var_type);
				exit(0);
			}
			else if(strcmp(right_var_type, "string") == 0)
			 {
				 if(node->right->left->left == NULL)
				 {
					printf("line: %d - can't assign %s to char.\n", yylineno, right_var_type);
					free(left_var_type);
					free(right_var_type);
					exit(0);
				 }
				 else
				 {
					if (strcmp(node->right->left->left->left->type, "int") != 0)
					{
						printf("line: %d - string[index] must be an integer, got(%s)\n", yylineno, node->right->left->left->left->type);
						free(left_var_type);
						free(right_var_type);
						exit(0);
					}
				 }
			 }
		}
	}
	
	int flag = 0;
	int flag1 = 0;
	LinkedList* search_id = Peek(st);
	char* left_var_type = (char*)malloc(sizeof(char) * 30);
	char* right_var_type = (char*)malloc(sizeof(char) * 30);
	char* left_token = (char*)malloc(sizeof(char) * 30);
	int address_flag = 0;
	int pointer_flag = 0;	
	
	while(search_id->next != NULL)
	{
		search_id = search_id->next;
	}
	while(strcmp(search_id->group, "func") != 0 && strcmp(search_id->group, "block") != 0)
	{
		search_id = search_id->prev;
	}
	LinkedList* scope_tracker = search_id->scope;
	int flag_tracker = 0;
	if(strcmp(node->right->left->type, "int") == 0 || strcmp(node->right->left->type, "int*") == 0 || strcmp(node->right->left->type, "real") == 0 || strcmp(node->right->left->type, "real*") == 0
	   || strcmp(node->right->left->type, "char") == 0 || strcmp(node->right->left->type, "char*") == 0 || strcmp(node->right->left->type, "string") == 0)
	   {
		   strcpy(right_var_type, node->right->left->type);
		   flag1 = 1;
	   }
	if(strcmp(node->type, "ASSIGNMENT") == 0 && node->left->left != NULL && strcmp(node->left->left->type, "pointer") == 0)
	{
		strcpy(left_token, node->left->left->token);
	}
	if(strcmp(node->type, "ASSIGNMENT") == 0 && strcmp(node->right->left->type, "ADDRESS") == 0)
	{
		address_flag = 1;
	}
	if(strcmp(node->type, "ASSIGNMENT") == 0)
	{
		while(scope_tracker != NULL && flag_tracker == 0)
		{
			while(search_id != NULL || flag == 0 || flag1 == 0)
			{
				if((strcmp(search_id->id, node->left->token) == 0 || strcmp(search_id->id, left_token) == 0)&& (strcmp(search_id->group, "var") == 0 || strcmp(search_id->group, "parm") == 0) && flag == 0)
				{	
					strcpy(left_var_type, search_id->type);
					flag = 1;
				}
				if(address_flag == 0 && strcmp(node->right->left->type, "id") == 0 && strcmp(search_id->id, node->right->left->token) == 0 && (strcmp(search_id->group, "var") == 0 || strcmp(search_id->group, "parm") == 0) && 
				(strcmp(search_id->type, "char") == 0 || strcmp(search_id->type, "char*") == 0 || strcmp(search_id->type, "int") == 0 || strcmp(search_id->type, "int*") == 0 || strcmp(search_id->type, "real") == 0 || strcmp(search_id->type, "real*") == 0 || strcmp(search_id->type, "bool") == 0))
				{
					if(flag1 != 1)
					{
						strcpy(right_var_type, search_id->type);
						flag1 = 1;
					}
				}
				else if(address_flag == 0 && flag1 == 0 && (strcmp(node->right->left->type, "int") == 0  || strcmp(node->right->left->type, "int*") == 0 
						|| strcmp(node->right->left->type, "real") == 0 || strcmp(node->right->left->type, "real*") == 0 || strcmp(node->right->left->type, "char") == 0
						|| strcmp(node->right->left->type, "char*") == 0 || strcmp(node->right->left->type, "bool") == 0))
				{
					strcpy(right_var_type, node->right->left->type);
					flag1 = 2;
				}
				else if(address_flag == 0 && strcmp(node->right->left->type, "id") != 0 && flag1 == 0)
				{	
					if(strcmp(node->right->left->type, "2EXP") == 0)
						strcpy(right_var_type, UpdateAssOperator(node->right->left));
					else if(strcmp(node->right->left->type, "1EXP") == 0 && strcmp(node->right->left->token, "*") == 0)
					{
						strcpy(right_var_type, UpdateOperatorType(node->right->left->right));
						if(strcmp(right_var_type, "int*") == 0)
							strcpy(right_var_type, "int");
						if(strcmp(right_var_type, "real*") == 0)
							strcpy(right_var_type, "real");
						if(strcmp(right_var_type, "char*") == 0)
							strcpy(right_var_type, "char");
					}
					
					flag1 = 2;
				}
				if(address_flag == 1 && strcmp(search_id->id, node->right->left->right->token) == 0 && strcmp(search_id->group, "var") == 0)
				{
					strcpy(right_var_type, search_id->type);
					flag1 = 1;
				}
				else if(address_flag == 1 && strcmp(node->right->left->right->type, "id") != 0 && flag1 == 0)
				{
					strcpy(right_var_type, node->right->left->right->type);
					flag1 = 2;
				}

				search_id = search_id->next;
				if(flag == 1 && flag1 != 0)
					flag_tracker = 1;
				if(search_id == NULL)
				{
					break;
				}
			}

			if(scope_tracker == NULL)
				break;
			else if(flag_tracker == 0)
			{
				search_id = scope_tracker;
				scope_tracker = scope_tracker->scope;
			}
		}
		if(strcmp(right_var_type, left_var_type) != 0 && flag == 1 && strcmp(right_var_type, "id") != 0 && address_flag == 0)
		{	
			if(strcmp(left_var_type, "int*") != 0 && strcmp(left_var_type, "real*") != 0 && strcmp(left_var_type, "char*") != 0 && strcmp(right_var_type, "none") == 0)
			{
				printf("line: %d - %s type can't assigned to %s type.\n", yylineno, right_var_type, left_var_type);
				free(left_var_type);
				exit(0);
			}
			else if(strcmp(right_var_type, "ABS") == 0 && strcmp(left_var_type, "int") != 0)
			{
				printf("line: %d - %s type can't assigned to %s type.\n", yylineno, right_var_type, left_var_type);
				free(left_var_type);
				exit(0);
			}
			else if(strcmp(right_var_type, "ABS") != 0 && strcmp(right_var_type, "none") != 0 && strcmp(right_var_type, left_var_type) != 0)
			{
				if(strcmp(left_var_type, "char") == 0)
				{
					 if(node->right->left->left == NULL)
				 	{
						printf("line: %d - can't assign %s to char.\n", yylineno, right_var_type);
						free(left_var_type);
						free(right_var_type);
						exit(0);
				 	}
					else
				 	{
						if (strcmp(node->right->left->left->left->type, "int") != 0)
						{
							if(strcmp(node->right->left->left->left->type, "id") == 0)
							{
								int f = 0;
								LinkedList* s_id = Peek(st);
								char* l_var = (char*)malloc(sizeof(char) * 30);
							
								while(s_id != NULL && f == 0)
								{
									if(strcmp(s_id->id, node->right->left->left->left->token) == 0 && strcmp(s_id->group, "var") == 0)
									{	
										strcpy(l_var, s_id->type);
										f = 1;
									}
									
									s_id = s_id->next;
								}
								if(strcmp(l_var, "int") != 0)
								{
									printf("line: %d - string[index] must be an integer, got(%s)\n", yylineno, l_var);
									free(left_var_type);
									free(right_var_type);
									free(l_var);
									exit(0);
								}
							}
							else
							{
								printf("line: %d - string[index] must be an integer, got(%s)\n", yylineno, node->right->left->left->left->type);
								free(left_var_type);
								free(right_var_type);
								exit(0);
							}
						}
						if(strcmp(right_var_type,"string") != 0)
						{
							printf("line: %d - can't assign %s to char.\n", yylineno, right_var_type);
							free(left_var_type);
							free(right_var_type);
							exit(0);
						}
				 	}
				}
			}
		}
		else if(address_flag == 1 && strcmp(right_var_type, left_var_type) == 0)
		{
			printf("line: %d - can't assign \"&\" of same variables type (%s).\n", yylineno, left_var_type);
			free(left_var_type);
			free(right_var_type);
			exit(0);
		}
		else if(address_flag == 1 && strcmp(right_var_type, left_var_type) != 0)
		{
			if( (strcmp(left_var_type, "int*") == 0 && strcmp(right_var_type, "int") != 0) || (strcmp(left_var_type, "real*") == 0 && strcmp(right_var_type, "real") != 0) ||
				(strcmp(left_var_type, "char*") == 0 && (strcmp(right_var_type, "char") != 0 && (strcmp(right_var_type, "string") != 0 && node->right->left->right->left->left == NULL)))
				|| strcmp(left_var_type, "int") == 0 || strcmp(left_var_type, "real") == 0 || strcmp(left_var_type, "char") == 0 || strcmp(left_var_type, "bool") == 0 ||
				strcmp(left_var_type, "string") == 0)
				{   
					printf("line: %d - can't assign \"&\" of %s to %s.\n", yylineno, right_var_type, left_var_type);
					free(left_var_type);
					free(right_var_type);
					exit(0);
				}
		}
		else if(flag == 0)
		{
			printf("line: %d - the variable (%s) is uninitialized.\n", yylineno, node->left->token);
			exit(0);
		}
		if(flag1 == 0)
		{
			printf("line: %d - the variable (%s) is uninitialized.\n", yylineno, node->right->left->token);
			exit(0);
		}
		if(strcmp(right_var_type, left_var_type) != 0)
		{
			if((strcmp(left_var_type, "int*") == 0 && strcmp(right_var_type, "int") == 0) || (strcmp(left_var_type, "real*") == 0 && strcmp(right_var_type, "real") == 0) ||
				(strcmp(left_var_type, "char*") == 0 && (strcmp(right_var_type, "char") == 0 || strcmp(right_var_type, "string") == 0)))
			{
				
			}
			else
			{
				printf("line: %d - %s type can't assigned to %s type.\n", yylineno, right_var_type, left_var_type);
				free(left_var_type);
				exit(0);
			}


		}
	}
	free(left_var_type);
	free(right_var_type);
}

void CheckFunc(LinkedList* scope, node* node, char* word)
{
	LinkedList* temp = scope->scope;
	if (strcmp(node->left->token, "main") == 0 && strcmp(scope->scope->id, "global") != 0)
	{
		printf("line: %d - main() must be at global, not nested function.\n", yylineno);
		exit(0);
	}
	else
	{
		while(temp->next != NULL)
		{	
			if(strcmp(word, temp->id) == 0)
			{
				printf("line: %d - there is one or more functions with the same name at the same scope. (%s)\n", yylineno, word);
				exit(0);
			}
			temp = temp->next;
		}
	}
}

void CheckFuncCall(node* node, LinkedList* scope)
{
	char* func_name = (char*)malloc(sizeof(char) * 30);
	strcpy(func_name, node->token);
	int flag = 0;
	LinkedList* track = scope, *func;
	LinkedList* temp = track;
	while(track->next != NULL)
		track = track->next;
	while(track != NULL && strcmp(track->group, "func") != 0)
	{
		track = track->prev;
	}
	while(track != NULL && flag == 0)
	{
		while(temp != NULL && flag == 0)
		{
			if(strcmp(func_name, temp->id) == 0)
			{
				flag = 1;
				func = temp;
			}
			if(flag == 0 && strcmp(track->id, "global") == 0)
			{
				int index = st->top;
				LinkedList* temp;

				while(index != -1 && flag == 0)
				{
					temp = GetByIndex(st, index);
					if(strcmp(temp->id, func_name) == 0)
					{
						func = temp;
						flag = 1;
					}
					if(flag == 1)
						break;
					index = index - 1;
				}
			}
			if(flag == 1)
				break;
			temp = temp->next;
		}
		if(flag == 1)
			break;
		temp = track;
		track = track->scope;
	}
	if(flag == 0)
	{	
		PrintStack(st);
		printf("line: %d - the function %s don't declare in this scope.\n", yylineno, func_name);
		free(func_name);
		exit(0);
	}

	int func_parms_call;
	int func_parms_org = func->parms_num;
	while(func->next != NULL && strcmp(func->group, "parm") != 0)
		func = func->next;

	func_parms_call = CheckFuncCallParmsNum(node->right, scope, 0, func);
	if(node->right->left != NULL && func_parms_call == 0)
		func_parms_call = 1;
	else if(node->right->left != NULL && func_parms_call > 0)
		func_parms_call = func_parms_call + 1;
	if(func_parms_org != func_parms_call)
	{
		printf("line %d - the actuall arguments number (%d) don't macth the formal arguments number (%d).\n", yylineno, func_parms_call, func_parms_org);
		free(func_name);
		exit(0);
	}

}

void CheckFuncCallAss(node* node, LinkedList* scope)
{
	char* var_name = (char*)malloc(sizeof(char) * 30);
	strcpy(var_name, node->left->token);
	char* var_type = (char*)malloc(sizeof(char) * 30);
	char* func_name = (char*)malloc(sizeof(char) * 30);
	strcpy(func_name, node->right->token);
	char* func_type = (char*)malloc(sizeof(char) * 30);
	int flag = 0;
	LinkedList* track = scope, *func;
	LinkedList* temp;
	temp = scope;
	track = track->scope;
	while(track != NULL && flag == 0)
	{
		while(temp != NULL && flag == 0)
		{
			if(strcmp(var_name, temp->id) == 0 && (strcmp(temp->group, "var") == 0 || strcmp(temp->group, "parm") == 0))
			{
				flag = 1;
				strcpy(var_type, temp->type);
			}
			temp = temp->next;
		}
		temp = track;
		track = track->scope;
	}
	if(flag == 0)
	{
		printf("line: %d - the variable %s don't declare in this scope.\n", yylineno, var_name);
		exit(0);
	}	
	flag = 0;
	track = scope;
	temp = track;
	while(track != NULL && flag == 0)
	{
		while(temp != NULL && flag == 0)
		{
			if(strcmp(func_name, temp->id) == 0 && (strcmp(temp->group, "func") == 0 || strcmp(temp->group, "closed") == 0))
			{
				flag = 1;
				func = track;
				strcpy(func_type, temp->type);
			}
			temp = temp->next;
			if(flag == 1)
				break;
		}
		if(flag == 0 && strcmp(track->id, "global") == 0)
		{
			int index = st->top;
			LinkedList* temp;

			while(index != -1 && flag == 0)
			{
				temp = GetByIndex(st, index);
				if(strcmp(temp->id, func_name) == 0)
				{
					strcpy(func_type, temp->type);
					flag = 1;
				}
				if(flag == 1)
					break;
				index = index - 1;
			}
		}
		track = track->scope;
		temp = track;
		if(flag == 1)
			break;
	}
	if(flag == 0)
	{
		printf("line: %d - the function %s don't declare in this scope.\n", yylineno, func_name);
		free(func_type);
		free(func_name);
		free(var_name);
		free(var_type);
		exit(0);
	}
	else if(strcmp(var_type, func_type) != 0)
	{
		printf("line: %d - the function %s return type (%s) don't match the variable %s type (%s).\n", yylineno, func_name, func_type, var_name, var_type);
		free(func_type);
		free(func_name);
		free(var_name);
		free(var_type);
		exit(0);
	}
}


int CheckFuncCallParmsNum(node* node, LinkedList* scope, int count, LinkedList* func)
{	
	if(strcmp(node->type, "FUNC_CALL") == 0)
	{
		int index = 0;
		while(index < count)
		{
			func = func->next;
			index = index + 1;
		}
		int flag = 0;
		LinkedList* save_scope = scope;
		LinkedList* track = scope, *temp = scope;
		while(track != NULL && flag == 0)
		{
			while(temp != NULL)
			{
				if(strcmp(node->token, temp->id) == 0)
				{
					if(strcmp(temp->type, func->type) != 0 && (strcmp(temp->group, "func") == 0 || strcmp(temp->group, "closed") == 0))
					{
					printf("line %d - the the function (%s) return type (%s) doesn't match the argument (%s) type (%s).\n", yylineno, node->token, temp->type, func->id, func->type);
					exit(0);
					}
					flag = 1;
				}
				if(flag == 1)
					break;
				temp = temp->next;
			}
			if(flag == 1)
				break;
			temp = track->scope;
			track = track->scope;
		}
		if(flag == 0)
		{
			printf("line %d - the function %s doesn't exist in this scope.\n", yylineno, node->token);
			exit(0);
		}

		CheckFuncCallParmsNum(node->right, scope, 0, func);
		count = count + 1;
	}
	else if(strcmp(node->token, ", ") == 0)
	{
		count = count + 1;
	}
	else if((strcmp(node->type, "id") == 0 || strcmp(node->type, "int") == 0 || strcmp(node->type, "int*") == 0 ||
	   strcmp(node->type, "real") == 0 || strcmp(node->type, "real*") == 0 || strcmp(node->type, "char") == 0 ||
	   strcmp(node->type, "char*") == 0 || strcmp(node->type, "bool") == 0 || strcmp(node->type, "FUNC_CALL") == 0) && node->visit != 1)
	   {	
			int index = 0;
			while(index < count)
			{
				func = func->next;
				index = index + 1;
			}
			int flag = 0;
			LinkedList* save_scope = scope;
			LinkedList* track = scope, *temp = scope;
			if(strcmp(node->type, "id") == 0)
			{
				while(track != NULL && flag == 0)
				{
					while(temp != NULL)
					{
						if(strcmp(node->token, temp->id) == 0)
						{
							if(strcmp(temp->type, func->type) != 0 && (strcmp(func->group, "parm") == 0 || strcmp(temp->group, "var") == 0))
							{
								printf("line %d - the argument (%s) type (%s) doesn't match the argument (%s) type (%s).\n", yylineno, node->token, temp->type, func->id, func->type);
								exit(0);
							}
							flag = 1;
						}
						if(flag == 1)
							break;
						temp = temp->next;
					}
					if(flag == 1)
						break;
					temp = track->scope;
					track = track->scope;
				}
				if(flag == 0)
				{
					printf("line %d - the argument %s doesn't exist in this scope.\n", yylineno, node->token);
					exit(0);
				}
			}
	   }

	node->visit = 1;
	if(node->left != NULL && node->left->visit != 1)
		 count = CheckFuncCallParmsNum(node->left, scope, count, func);
	if(node->right != NULL && node->right->visit != 1)
		count = CheckFuncCallParmsNum(node->right, scope, count, func);

	return count;
}


void CheckMain(LinkedList* scope)
{
	LinkedList* temp = Peek(st);
	if(strcmp(temp->id, "main") != 0)
	{
		printf("line: %d - main() must be the last function in the global.\n", yylineno);
		exit(0);
	}	
	else
	{
		if(temp->parms_num > 0)
		{
			printf("line: %d - main() function doesn't get parameters.\n", yylineno); 
			exit(0);
		}
	}

	int index = st->top, count = 0;

	while (index > -1)
	{
		temp = GetByIndex(st, index);
		if(strcmp(temp->id, "main") == 0)
		{
			count = count + 1;
			if(count > 1)
			{
				printf("line: %d - program must include only one main().\n", yylineno);
				exit(0);
			}
		}
		index = index - 1;
	}
}

void CheckGlobal(LinkedList* scope, char* word)
{
	int index = st->top;
	LinkedList* temp;

	while (index > -1)
	{
		temp = GetByIndex(st, index);
		if(strcmp(temp->id, word) == 0)
		{
			printf("line: %d - there is one or more functions with the same name at the same scope. (%s)\n", yylineno, word);
			exit(0);
		}
		index = index - 1;
	}
}

void UpdateVisit(node* node)
{
	node->visit = 0;

	if(node->left != NULL)
		UpdateVisit(node->left);
	if(node->right != NULL)
		UpdateVisit(node->right);
}
void PrintTree(node* node, int tab)
{
	int flag = 0;
	if(strcmp(node->token, "FUNC") == 0 && node->visit == 0)
	{
		flag = 1;
		for(int i = 0; i < tab + 1; i++)	
			printf("  ");
		printf("(%s\n", node->token);
		PrintFunc(node, tab + 1);
	}
	else if(strcmp(node->token, "{") == 0 || strcmp(node->token, "{{") == 0 && node->visit == 0)
	{
		for(int i = 0; i < tab + 2; i++)
			printf("  ");
		printf("(BODY");
		PrintBody(node, tab + 2);
		for(int i = 0; i < tab + 2; i++)			/* close body */
			printf("  ");
		printf(")\n");
		for(int i = 0; i < tab + 1; i++)			/* close func */
			printf("  ");
		printf(")\n");
	}

	node->visit = 1;
	if(node->left != NULL && node->left->visit == 0)
		PrintTree(node->left, tab);
	if(node->right != NULL && node->right->visit == 0)
		PrintTree(node->right, tab);
}

void PrintFunc(node* node, int tab)										
{
	for(int i = 0; i < tab + 1; i++)						/* func name */
		printf("  ");
	printf("%s\n", node->left->token);

		for(int i = 0; i < tab + 1; i++)
			printf("  ");
		printf("(ARGS\n");
	if(node->left->left->left->left != NULL)			   	/* args */
	{
		for(int i = 0; i < tab +2; i++)
			printf("  ");
		printf("(");
		PrintArgs(node->left->left->left, tab + 1);
		printf(")\n");
	}
	else
	{
		for(int i = 0; i < tab + 2; i++)
			printf("  ");
		printf("(NO PARAMETERS)\n");
	}
	for(int i = 0; i < tab + 1; i++)
			printf("  ");
	printf(")\n");
	for(int i = 0; i < tab + 1; i++)						/* return value */
			printf("  ");
	printf("(RET %s)\n", node->left->right->left->token);

	node->visit = 1;
}

void PrintArgs(node* node, int tab)
{
	if(strcmp(node->token, "") != 0)
		printf("%s ", node->token);

	node->visit = 1;
	if(node->left != NULL && node->left->visit != 1)
		PrintArgs(node->left, tab);
	if(node->right != NULL && node->right->visit != 1)
		PrintArgs(node->right, tab);
}

void PrintBody(node* node, int tab)
{
	if(strcmp(node->token, "DECLARATIONS") == 0 && node->visit == 0)
	{
		PrintDeclarations(node->left, tab + 1);
		printf("\n");
	}
	if((strcmp(node->type, "ASSIGNMENT") == 0 || strcmp(node->type, "CHAR_ASSIGNMENT") == 0) && node->visit == 0)
		PrintAssignment(node, tab + 1);
	if(strcmp(node->token, "IF") == 0)
	{
		PrintIf(node, tab + 1);
		if(node->right->right != NULL)
		{
			for(int i = 0; i < tab + 1; i++)
				printf("  ");
			printf("(ELSE\n");
			for(int i = 0; i < tab + 1; i++)
				printf("  ");
			printf("(BODY\n");
			PrintBody(node->right->right, tab + 1);
			for(int i = 0; i < tab + 1; i++)
				printf("  ");
			printf(")\n");
		}
	}
	if(strcmp(node->token, "WHILE") == 0)
		PrintWhile(node, tab + 1);
	if(strcmp(node->token, "FOR") == 0)
	{
		PrintFor(node, tab + 1);
		printf("\n");
		for(int i = 0; i < tab + 1; i++)
				printf("  ");
		printf("(BODY");
		PrintBody(node->right, tab + 1);
		printf("\n");
		for(int i = 0; i < tab + 1; i++)
			printf("  ");
		printf(")");
	}
	if(strcmp(node->token, "DO_WHILE") == 0)
		PrintDoWhile(node, tab + 1);
	
	if(strcmp(node->type, "FUNC_CALL") == 0 || strcmp(node->type, "FUNC_CALL_ASS") == 0)
	{
		printf("\n");
		for(int i = 0; i < tab + 1; i++)
			printf("  ");
		printf("(FUNC_CALL\n");
		
		if(strcmp(node->type, "FUNC_CALL_ASS") == 0)
		{
			for(int i = 0; i < tab + 2; i++)
				printf("  ");
			printf("(= %s  ", node->left->token);
			PrintFuncCall(node->right, tab + 1);
			printf("\n");
			for(int i = 0; i < tab + 2; i++)
				printf("  ");
			printf(") \n");
		}
		else
		{
			for(int i = 0; i < tab + 2; i++)
				printf("  ");
			printf("%s(", node->token);
			PrintFuncCall(node->right, tab + 1);
			printf(")\n");
		}
		for(int i = 0; i < tab + 1; i++)
			printf("  ");
		printf(")");
	}
	if(strcmp(node->token, "RETURN_STM") == 0)
	{
		printf("\n");
		for(int i = 0; i < tab + 2; i++)
				printf("  ");
		printf("RETURN ");
		PrintExp(node->left->left, tab + 1);
		printf("\n");
	}

	node->visit = 1;
	if(node->left != NULL && node->left->visit == 0)
		PrintBody(node->left, tab);
	if(node->right != NULL && node->right->visit == 0)
		PrintBody(node->right, tab);
}

void PrintDeclarations(node* node, int tab)
{
	if(strcmp(node->token, "FUNC") == 0 && node->visit == 0)
	{
		printf("\n");
		PrintTree(node, tab);
	}
	else if(strcmp(node->token, "int") == 0 || strcmp(node->token, "int*") == 0 || strcmp(node->token, "real") == 0 || strcmp(node->token, "real*") == 0 || strcmp(node->token, "char") == 0 
	   || strcmp(node->token, "char*") == 0 || strcmp(node->token, "string") == 0 || strcmp(node->token, "bool") == 0&& node->visit == 0)
	{
		printf("\n");
		for(int i = 0; i < tab; i++)	
			printf("  ");
		printf("%s ", node->token);
	}
	else if(strcmp(node->token, "=") == 0 && node->visit == 0)
		printf(" = ");
	else if((strcmp(node->type, "var") == 0 || strcmp(node->type, "int") == 0 || strcmp(node->type, "int*") == 0 || strcmp(node->type, "real") == 0 || strcmp(node->type, "real*") == 0 
			|| strcmp(node->type, "char") == 0 || strcmp(node->type, "char*") == 0 || strcmp(node->type, "string") == 0 || strcmp(node->token, "[") == 0 || strcmp(node->token, "]") == 0) && node->visit == 0)
	{
			printf("%s ", node->token);
	}

	node->visit = 1;
	if(node->left != NULL && node->left->visit == 0)
		PrintDeclarations(node->left, tab);
	if(node->right != NULL && node->right->visit == 0)
		PrintDeclarations(node->right, tab);
}

void PrintAssignment(node* node, int tab)
{
	for(int i = 0; i < tab; i++)						
		printf("  ");
	printf("= %s ", node->left->token);
	PrintExp(node->right->left, tab);
	printf("\n");
	node->visit = 1;
}

void PrintExp(node* node, int tab)
{
	if (strcmp(node->type, "int") == 0 || strcmp(node->type, "int*") == 0 || 
		strcmp(node->type, "real") == 0 || strcmp(node->type, "real*") == 0 ||
		strcmp(node->type, "char") == 0 || strcmp(node->type, "char*") == 0 ||
		strcmp(node->type, "id") == 0 || strcmp(node->type, "string") == 0)
	{
		printf("%s ", node->token);
	}
	else if (strcmp(node->type, "2EXP") == 0 || strcmp(node->type, "1EXP") == 0 || 
		strcmp(node->type, "ADDRESS") == 0 || strcmp(node->type, "ABS") == 0)
		{
			printf("\n");
			for(int i = 0; i < tab + 1; i++)
				printf("  ");
			printf("%s ", node->token);
		}
	
	node->visit = 1;
	if(node->left != NULL && node->left->visit ==0)
		PrintExp(node->left, tab);
	if(node->right != NULL && node->right->visit ==0)
		PrintExp(node->right, tab);
}
void PrintIf(node* node, int tab)
{
	for(int i = 0; i < tab; i++)	
		printf("  ");
	for(int i = 0; i < tab; i++)	
		printf(" ");
	printf("(COND");
	for(int i = 0; i < tab; i++)	
		printf("  ");
	PrintExp(node->left->left->left, tab);
	printf("\n");
	for(int i = 0; i < tab + 1; i++)	
		printf("  ");
	printf(")\n");
	for(int i = 0; i < tab; i++)	
		printf("  ");
	printf(")\n");
	for(int i = 0; i < tab; i++)
		printf("  ");
	printf("(BODY");							/* open body */
	printf("\n");
	PrintBody(node->left->right, tab);
	for(int i = 0; i < tab; i++)			/* close body */
		printf("  ");
	printf(")\n");
}

void PrintWhile(node* node, int tab)
{
	for(int i = 0; i < tab; i++)	
		printf("  ");
	printf("(WHILE");
	for(int i = 0; i < tab; i++)	
		printf("  ");
	PrintExp(node->left->left, tab);
	printf("\n");
	for(int i = 0; i < tab; i++)	
		printf("  ");
	printf(")\n");
	for(int i = 0; i < tab; i++)
		printf("  ");
	printf("(BODY");							/* open body */
	printf("\n");
	if(node->right)	
		PrintBody(node->right, tab);
	else
		PrintBody(node->right->left->left, tab);
	printf("\n");
	for(int i = 0; i < tab; i++)			/* close body */
		printf("  ");
	printf(")\n");
}

void PrintFor(node* node, int tab)
{
	for(int i = 0; i < tab; i++)	
		printf("  ");
	printf("(FOR\n");
	for(int i = 0; i < tab; i++)	
		printf("  ");
	printf("for_init: \n");
	for(int i = 0; i < tab + 1; i++)	
		printf("  ");
	printf("%s %s %s \n", node->left->left->left->token, node->left->left->token, node->left->left->right->token);
	for(int i = 0; i < tab; i++)	
		printf("  ");
	printf("for_exp:  ");
	PrintExp(node->left->right->left->right->left, tab);
	printf("\n");
	for(int i = 0; i < tab; i++)	
		printf("  ");
	printf("for_update: \n");
	for(int i = 0; i < tab + 1; i++)	
		printf("  ");
	printf("%s %s ", node->left->right->right->right->left->token, node->left->right->right->right->token);
	PrintExp(node->left->right->right->right->right->left, tab);
}

void PrintDoWhile(node* node, int tab)
{
	for(int i = 0; i < tab; i++)	
		printf("  ");
	printf("(DO_WHILE\n");
	for(int i = 0; i < tab; i++)	
		printf("  ");
	printf("(BODY");
	PrintBody(node->left, tab);
	printf("\n");
	for(int i = 0; i < tab; i++)	
		printf("  ");
	printf("\n");
	for(int i = 0; i < tab; i++)	
		printf("  ");
	printf("DO_EXP: ");
	for(int i = 0; i < tab + 2; i++)	
		printf("  ");
	PrintExp(node->right->left, tab);
	printf("\n");
	for(int i = 0; i < tab; i++)	
		printf("  ");
	printf(")");
}

void PrintFuncCall(node* node, int tab)
{	
	if(strcmp(node->type, "FUNC_CALL") == 0)
	{
		printf("%s( ", node->token);
		PrintFuncCall(node->right, tab + 1);
		printf(") ");
	}
	else if(strcmp(node->type, "id") == 0 || strcmp(node->type, "int") == 0 || strcmp(node->type, "int*") == 0 || 
			strcmp(node->type, "real") == 0 || strcmp(node->type, "real*") == 0 || strcmp(node->type, "char") == 0 || 
			strcmp(node->type, "char*") == 0 || strcmp(node->type, "bool") == 0)
				printf("%s ", node->token);
	
	node->visit = 1;
	if(node->left != NULL && node->left->visit == 0)
		PrintFuncCall(node->left, tab);
	if(node->right != NULL && node->right->visit == 0)
		PrintFuncCall(node->right, tab);
}



char* FreshVar()
{
	char* temp = (char*)malloc(sizeof(char) * 10);
	char* temp2 = (char*)malloc(sizeof(char) * 10);
	itoa( freshvar, temp);
	freshvar += 1;
	strcpy(temp2, "t");
	strcat(temp2, temp);
	return temp2;
}

char* FreshLable()
{
	char* temp = (char*)malloc(sizeof(char) * 10);
	char* temp2 = (char*)malloc(sizeof(char) * 10);
	itoa( freshlable, temp2);
	strcpy(temp, "   L");
	strcat(temp, temp2);
	strcat(temp, ":  ");
	freshlable = freshlable + 1;
	return temp;
}

void SetLable(node* root, node* node, int flag)
{
	if(node->left != NULL)
		SetLable(root, node->left, flag);

	char* temp = (char*)malloc(sizeof(char) * 10);
	char* temp2 = (char*)malloc(sizeof(char) * 10);

	if(strcmp(node->token, "IF") == 0)
	{
		strcpy(node->left->right->begin, FreshLable());

		if(strcmp(node->right->token, "ELSE") == 0)
		{
			strcpy(node->right->right->begin, FreshLable());
			strcat(node->code, node->begin);
			strcat(node->code, ShortCircuit(node->left->left->left, node->left->right->begin, node->right->right->begin));
			strcpy(node->after, FreshLable());
			strcat(node->code, "\n");
		}
		else
		{
			strcat(node->code, node->begin);
			strcpy(node->after, FreshLable());
			strcat(node->code, ShortCircuit(node->left->left->left, node->left->right->begin, node->after));
			strcat(node->code, "\n");
			strcat(node->code, node->left->right->begin);
		}
	}

	else if(strcmp(node->token, "WHILE") == 0) 
	{
		strcpy(node->begin, FreshLable());
		strcpy(node->right->begin, FreshLable());
		strcpy(node->after, FreshLable());

		strcat(node->code, node->begin);
		strcat(node->code, ShortCircuit(node->left->left, node->right->begin, node->after));
		strcat(node->code, "\n");
		strcat(node->code, node->right->begin);
	}

	else if(strcmp(node->token, "FOR") == 0)
	{
		strcpy(node->begin, FreshLable());
		strcpy(node->right->begin, FreshLable());
		strcpy(node->after, FreshLable());

		strcat(node->code, node->begin);
		strcat(node->code, ShortCircuit(node->left->right->left->right->left, node->right->begin, node->after));
		strcat(node->code, "\n");
		strcat(node->code, node->right->begin);
	}

	else if(strcmp(node->token, "DO_WHILE") == 0)
	{
		strcpy(node->right->begin, FreshLable());
		strcpy(node->begin, FreshLable());
		strcpy(node->after, FreshLable());
	}


	if(node->right != NULL)
		SetLable(root, node->right, flag);
}

char* ShortCircuit(node* node, char* truel, char* falsel)
{
	if(strcmp(node->token, "||") == 0)
	{
		char* code = (char*)malloc(sizeof(char) * 200);
		char* fl = (char*)malloc(sizeof(char) * 200);
		fl = FreshLable();
		strcat(code, ShortCircuit(node->left, truel, fl));
		strcat(code, "\n");
		strcat(code, fl);
		strcat(code, ShortCircuit(node->right, truel, falsel));
		return code;
	}
	
	if(strcmp(node->token, "&&") == 0)
	{
		char* code = (char*)malloc(sizeof(char) * 200);
		char* tl = (char*)malloc(sizeof(char) * 200);
		tl = FreshLable();
		strcat(code, ShortCircuit(node->left, tl, falsel));
		strcat(code, "\n");
		strcat(code, tl);
		strcat(code, ShortCircuit(node->right, truel, falsel));
		return code;
	}

	if(strcmp(node->token, "!") == 0)
	{
		char* code = (char*)malloc(sizeof(char) * 200);
		strcat(code, ShortCircuit(node->right, falsel, truel));
		return code;
	}

	if(strcmp(node->token, "||") != 0 && strcmp(node->token, "&&") != 0)
	{
		char* code = (char*)malloc(sizeof(char) * 200);
		strcat(code, node->code);

		if(strcmp(node->token, "true") == 0)
		{
			strcat(code, "Goto ");
			strcat(code, truel);
			return code;
		}
		else if(strcmp(node->token, "false") == 0)
		{
			strcat(code, "Goto ");
			strcat(code, falsel);
			return code;
		}
		else if(strcmp(node->token, "(") == 0 && strcmp(node->right->token, ")") == 0)
		{
			char* code = (char*)malloc(sizeof(char) * 200);
			strcat(code, ShortCircuit(node->left, truel, falsel));
			return code;
		}
		else
		{
			strcat(code, "if ");
			strcat(code, node->left->token);
			strcat(code, " ");
			strcat(code, node->token);
			strcat(code, " ");
			strcat(code, node->right->token);
			strcat(code, " Goto");
			strcat(code, truel);
			strcat(code, "\n        Goto");
			strcat(code, falsel);
			return code;
		}
	}
}

void SetVar(node* node)
{
	if(strcmp(node->token, "FUNC") == 0)
	{
		freshvar = 0;	
	}	

	else if(strcmp(node->type, "CHAR_ASSIGNMENT") == 0)
	{
		strcpy(node->left->left->var, FreshVar());
		strcpy(node->var,  FreshVar());
		strcpy(node->right->left->var,  FreshVar());
	}

	else if(strcmp(node->type, "DEC_ASS") == 0)
	{
		strcpy(node->left->var,  FreshVar());
	}

	else if(strcmp(node->type, "DEC_STR") == 0)
	{
		strcpy(node->left->var,  FreshVar());
	}

	else if(strcmp(node->type, "ASSIGNMENT") == 0)
	{
		strcpy(node->right->left->var,  FreshVar());
	}

	else if(strcmp(node->token, "RETURN_STM") == 0)
	{
		strcpy(node->var,  FreshVar());
	}

	else if(strcmp(node->type, "FUNC_CALL_ASS") == 0)
	{
		SetParmsVar(node->right);
		strcpy(node->var, FreshVar());
	}

	else if(strcmp(node->type, "FUNC_CALL") == 0 && node->visit == 0)
	{
		SetParmsVar(node);
		strcpy(node->var, FreshVar());
	}

	node->visit = 1;

	if(node->left != NULL)
		SetVar(node->left);
	if(node->right != NULL)
		SetVar(node->right);
}

void SetParmsVar(node* node)
{
	if(strcmp(node->type, "FUNC_CALL_PARMS") == 0)
		strcpy(node->left->left->var, FreshVar());
	
	if(node->left != NULL)
		SetParmsVar(node->left);
	if(node->right != NULL)
		SetParmsVar(node->right);
}

void SetCode(node* node)
{
	
	if(node->left != NULL && node->left->visit == 0)
		SetCode(node->left);
	if(node->right != NULL && node->right->visit == 0)
		SetCode(node->right);

	if(strcmp(node->token, "FUNC") == 0 && node->visit == 0)
	{
		strcat(node->code, node->left->token);
		strcat(node->code, ":\n");

		strcat(node->code, node->begin);

		strcat(node->code, "BeginFunc\n");
		if(node->left != NULL)
			strcat(node->code, node->left->code);
		if(node->right != NULL)
			strcat(node->code, node->right->code);
	}
	else if(strcmp(node->token, "}") == 0)
	{
		strcat(node->code, node->begin);
		strcat(node->code, "EndFunc\n\n");
	}
	else if((strcmp(node->type, "ASSIGNMENT") == 0 || strcmp(node->type, "CHAR_ASSIGNMENT") == 0)&& node->visit == 0)
	{	
		if(strcmp(node->type, "ASSIGNMENT") == 0)
		{
			if(strcmp(node->right->left->type, "2EXP") == 0)
			{
				strcat(node->code, node->begin);
				strcat(node->code, node->right->left->var);
				strcat(node->code, " = ");
				strcat(node->code, node->right->left->left->token);
				strcat(node->code, " ");
				strcat(node->code, node->right->left->token);
				strcat(node->code, " ");
				strcat(node->code, node->right->left->right->token);
				strcat(node->code, " \n");
				strcat(node->code, "        ");
				strcat(node->code, node->left->token);
				strcat(node->code, " = ");
				strcat(node->code, node->right->left->var);
				strcat(node->code, " \n");
			}
			else if(strcmp(node->right->left->type, "1EXP") == 0 && node->visit == 0)
			{
				strcat(node->code, node->begin);
				strcat(node->code, node->right->left->var);
				strcat(node->code, " = ");
				strcat(node->code, node->right->left->right->token);
				strcat(node->code, "\n        ");
				strcat(node->code, node->left->token);
				strcat(node->code, " = ");
				strcat(node->code, node->right->left->var);
				strcat(node->code, "\n        ");
			}
			else if(node->visit == 0 && (strcmp(node->right->left->type, "int") == 0  || strcmp(node->right->left->type, "int*") == 0 || strcmp(node->right->left->type, "real") == 0
						|| strcmp(node->right->left->type, "real*") == 0 || strcmp(node->right->left->type, "char") == 0 || strcmp(node->right->left->type, "char*") == 0 
						|| strcmp(node->right->left->type, "id") == 0 || strcmp(node->right->left->type, "bool") == 0 || strcmp(node->right->left->type, "string") == 0))
			{
				strcat(node->code, node->begin);
				strcat(node->code, node->right->left->var);
				strcat(node->code, " = ");
				strcat(node->code, node->right->left->token);
				strcat(node->code, "\n");
				strcat(node->code, "        ");

				if(strcmp(node->left->token, "*") == 0)
					strcat(node->code, node->left->left->token);
				else
					strcat(node->code, node->left->token);

				strcat(node->code, " = ");
				strcat(node->code, node->right->left->var);
				strcat(node->code, "\n");
			}
		}
		else if(strcmp(node->type, "CHAR_ASSIGNMENT") == 0)
		{
			strcat(node->code, node->begin);
			strcat(node->code, "i = 4\n");
			strcat(node->code, "        ");
			strcat(node->code, node->left->left->var);
			strcat(node->code, " = ");
			strcat(node->code, node->left->left->left->token);
			strcat(node->code, "\n        ");
			strcat(node->code, node->var);
			strcat(node->code, " = ");
			strcat(node->code, "i * ");
			strcat(node->code, node->left->left->var);
			strcat(node->code, "\n        ");
			strcat(node->code, node->right->left->var);
			strcat(node->code, " = ");
			strcat(node->code, node->right->left->token);
			strcat(node->code, "\n        ");
			strcat(node->code, node->left->token);
			strcat(node->code, "[");
			strcat(node->code, node->var);
			strcat(node->code, "] = ");
			strcat(node->code, node->right->left->var);
			strcat(node->code, "\n");
		}

		if(node->left != NULL)
			strcat(node->code, node->left->code);
		if(node->right != NULL)
			strcat(node->code, node->right->code);
	}

	else if(strcmp(node->type, "DEC_ASS") == 0)
	{

		if(node->left != NULL)
			strcat(node->code, node->left->code);
		if(node->right != NULL)
			strcat(node->code, node->right->code);

		strcat(node->code,  "        ");
		strcat(node->code,  node->left->var);
		strcat(node->code,  " = ");
		if (strcmp(node->left->right->left->left->type, "2EXP") == 0)
		{
			strcat(node->code,  node->left->right->left->left->left->token);
			strcat(node->code,  " ");
			strcat(node->code,  node->left->right->left->left->token);
			strcat(node->code,  " ");
			strcat(node->code,  node->left->right->left->left->right->token);
			strcat(node->code,  "\n");
		}
		else if(strcmp(node->left->right->left->left->type, "1EXP") == 0)
		{
			strcat(node->code,  node->left->right->left->left->token);
			strcat(node->code,  node->left->right->left->left->right->token);
			strcat(node->code,  "\n");
		}
		else
		{
			strcat(node->code,  node->left->right->left->left->token);
			strcat(node->code,  "\n");
		}

		strcat(node->code,  "        ");
		strcat(node->code,  node->left->token);
		strcat(node->code,  " = ");
		strcat(node->code,  node->left->var);
		strcat(node->code,  "\n");
	}

	else if(strcmp(node->type, "DEC_STR") == 0)
	{
		if(node->left != NULL)
			strcat(node->code, node->left->code);
		if(node->right != NULL)
			strcat(node->code, node->right->code);

		strcat(node->code,  "        ");
		strcat(node->code,  node->left->var);
		strcat(node->code,  " = ");
		strcat(node->code,  node->left->right->left->left->token);
		strcat(node->code,  "\n        ");
		strcat(node->code,  node->left->token);
		strcat(node->code,  " = ");
		strcat(node->code,  node->left->var);
		strcat(node->code,  "\n");
	}

	else if(strcmp(node->token, "IF") == 0 && node->visit == 0)
	{
		if(node->left != NULL)
			strcat(node->code, node->left->code);

		if(strcmp(node->right->token, "ELSE") == 0)
		{
			strcat(node->code, "        Goto");
			strcat(node->code, node->after);
			strcat(node->code, "\n");
		}
		if(node->right != NULL)
			strcat(node->code, node->right->code);
		strcat(node->code, node->after);

	}
	else if(strcmp(node->token, "WHILE") == 0 && node->visit == 0)
	{
		if(node->left != NULL)
			strcat(node->code, node->left->code);
		if(node->right != NULL)
			strcat(node->code, node->right->code);
		
		strcat(node->code, "        Goto");
		strcat(node->code, node->begin);
		strcat(node->code, "\n");
		strcat(node->code, node->after);
	}

	else if(strcmp(node->token, "FOR") == 0 && node->visit == 0)
	{
		if(node->left != NULL)
			strcat(node->code, node->left->code);
		if(node->right != NULL)
			strcat(node->code, node->right->code);
		
		strcat(node->code, "        ");
		strcat(node->code, node->left->right->right->right->left->token);
		strcat(node->code, " = ");
		strcat(node->code, node->left->right->right->right->right->left->left->token);
		strcat(node->code, " ");
		strcat(node->code, node->left->right->right->right->right->left->token);
		strcat(node->code, " ");
		strcat(node->code, node->left->right->right->right->right->left->right->token);
		strcat(node->code, "\n");
		strcat(node->code, "        Goto");
		strcat(node->code, node->begin);
		strcat(node->code, "\n");
		strcat(node->code, node->after);
	}

	else if(strcmp(node->token, "DO_WHILE") == 0 && node->visit == 0)
	{	
		strcat(node->code, node->right->begin);

		if(node->left != NULL)
			strcat(node->code, node->left->code);
		if(node->right != NULL)
			strcat(node->code, node->right->code);


		strcat(node->code, node->begin);
		strcat(node->code, ShortCircuit(node->right->left, node->right->begin, node->after));
		
		strcat(node->code, "\n");

		strcat(node->code, node->after);
	}

	else if(strcmp(node->type, "FUNC_CALL") == 0 && node->visit == 0)
	{
		int memory;
		char* temp = (char*)malloc(sizeof(char) * 10);
	
		Params(node);
		memory = CalaParams(node->token);

		if(node->left != NULL)
			strcat(node->code, node->left->code);
		if(node->right != NULL)
			strcat(node->code, node->right->code);

		strcat(node->code, "        LCall ");
		strcat(node->code, node->token);
		strcat(node->code, "\n");

		strcat(node->code, node->begin);
		strcat(node->code, "PopParams ");
		itoa(memory, temp);
		strcat(node->code, temp);
		strcat(node->code, "\n");
	}

	else if(strcmp(node->type, "FUNC_CALL_ASS") == 0)
	{
		int memory;
		char* temp = (char*)malloc(sizeof(char) * 10);

		node->right->visit = 1;
		
		if(node->left != NULL)
			strcat(node->code, node->left->code);
		if(node->right != NULL)
			strcat(node->code, node->right->code);

		strcat(node->code, "        ");
		strcat(node->code, node->left->token);
		strcat(node->code, " = ");
		strcat(node->code, node->var);
		strcat(node->code, "\n");
	}

	else if(strcmp(node->token, "RETURN_STM") == 0 && node->visit == 0)
	{
		if(node->left != NULL)
			strcat(node->code, node->left->code);
		if(node->right != NULL)
			strcat(node->code, node->right->code);
		
		if(strcmp(node->left->left->type, "2EXP") == 0)
		{
			strcat(node->code, node->begin);
			strcat(node->code, node->var);
			strcat(node->code, " = ");
			strcat(node->code, node->left->left->left->token);
			strcat(node->code, " ");
			strcat(node->code, node->left->left->token);
			strcat(node->code, " ");
			strcat(node->code, node->left->left->right->token);
			strcat(node->code, " \n");
		}
		else if(strcmp(node->left->left->type, "1EXP") == 0)
		{
			strcat(node->code, node->begin);
			strcat(node->code, node->var);
			strcat(node->code, " = ");
			strcat(node->code, node->left->left->token);
			strcat(node->code, node->left->left->right->left->token);
			strcat(node->code, " ");
			strcat(node->code, node->left->left->right->token);
			strcat(node->code, " ");
			strcat(node->code, node->left->left->right->right->token);
			strcat(node->code, " \n");
		}
		else
		{
			strcat(node->code, node->begin);
			strcat(node->code, node->var);
			strcat(node->code, " = ");
			strcat(node->code, node->left->left->token);
			strcat(node->code, " \n");
		}
			strcat(node->code, "        ");
			strcat(node->code, "Return ");
			strcat(node->code, node->var);
			strcat(node->code, " \n");
	}
	else
	{
		if(node->left != NULL)
			strcat(node->code, node->left->code);
		if(node->right != NULL)
			strcat(node->code, node->right->code);
	}
	node->visit = 1;
}

void Params(node* node)
{	
	if(strcmp(node->type, "FUNC_CALL_PARMS") == 0)
		if(strcmp(node->left->left->type, "2EXP") == 0)
		{
			strcat(node->code, node->left->left->var);
			strcat(node->code, " = ");
			strcat(node->code, node->left->left->left->token);
			strcat(node->code, " = ");
			strcat(node->code, node->left->left->token);
			strcat(node->code, " = ");
			strcat(node->code, node->left->left->right->token);

			strcat(node->code, "PushParam ");
			strcat(node->code, node->left->left->var);
			strcat(node->code, "\n");
		}
		else if(strcmp(node->left->left->type, "1EXP") == 0)
		{
			strcat(node->code, node->left->left->var);
			strcat(node->code, " = ");
			strcat(node->code, node->left->left->left->token);
			strcat(node->code, node->left->left->token);	

			strcat(node->code, "PushParam ");
			strcat(node->code, node->left->left->var);
			strcat(node->code, "\n");
		}
		else
		{
			strcat(node->code, node->left->left->var);
			strcat(node->code, " = ");
			strcat(node->code, node->left->left->token);
			strcat(node->code, "\n");

			strcat(node->code, node->begin);
			strcat(node->code, "PushParam ");
			strcat(node->code, node->left->left->var);
			strcat(node->code, "\n");
		}
	

	if(node->left != NULL)
		Params(node->left);
	if(node->right != NULL)
		Params(node->right);
}

int CalaParams(char* func_name)
{
	LinkedList* func;
	int count = 0;
	int index = st->top;

	while(index > -1)
	{
		func = GetByIndex(st, index);
		if(strcmp(func->id, func_name) == 0)
			break;

		index = index - 1;
	}

	func = func->next;
	while(strcmp(func->group, "parm") == 0)
	{
		if(strcmp(func->type, "int") == 0 || strcmp(func->type, "int*") == 0 || strcmp(func->type, "real*") == 0
		   || strcmp(func->type, "char*") == 0 || strcmp(func->type, "bool") == 0)
			count += 4;
		else if(strcmp(func->type, "real") == 0)
			count += 8;
		else if(strcmp(func->type, "char") == 0)
			count += 1;
		
		func = func->next;
	}

	return count;
}


void itoa(int n, char s[])
 {
     int i, sign;
 
     if ((sign = n) < 0)  /* record sign */
         n = -n;          /* make n positive */
     i = 0;
     do {       /* generate digits in reverse order */
         s[i++] = n % 10 + '0';   /* get next digit */
     } while ((n /= 10) > 0);     /* delete it */
     if (sign < 0)
         s[i++] = '-';
     s[i] = '\0';
     reverse(s);
 }
void reverse(char s[])
 {
     int i, j;
     char c;
 
     for (i = 0, j = strlen(s)-1; i<j; i++, j--) {
         c = s[i];
         s[i] = s[j];
         s[j] = c;
     }
 }

int yyerror()
{
	printf("\nSyntax error in line number: %d\nThe error is: '%s'.\n", yylineno, yytext);
	return 0;
}