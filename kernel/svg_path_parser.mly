%token EOL
%token <float> FLOAT
%token <char> OP

%start data
%type <(char * float list) list> data
%%

data:
	| op_list { $1 }
	| EOL { [] }
	;

op_list:
	/* empty */ { [] }
	| OP float_list op_list { ($1, $2) :: $3 }
	| EOL { [] }
	;

float_list:
	/* empty */ { [] }
	| FLOAT float_list { $1 :: $2 }
	| EOL { [] }
	;
