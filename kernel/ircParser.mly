
%token EOL
%token <string> STRING
%token <IrcTypes.command> COMMAND

%start args
%type <string option * IrcTypes.command * string list> args
%%

params:
	| STRING params { $1 :: $2 }
	| EOL { [] }

args:
	| STRING COMMAND params { (Some $1, $2, $3) }
	| COMMAND params { (None, $1, $2) }
