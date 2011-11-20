
{
open Lexing
open IrcParser
open IrcTypes
}

let letter = [^' ']
let digit  = ['0'-'9']

rule param = parse
	| ':'((letter|' ')* as s)		{ STRING s }
	| (letter+) as s				{ STRING s }
	| [' ']+						{ param lexbuf }
	| eof							{ EOL }

and command = parse
	| (digit digit digit) as num	{ COMMAND (Numeric (int_of_string num)) }
	| "JOIN"						{ COMMAND Join }
	| "PART"						{ COMMAND Part }
	| "MODE"						{ COMMAND Mode }
	| "TOPIC"						{ COMMAND Topic }
	| "NAMES"						{ COMMAND Names }
	| "LIST"						{ COMMAND List }
	| "INVITE"						{ COMMAND Invite }
	| "KICK"						{ COMMAND Kick }
	| "PRIVMSG"						{ COMMAND Privmsg }
	| "NOTICE"						{ COMMAND Notice }
	| "QUIT"						{ COMMAND Quit }
	| "PING"						{ COMMAND Ping }
	| (letter+) as s				{ COMMAND (Unknown s) }

and sender = parse
	| ((letter+) as s)[' ']+		{ STRING s }

and message = parse
	| ':'							{ sender lexbuf }
	| [' ']+						{ param lexbuf }
	| ""							{ command lexbuf }
	| eof							{ EOL }
