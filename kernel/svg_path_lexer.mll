{
open Lexing
open Svg_path_parser
}

let letter = ['A'-'Z' 'a'-'z']
let delim = ['\t' '\r' '\n' ',' ' ']
let digit = ['0'-'9']
let plusminus = ['-' '+']
let mantissa = plusminus? ((digit+ '.' digit*) | (digit* '.' digit+) | digit+)
let exponent = ('e' | 'E') plusminus? digit+
let floating = mantissa exponent?

rule data = parse
	| letter as c { OP c }
	| floating as s { FLOAT (float_of_string s) }
	| delim+ { data lexbuf }
	| eof { EOL }
