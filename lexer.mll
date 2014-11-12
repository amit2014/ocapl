(*
Shivanker Goel
2011CS10298
Group 3
*)

{
  open Parser;;
  exception Unrecognized;;
  open Printf;;
}

let nolead = '0' | (['1'-'9'] ['0'-'9']*)
let signed = '0' | ('-'? ['1'-'9'] ['0'-'9']*)
let notrail = (['0'-'9']* ['1'-'9']) | '0'

rule tokenize = parse
  | nolead as num			{ INT (int_of_string num) }

  | (nolead '.' notrail? (['e' 'E'] signed)?)
  | (nolead ['e' 'E'] signed) as real
										  { 
												REAL (float_of_string real)
							  			}
  | ';'               { SEMIC }
  | ":-"							{ IF }
  | ')'     					{ RPAREN }
  | '('               { LPAREN }
  | '!'               { CUT }
   
  | (['A'-'Z']['0'-'9' 'a'-'z' 'A'-'Z' '\'' '_']*) as var
                      { VAR var }
  | (['a'-'z']['0'-'9' 'a'-'z' 'A'-'Z' '\'' '_']*) as id
  							      { ID id }
  | ','               { COMMA }
  | [' ' '\t' '\n']+  { tokenize lexbuf }
  | '.'						    { DOT }
  | _ as c
                      { 
                        printf "Unrecognized character %c\n" c;
                        raise Unrecognized
                      }
  | eof               { raise End_of_file }
