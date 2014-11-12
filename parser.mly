/*
Shivanker Goel
2011CS10298
Group 3
*/

%{
  open Op;;
%}

/* Ocamlyacc Declarations */
%token DOT COMMA IF SEMIC CUT
%token LPAREN RPAREN
%token <int> INT
%token <float> REAL
%token <string> VAR ID

%start clause
%type <Op.tree> clause

%start query
%type <Op.tree list> query

%start action
%type <Op.action> action

/* Grammar follows */
%%

query: 	error DOT				{ print_string "\tSyntax Error. Could not parse.\n"; flush stdout; [Empty] }
	| 	predseq DOT				{ $1 }
;

action: DOT 	{ Stop } 
	| 	SEMIC	{ Next }
	| 	error 	{ print_string "\tSyntax Error. Could not parse.\n"; flush stdout; Stop }
;

clause: error DOT				{ print_string "\tSyntax Error. Could not parse.\n"; flush stdout; Empty }
	| 	pred DOT				{ $1 }
	| 	pred IF predseq DOT		{ Rule ($1, $3) }
;

predseq: pred { [$1] }
	| CUT  { [Cut 0] }
	| pred COMMA predseq { $1::$3 }
	| CUT COMMA predseq { (Cut 0) :: $3 }
;

pred: ID LPAREN termseq RPAREN	{ Pred ($1, $3) };

termseq: term 			 { [$1] }
	| term COMMA termseq { $1::$3 }
;

term: ID { Id $1 } | num { $1 } | variable { $1 } | pred { $1 };

variable: VAR { Var $1 };

num:  INT { Int $1 }
	| REAL { Real $1 }
;

%%