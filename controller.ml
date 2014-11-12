(*
Shivanker Goel
2011CS10298
Group 3

type tree = Empty
        | Var of string
        | Int of int | Real of float | Id of string
        | Pred of string*(tree list)
        | Rule of tree*(tree list)
;;

type action = Stop | Next;;

*)

(* Make debug true, if you want trace to be on. *)
let debug = try
        Sys.getenv "debug" = "true"
    with Not_found -> false;;

let print_string s = print_string s; flush stdout;;
let print_endline s = print_endline s; flush stdout;;

open Op;;
open Parser;;

(* Tree printing functions *)
let rec print_tl tl = match tl with
    [] -> ()
| [x] -> print_tree x
| x::xs -> print_tree x; print_string ", "; print_tl xs

and print_tree t = match t with
  Cut i -> print_string "Cut "; print_int i
| Var x -> print_string x
| Int i -> print_int i
| Real r -> print_float r
| Id s -> print_string s
| Pred (s,l) -> print_string (s^"("); print_tl l; print_string ")"
| Rule (r,rl) -> print_tree r; print_string " :- "; print_tl rl
| Empty -> ();
flush stdout;;


(* Inputting clauses *)
exception BREAK;;

let db = ref [];;
let temp = ref Empty;;

let pin =
  if Array.length Sys.argv > 1
  then open_in Sys.argv.(1)
  else stdin
in
let lexbuf = Lexing.from_channel pin in
try
    while true do
      try
        temp := Parser.clause Lexer.tokenize lexbuf;
        if !temp <> Empty then
            db := !temp :: !db;
      with End_of_file -> raise BREAK;
        |  Lexer.Unrecognized -> flush stdout;
    done;
with BREAK -> ();;

print_string "\n";;
print_int (List.length !db);;
print_string " clause(s) were parsed.\n";;

(* Substitutions and MGU *)
type substitution = (string * tree) list;;

let rec find (v : string) (s : substitution) : tree = match s with
    [] -> Var v
| (i,t)::xs ->  if i = v then t else find v xs;;

let rec subst (s : substitution) (t : tree) : tree = match t with
    Var v -> find v s
| Pred (sym,l) -> Pred(sym, List.map (subst s) l)
| Rule (t,l) -> Rule(subst s t, List.map (subst s) l)
| _ -> t;;

let rec occurs (v : string) (t : tree) : bool = match t with
    Var x -> (x = v)
| Pred (_,l) -> List.exists (occurs v) l
| Rule (t,l) -> List.exists (occurs v) (t::l)
| _ -> false;;

let rec mgu (t1 : tree) (t2 : tree) : substitution = 
    let multimgu (s : substitution) (t1 : tree) (t2 : tree) : substitution = s @ mgu (subst s t1) (subst s t2)
    in if t1 = t2 then [] else match (t1,t2) with
      (Var x, t) | (t, Var x)    -> if not(occurs x t) then [(x, t)] else failwith ""
    | (Pred (f,fl), Pred (g,gl)) -> if (f = g) && (List.length fl = List.length gl) 
                                    then List.fold_left2 multimgu [] fl gl
                                    else failwith ""
    | (Rule (a,al), Rule(b,bl))  -> if List.length al = List.length bl
                                    then List.fold_left2 multimgu [] (a::al) (b::bl)
                                    else failwith ""
    | (_, _) -> failwith ""
;;

(* Masking the variables *)
let v = ref 0;;
let varmask p = let s = ref [] in
    let rec apply p =
        let p = subst !s p in
        match p with
            Var var ->  if var.[0] <> '_'
                      then (
                        v := !v + 1;
                        s := (var, Var ("_V"^(string_of_int !v))) :: !s
                      )
        |   Pred (r, rl) -> List.iter apply rl
        |   Rule (r, rl) -> List.iter apply (r::rl)
        |   _ -> ()
    in apply p;
    !s;;

(* Pretty printing relevant variables from the substitutions *)
let rec lhsHelper (id,rep) sub = match sub with
    [] -> []
|   (s,t)::xs -> if id = s then lhsHelper (id,rep) xs
                else (s,subst [(id,rep)] t)::(lhsHelper (id,rep) xs)
;;

let rec rhsHelper (id, rep) sub = match sub with
    [] -> []
|   (s,t)::xs -> if rep = Var s then rhsHelper (id,rep) xs
                else (s,subst [(id,rep)] t)::(rhsHelper (id,rep) xs)
;;

let occam sub =
    let sub = ref sub in
    let rec cleanLHS cur = match cur with
            [] -> ()
        | (s,t)::xs ->  if s.[0] = '_' then 
                        (
                            sub := lhsHelper (s,t) !sub;
                            cleanLHS !sub
                        )
                        else cleanLHS xs
    and cleanRHS cur = match cur with
            [] -> ()
        | (s, Var v)::xs -> if v.[0] = '_' then
                            (
                                sub := rhsHelper (v, Var s) !sub;
                                cleanRHS !sub
                            )
                            else cleanRHS xs
        | x::xs -> cleanRHS xs
    in
    cleanLHS !sub;
    cleanRHS !sub;
    !sub;;

let rec print_sub sub = match sub with
    [] -> print_endline ""
| (s,t)::xs -> print_string (" "^s^" -> "); print_tree t; print_sub xs;;


(* Main stack procedures *)
let stack :((tree Queue.t)*substitution*int) Stack.t = Stack.create();;

(* for debugging *)
let trace() = 
    if not(debug) || Stack.is_empty stack then () else
    let q,s,id = Stack.top stack in
    print_int id;
    print_string ": ";
    let print t = print_string "{"; print_tree t; print_string "} " in
    print_string "[ ";
    Queue.iter print q;
    print_string "];";
    print_sub s

let pushrest q s qs =
    let qs = Queue.copy qs in 
    while not(Queue.is_empty qs) do
        Queue.push (subst s (Queue.pop qs)) q
    done;;

let cut = Hashtbl.create 42;;
let matchid = ref 0;;

let rec dbmatch p (qi,sub) db =
    match p with
      Cut i ->  let rec iter i = if i < !matchid
                    then ( Hashtbl.add cut i true; iter (i+1))
                in iter i;
                Stack.push (qi, sub, !matchid) stack
    | Pred (s,l) ->
        (
        match db with
            [] -> ((*fail*))
        | (Pred (r,rl) as pred)::dbs -> (try
                                            let m = varmask pred in
                                            let pred = subst m pred in
                                            let s = mgu pred p in
                                            let q = Queue.create() in
                                            pushrest q s qi; 
                                            Stack.push (q, sub@s, !matchid) stack;
                                            trace()
                                        with
                                            Failure s -> ());
                                        dbmatch p (qi,sub) dbs

        | (Rule ((Pred (r,rl) as pred), rll))::dbs ->   (try
                                                            let m = varmask pred in
                                                            let pred = subst m pred in
                                                            let s = mgu pred p in
                                                            let q = Queue.create() in
                                                            let qp t = Queue.push t q in
                                                            let rec pushbody m l = match l with
                                                                [] -> []
                                                            | (Cut(i))::xs -> (Cut(!matchid))::(pushbody m xs)
                                                            | x::xs ->  let sm = subst m x in
                                                                        let vm = varmask sm in
                                                                        let p = (subst s (subst vm sm)) in
                                                                        p::(pushbody (m@vm) xs) in
                                                            List.iter qp (pushbody m rll);
                                                            pushrest q s qi;
                                                            Stack.push (q, sub@s, !matchid) stack;
                                                            trace()
                                                        with
                                                            Failure s -> ());
                                                        dbmatch p (qi,sub) dbs
        | _ -> failwith "This was not supposed to be."
        )
    | _ -> failwith "This too was not supposed to be.";;

let rec print_success sub = let sub = occam sub in
    let rec print sub = match sub with 
            [] -> print_string "yes"
        |   [(s,t)] -> print_string (s^" = "); print_tree t
        | (s,t)::xs -> print_string (s^" = "); print_tree t; print_endline ","; print xs
    in print sub;
    flush stdout;
    if Stack.is_empty stack then print_endline "." else
    let act = Parser.action Lexer.tokenize (Lexing.from_channel stdin) in
    match act with Next -> solve() | Stop -> ()
and solve () = 
    if Stack.is_empty stack then print_endline "no." else
    let (q,sub,id) = Stack.pop stack in
    if Hashtbl.mem cut id then solve() else
    if Queue.is_empty q then print_success sub else
    let e = Queue.pop q in
    matchid := !matchid + 1;
    dbmatch e (q,sub) !db;
    solve();;


(* Inputting the queries *)
let nq = ref 0;;
let temp = ref [Empty];;

let qin =
  if Array.length Sys.argv > 2
  then open_in Sys.argv.(2)
  else stdin
in let lexbuf = Lexing.from_channel qin
in try
    while true do
      try
        print_string "\n?- ";
        temp := Parser.query Lexer.tokenize lexbuf;
        if !temp <> [Empty] then
            ( if qin <> stdin then
              ( print_tl !temp; print_endline "." );
              nq := !nq + 1;
              Stack.clear stack;
              v := 0;
              matchid := 0;
              Hashtbl.clear cut;
              let q = Queue.create() in
              let qp t = Queue.push t q in
              List.iter qp !temp;
              Stack.push (q, [], 0) stack;
              trace();
              solve() );
      with End_of_file -> raise BREAK;
        |  Lexer.Unrecognized -> flush stdout
        |  Failure s -> print_string s; flush stdout;
    done;
with BREAK -> ();;

print_string "\n";;
print_int !nq;;
print_string " query(-ies) were parsed.\n";;
