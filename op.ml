type tree = Empty
		| Cut of int
		| Var of string
		| Int of int | Real of float | Id of string
		| Pred of string*(tree list)
		| Rule of tree*(tree list)
;;

type action = Stop | Next;;