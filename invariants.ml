open Printf

(* Définitions de terme, test et programme *)
type term = 
 | Const of int
 | Var of int
 | Add of term * term
 | Mult of term * term

type test = 
 | Equals of term * term
 | LessThan of term * term

let tt = Equals (Const 0, Const 0)
let ff = LessThan (Const 0, Const 0)
 
type program = {nvars : int; 
                inits : term list; 
                mods : term list; 
                loopcond : test; 
                assertion : test}

let x n = "x" ^ string_of_int n

(* 
   et `str_of_test : test -> string` qui convertissent des termes 
   et des tests en chaînes de caractères du format SMTLIB.

  Par exemple, str_of_term (Var 3) retourne "x3", str_of_term (Add
   (Var 1, Const 3)) retourne "(+ x1 3)" et str_of_test (Equals (Var
   2, Const 2)) retourne "(= x2 2)".  *)
let rec str_of_term t = 
  match t with  
  |Const(v)-> string_of_int (v)
  |Var(v)-> "x"^string_of_int (v)
  |Add(t1,t2) -> "(+ "^ str_of_term (t1)  ^ " " ^ str_of_term (t2)^")"
  |Mult(t1,t2) ->  "(* "^ str_of_term (t1)  ^ " " ^ str_of_term (t2)^")"

let str_of_test t = 
  match t with 
  |Equals (t1,t2) -> "(= "^ str_of_term (t1)  ^ " " ^ str_of_term (t2)^")"
  |LessThan (t1,t2) ->"(< "^ str_of_term (t1)  ^ " " ^ str_of_term (t2)^")"

let string_repeat s n =
  Array.fold_left (^) "" (Array.make n s)

(* 
   qui prend une liste de termes t1, ..., tk et retourne une chaîne 
   de caractères qui exprime que le tuple (t1, ..., tk) est dans 
   l'invariant.  Par exemple, str_condition [Var 1; Const 10] retourne 
   "(Invar x1 10)".
   *)
let str_condition l = 
  let rec aux l =
    match l with 
    |x::l' -> " "^str_of_term (x) ^ aux (l')
    |_-> ""
  in 
  "(Invar"^ aux (l)^")"

let rec list_var lter acc=
let rec extraire_var t=
match t with 
|Var(x)->[Var(x)]
|Add(x,y)->extraire_var (x) @ extraire_var (y)
|Mult(x,y)->extraire_var (x) @ extraire_var (y)
|_->[]
in
match lter with
|[]->List.rev (acc)
|x::l->
  match x with 
|Var(y)-> list_var (l) (Var(y)::acc)
|Add(y,z)->list_var (l) (extraire_var(y)@extraire_var(z)@acc)
|Mult(y,z)->list_var (l) (extraire_var(y)@extraire_var(z)@acc)
|Const(y)->list_var (l) (acc)


(* 
   `str_assert_for_all : int -> string -> string` qui prend en
   argument un entier n et une chaîne de caractères s, et retourne
   l'expression SMTLIB qui correspond à la formule "forall x1 ... xk
   (s)".

  Par exemple, str_assert_forall 2 "< x1 x2" retourne : "(assert
   (forall ((x1 Int) (x2 Int)) (< x1 x2)))".  *)

let str_assert s = "(assert " ^ s ^ ")"



let str_assert_forall n s = 
  let rec gen_var n i=
       if n=1 then
        "(x"^string_of_int(i)^" Int) "
    else
      "(x"^string_of_int(i)^" Int) "^ gen_var (n-1) (i+1)
  in "(forall ("^ gen_var (n) (1) ^") "^s 

  (*           *)
  let inverse_cond test =
    match test with 
    |Equals(x,y)-> "(> "^ str_of_term (x)  ^ " " ^ str_of_term (y)^")"
    |LessThan(x,y)-> "(>= "^ str_of_term (x)  ^ " " ^ str_of_term (y)^")"




let smtlib_of_wa p = 
  let declare_invariant n =
    "; synthèse d'invariant de programme\n"
    ^"; on déclare le symbole non interprété de relation Invar\n"
    ^"(declare-fun Invar (" ^ string_repeat "Int " n ^  ") Bool)" in
  let loop_condition p =
    "; la relation Invar est un invariant de boucle\n"
    ^  str_assert (str_assert_forall p.nvars ("(=> "^"and"^ (str_condition (list_var (p.mods)([])) )^" "^str_of_test(p.loopcond))^ " "^ str_condition(p.mods)^"))") 
  
  in
  let initial_condition p =
    "; la relation Invar est vraie initialement\n"
    ^str_assert (str_condition p.inits) in
  let assertion_condition p =
    "; l'assertion finale est vérifiée\n"
    ^  str_assert (str_assert_forall p.nvars ("(=> "^"and"
    ^(str_condition (list_var (p.mods)([])) )^" "^inverse_cond(p.loopcond))^ " "
    ^ str_of_test(p.assertion)^"))")
  in
  let call_solver =
    "; appel au solveur\n(check-sat-using (then qe smt))\n(get-model)\n(exit)\n" in
  String.concat "\n" [declare_invariant p.nvars;
                      loop_condition p;
                      initial_condition p;
                      assertion_condition p;
                      call_solver]

let p1 = {nvars = 2;
          inits = [(Const 0) ; (Const 0)];
          mods = [Add ((Var 1), (Const 1)); Add ((Var 2), (Const 3))];
          loopcond = LessThan ((Var 1),(Const 3));
          assertion = Equals ((Var 2),(Const 9))}




(*  Vérification que notrz implémentation donne un fichier
   SMTLIB qui est équivalent au fichier que nous avons écrit à la main
   dans l'exercice 1. Plus un exemple ajouter par souci de verification. *)

let p2 = {nvars = 3;
inits = [(Const 0) ; (Const 0); (Const 1)];
mods = [Add ((Var 1), (Const 1)); Add ((Var 2), (Const 3));Add ((Var 3), (Const 1))];
loopcond = LessThan ((Var 1),(Const 3));
assertion = Equals ((Var 3),(Const 9))}

let p3 = {nvars = 3;
        inits = [(Const 0) ; (Const 0) ; (Const 2)];
        mods = [Add ((Var 1), (Const 1)); 
                Add ((Var 2), (Const 1)); 
                Add((Var 3), (Const 2))];
        loopcond = LessThan ((Var 1),(Const 5));
        assertion = Equals ((Var 3),(Const 12))}



let () = Printf.printf "%s" (smtlib_of_wa p3)
