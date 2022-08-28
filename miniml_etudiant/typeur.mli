(* Titre   : typeur.mli
   Auteur  : Patrick Sall�
   Contenu : interface du module typeur du langage miniml
*)

open Asyn;;
open Miniml;;
open Type_ml;;

exception Erreur_type;;
exception Undef of string;;

(* typer exp env sys 

prend en entr�e:
   - une expression miniml exp
   - un environnement de typage env
   - un syst�me d'�quations sys

renvoie:
   - l'exception Undef s, si la variable s n'est pas d�finie
   - le type de exp sinon
   - et le syst�me d'�quations compl�t�
*)

val typer: expr -> (string*ct) list -> (ct*ct) list -> ct*((ct*ct) list);;

(* Exemple:
#typer (Let("f",Fonction("x",Ident "x"),Call(Ident "f",Entier(1))))[][];;
- : ct * (ct * ct) list = Var 2, [Var 2, Int]
*)

(* resoudre tau sys 

prend en entr�e 
   - un type tau 
   - et une liste d'�quations de type sys

et renvoie:
   - l'exception Erreur_type si le syst�me sys n'a pas de solution
   - le type issu de la r�solution du syst�me sinon
*)

val resoudre: ct -> (ct*ct) list -> ct;;

(*
resoudre (Var 2)  [Var 2,Var 3; Var 3, Int];;
- : ct = Int
*)

(* lit une expression du langage miniml et renvoie son type *)

val type_de: unit -> string;;

(* Exemple:
#type_de();;
let f = function x -> x
     in f;;
- : string = "('a -> 'a)"
*)

(* lit ind�finiment une expression du langage miniml et affiche son type *)

val top: unit -> unit;;

(* Exemple:
#top();;
? letrec app = function x ->
                function y -> 
   if x=nil then y else (hd x)::(app (tl x) (y))
in app;;
(('a list) -> (('a list) -> ('a list)))

? function x -> x+y;;
Uncaught exception: Undef "y"
#
*)
