(* Titre   : typeur.mli
   Auteur  : Patrick Sallé
   Contenu : interface du module typeur du langage miniml
*)

open Asyn;;
open Miniml;;
open Type_ml;;

exception Erreur_type;;
exception Undef of string;;

(* typer exp env sys 

prend en entrée:
   - une expression miniml exp
   - un environnement de typage env
   - un système d'équations sys

renvoie:
   - l'exception Undef s, si la variable s n'est pas définie
   - le type de exp sinon
   - et le système d'équations complété
*)

val typer: expr -> (string*ct) list -> (ct*ct) list -> ct*((ct*ct) list);;

(* Exemple:
#typer (Let("f",Fonction("x",Ident "x"),Call(Ident "f",Entier(1))))[][];;
- : ct * (ct * ct) list = Var 2, [Var 2, Int]
*)

(* resoudre tau sys 

prend en entrée 
   - un type tau 
   - et une liste d'équations de type sys

et renvoie:
   - l'exception Erreur_type si le système sys n'a pas de solution
   - le type issu de la résolution du système sinon
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

(* lit indéfiniment une expression du langage miniml et affiche son type *)

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
