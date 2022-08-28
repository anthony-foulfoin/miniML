(* Titre   : type_ml.mli
   Auteur  : Patrick Sallé
   Contenu : interface du module type miniml
*)


(* Syntaxe abstraite des types *)

type ct = Int|Bool|Var of int|F of ct*ct| List of ct;;

(* ecriture d'un type à la ML *)

val string_of_type: ct -> string;;

(* 
string_of_type (F(Var 1, List(Var 2)));;
- : string = "('a -> ('b list))"
*)

exception Trop_de_variables_de_types;;
