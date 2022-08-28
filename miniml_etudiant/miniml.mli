(* Titre   : miniml.mli
   Auteur  : Patrick Sall�
   Contenu : interface du module langage miniml
*)

(* Syntaxe abstraite de miniml *)

open Asyn;;

exception ErreurSyntaxique ;;

val  read_expr : unit -> expr
;;
