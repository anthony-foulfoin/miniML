(* Titre   : miniml.ml
   Auteur  : Patrick Sallé
   Contenu : réalisation du module langage miniml
*)

(* type expr synatxe abstraite de miniml *)


(* Lecture d'expressions miniml *)


type expr = 
   Entier of int
  |Booleen of bool
  |Ident of string
  |Plus of expr*expr
  |Moins of expr*expr
  |Mult of expr*expr
  |Div of expr*expr
  |Neg of expr
  |Et of expr*expr
  |Ou of expr*expr
  |Non of expr
  |Egal of expr*expr
  |Inf of expr*expr
  |Infeg of expr*expr
  |Sup of expr*expr
  |Supeg of expr*expr
  |Diff of expr*expr
  |Hd of expr
  |Tl of expr
  |Cons of expr*expr
  |Nil
  |If of expr*expr*expr
  |Let of string*expr*expr
  |Fonction of string*expr
  |Call of expr*expr
  |Letrec of string*expr*expr
;;

