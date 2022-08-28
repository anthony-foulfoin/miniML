(* Titre   : type_ml.ml
   Auteur  : Patrick Sallé
   Contenu : implantation du module type miniml
*)

(* Calcul de type de miniml *)
(* Le principe:
   - le parcours récursif d'une expression miniml engendre

      * un environnement associant chaque variable rencontrée à
       une nouvelle variable de type 

      * une liste d'équations entre types représentant les contraintes
       entre les types

   - la résolution de l'ensemble des contraintes permet de construire
   le type de l'expression ou conduit a un échec 

*)

exception Au_revoir;;
exception Erreur_type;;
exception Undef of string;;

open List;;
open Asyn;;
open Miniml;;
open Type_ml;;

(* newvar() engendre à chaque appel une variable de type fraîche 
   Var 1 puis Var 2 ...

*)

let newvar = let mem=ref 0 in function ()-> incr mem;Var !mem;;

(*
##############################################################################
##                    RESOLUTION DU SYSTEME D'EQUATIONS                     ##
##############################################################################
*)

(*
Fonction substituer :  Type_ml.ct -> Type_ml.ct -> Type_ml.ct -> Type_ml.ct = <fun>
-------------------------
Cette fonction permet de substituer une variable à une autre dans un type.
-------------------------
Par exemple : 
substituer (Var 33) Int (F(Var 33, F(Var 22, Var 33)))  remplacera toutes les occurences de Var 33 par Int dans le type  F(Var 33, F(Var 22, Var 33))) 
Cet exemple aura donc pour resultat F(Int, F(Var 22, Int))) 
-------------------------
Parametres :
		- x : Type que l'on souhaiter remplacer par y
		- y : Type destinee a remplacer x
		- t : Type sujet de la substitution
Resultat :
		- Le type t dans lequel toutes les occurences de x ont ete remplacees par y
*)
let rec substituer x y t =
	match t with
	a 	when a = x -> y
	|
	List a 			-> List (substituer x y a)
	|
	F(a,b) 			-> F(substituer x y a,substituer x y b)
	|
	_ -> t
;;
(*

---------------Tests de la fonction substituer-----------

# substituer (Var 4) (Var 5) (Var 3);;
- : Type_ml.ct = Var 3

# substituer (Var 4) (Var 5) (Var 4);;
- : Type_ml.ct = Var 5

# substituer (Var 4) (Var 5) (List (Var 4));;
- : Type_ml.ct = List (Var 5)

# substituer (Var 4) (Var 5) (List (List (List (List (Var 4)))));;
- : Type_ml.ct = List (List (List (List (Var 5))))

# substituer (Var 20) (Int) (F (Var 19, F (Var 20, Int)));;
- : Type_ml.ct = F (Var 19, F (Int, Int))
*)

(*
Fonction remplacer :  Type_ml.ct -> Type_ml.ct -> (Type_ml.ct * Type_ml.ct) list -> (Type_ml.ct * Type_ml.ct) list = <fun>
-------------------------
Cette fonction permet de substituer une variable à une autre dans une liste contenant un ensemble d'equations entre des types
-------------------------
Par exemple : 
remplacer (Var 33) Int ([(F(Var 33, F(Var 22, Var 33)),Var 40);(Var 21,Int);(List(Int),List (Var 33))])  remplacera toutes les occurences de Var 33 par Int dans la liste de types. 
Cet exemple aura donc pour resultat[(F (Int, F (Var 22, Int)), Var 40); (Var 21, Int); (List Int, List Int)]
-------------------------
Parametres :
		- x : Type que l'on souhaiter remplacer par y
		- y : Type destinee a remplacer x
		- t : Type sujet de la substitution
Resultat :
		- La liste eq dans laquelle toutes les occurences de x ont ete remplacees par y
*)
let rec remplacer x y eq =
	match eq with
	[] -> []
	|
	(fst,snd)::q 	-> ((substituer x y fst, substituer x y snd)::remplacer x y q)
;;
(*

---------------Tests de la fonction remplacer-----------

# remplacer (Var 4) (Var 5) ([(Var 4,F(Var 4,Int));(Var 5,Int)]);;
- : (Type_ml.ct * Type_ml.ct) list = [(Var 5, F (Var 5, Int)); (Var 5, Int)]

- : (Type_ml.ct * Type_ml.ct) list =
[(List (Var 5), List (List (Var 5))); (Var 5, Int)]

# remplacer (Var 33) Int ([(F(Var 33, F(Var 22, Var 33)),Var 40);(Var 21,Int);(L
ist(Int),List (Var 33))]);;
- : (Type_ml.ct * Type_ml.ct) list =
[(F (Int, F (Var 22, Int)), Var 40); (Var 21, Int); (List Int, List Int)]
	
# remplacer (Var 4) (Var 5) ([(F(Var 31, (F(F(Var 4, List (List (Var 4))), Var 4
))), Var 4)]);;
- : (Type_ml.ct * Type_ml.ct) list =
[(F (Var 31, F (F (Var 5, List (List (Var 5))), Var 5)), Var 5)]
*)

(*
Fonction resoudre :  Type_ml.ct -> (Type_ml.ct * Type_ml.ct) list -> Type_ml.ct = <fun>
-------------------------
Cette fonction permet de calculer un type t en resolvant les contraintes d'un système eq
-------------------------
Parametres :
		- t : Type que l'on souhaite déterminer
		- eq : Systeme d'equation utilise pour déterminer le type t
Resultat :
		- Le type t determine a partir du systeme d'equation eq, ou une exception si le systeme ne peut etre resolu
*)
let rec resoudre t eq = 
	match eq  with
	[] -> t
	|
	(x,y)::q when x = y -> resoudre t q
	|
	(Var x,y)::q -> resoudre (substituer (Var x) y t) (remplacer (Var x) y q)    
	|
	(x,Var y)::q -> resoudre (substituer (Var y) x t) (remplacer (Var y) x q)  
	|
	(F(x,y),F(z,w))::q -> resoudre t ((x,z)::(y,w)::q)
	|
	(List x,List y)::q -> resoudre t ((x,y)::q)
	|
	_ -> raise Erreur_type
;;
(*

---------------Tests de la fonction resoudre-----------

# resoudre Int [];;
- : Type_ml.ct = Int

# resoudre (Var 4) [(Var 4,Var 4);(Var 4,Var 5)];;
- : Type_ml.ct = Var 5

# resoudre (Var 4) [(Var 4,Var 4);(Var 5,Var 4)];;
- : Type_ml.ct = Var 4

# resoudre (Var 4) [(F(Var 3, Var 4),F(Int,Bool))];;
- : Type_ml.ct = Bool

# resoudre (Var 4) [(List (Var 4),List (Var 3));(Int,Var 3)];;
- : Type_ml.ct = Int

# resoudre (Var 4) [(Int, Bool)];;
Exception: Erreur_type.

# resoudre (Var 4) [(List (Var 4),Var 5);(Var 5, List (Var 3));(Int,Var 3)];;
- : Type_ml.ct = Int

Fonction reverse : 
# resoudre (F (Var 13, Var 22)) ([(F (List (Var 17), F (List (Var 21), List (Var
 21))),   F (List (Var 17), F (List (Var 21), List (Var 21))));  (F (List (Var 1
7), F (List (Var 21), List (Var 21))),   F (Var 13, F (List (Var 24), Var 22)));
  (Bool, Bool); (List (Var 17), List (Var 21));  (List (Var 17), List (Var 20));
 (List (Var 17), List (Var 17))]);;
- : Type_ml.ct = F (List (Var 20), List (Var 20))

*)

(*
##############################################################################
##                 	      TYPAGE DE L EXPRESSION                            ##
##############################################################################
*)

(*
Fonction getFirstLeft :  (string * 'a) list -> string -> 'a = <fun>
-------------------------
Un environnement se compose d'une liste de couples. Chaque couple est compose d'un nom de variable  et d'un type  qui lui est associe.
Cette fonction recherche le premier couple contenant x dans l'environnement, et renvoie le type qui lui est associe
-------------------------
Par exemple : 
getFirstLeft [("x",Bool);("x",Int);("y",Bool)] "x" renvoie Bool
-------------------------
Parametres :
		- env : Environnement contenant les couples de valeurs
		- x : Variable dont on souhaite obtenir le type
Resultat :
		- Le type qui associe a x dans le premier couple le contenant. Si aucun couple ne contient x, la fonction lance une exception Undef x
*)
let rec getFirstLeft env x =
	match env with 
	[] -> raise (Undef x)
	|
	(hd,tl)::q when hd = x -> tl
	|
	(hd,tl)::q -> getFirstLeft q x
;;

(*
Fonction : union : 'a list -> 'a list -> 'a list = <fun>
-------------------------
Effectue une union ensembliste entre les 2 listes x et y
-------------------------
Parametres :
		-  x et y : Les 2 listes dont on souhaite effectuer l'union ensembliste
Resultat :
		- L'union ensembliste des listes x et y
*)
let rec union x y =
	let add x l =
	if (mem x l) 
	then l
	else x::l in
		match x with
		[] -> y
		|
		t::q -> union q (add t y)
;;
(*
union [1;2;3;4] [1;2;3;5];;
- : int list = [4; 1; 2; 3; 5] 
*)

(*
----------------------------
---- REGLES D INFERENCE ----
----------------------------

Afin de ne pas surcharger la fonction typer, et afin de pouvoir modifier le programme plus aisement, les regles d'inferences sont representees par des fonctions qui leurs sont propres.
Certaines regles similaires ont ete factorisees.
*)

(*
Fonction binaire : Asyn.expr ->Asyn.expr ->Type_ml.ct ->Type_ml.ct ->(string * Type_ml.ct) list ->(Type_ml.ct * Type_ml.ct) list ->Type_ml.ct * (Type_ml.ct * Type_ml.ct) list = <fun>
-------------------------
Une expression binaire est une expression du type x op y
Cette fonction represente la regle de typage des expressions binaires qu'elles soient entieres ou booleennes
-------------------------
Parametres :
		- x : Premiere operande
		- y : Seconde operande
		- type1 : type de l'expressions x op y
		- type2 : type des 2 operandes x et y
		- env : Environnement du typage
		- eq : Systeme d'equation du typage
Resultat :
		- Le type de l'expression x op y et un systeme d'equation mis a jour
*)
let rec binaire x y type1 type2 env eq = 
	let (type_x,eq_x) = typer x env eq in
	let (type_y,eq_y) = typer y env eq in
	(type1, (union (union eq_x eq_y) [(type_x,type2);(type_y,type2)]))

and

(*
Fonction unaire : Asyn.expr ->Type_ml.ct ->(string * Type_ml.ct) list ->(Type_ml.ct * Type_ml.ct) list ->Type_ml.ct * (Type_ml.ct * Type_ml.ct) list = <fun>
-------------------------
Une expression unaire est une expression du type op x
Cette fonction represente la regle de typage des expressions unaires qu'elles soient entieres ou booleennes
-------------------------
Parametres :
		- x : Premiere operande
		- type1 : type de l'expressions op x
		- env : Environnement du typage
		- eq : Systeme d'equation du typage
Resultat :
		- Le type de l'expression op x et un systeme d'equation mis a jour
*)
unaire x type1 env eq = 
	let (type_x,eq_x) = typer x env eq in
	type1, (union eq_x [(type_x,type1)])
	
and

(*
Fonction consList : Asyn.expr ->Asyn.expr ->(string * Type_ml.ct) list ->(Type_ml.ct * Type_ml.ct) list ->ype_ml.ct * (Type_ml.ct * Type_ml.ct) list= <fun>
-------------------------
Cette fonction represente la regle de typage de la concatenation d'une valeur a une liste
-------------------------
Parametres :
		- x : Valeur a concatener au debut de la liste
		- y : Liste à laquelle x est concatene
		- env : Environnement du typage
		- eq : Systeme d'equation du typage
Resultat :
		- Le type de l'expression x::y et un systeme d'equation mis a jour
*)
consList x y env eq = 
	let (type_x,eq_x) = typer x env eq in
	let (type_y,eq_y) = typer y env eq in
	(List(type_x), (union (union eq_x eq_y) [(type_y,List(type_x))]))

and

(*
Fonction egalDiff : Asyn.expr ->Asyn.expr ->(string * Type_ml.ct) list ->(Type_ml.ct * Type_ml.ct) list ->Type_ml.ct * (Type_ml.ct * Type_ml.ct) list = <fun>
-------------------------
Cette fonction represente la regle de typage de l'égalité et de la différence entre deux valeurs, c'est à dire de type x=y ou x!=y
-------------------------
Parametres :
		- x et y : Valeurs dont on souhaite verifier l'egalite ou la difference
		- env : Environnement du typage
		- eq : Systeme d'equation du typage
Resultat :
		- Le type de l'expression x=y ou x!=y et un systeme d'equation mis a jour
*)
egalDiff x y env eq =
	let (type_x,eq_x),(type_y,eq_y) = (typer x env eq),(typer y env eq) in
			match (type_x),(type_y) with
			_,Bool | Bool,_		-> (Bool, (union (union eq_x eq_y) [(type_x,Bool);(type_y,Bool)]))
			|
			Int,_ | _,Int		-> (Bool, (union (union eq_x eq_y) [(type_x,Int);(type_y,Int)]))
			|
			List(a),_ | _,List(a) -> (Bool, (union (union eq_x eq_y) [(type_x,List(a));(type_y,List(a))]))
			|
			_ -> raise Erreur_type

and

(*
Fonction ifThenElse : Asyn.expr ->Asyn.expr ->Asyn.expr ->(string * Type_ml.ct) list ->(Type_ml.ct * Type_ml.ct) list ->Type_ml.ct * (Type_ml.ct * Type_ml.ct) list= <fun>
-------------------------
Cette fonction represente la regle de typage de la structure if x then y else z
-------------------------
Parametres :
		- x : Condition du if
		- y : Resultat du then
		- z : Resultat du else
		- env : Environnement du typage
		- eq : Systeme d'equation du typage
Resultat :
		- Le type de l'expression if x then y else z et un systeme d'equation mis a jour
*)
ifThenElse x y z env eq =
	let (type_x,eq_x) = typer x env eq and
	(type_y,eq_y) = typer y env eq and
	(type_z,eq_z) = typer z env eq in
	(type_y, (union (union (union (union eq_x eq_y) eq_z) [(type_x,Bool)]) [(type_y,type_z)]))

and

(*
Fonction head : Asyn.expr ->(string * Type_ml.ct) list ->(Type_ml.ct * Type_ml.ct) list -> Type_ml.ct * (Type_ml.ct * Type_ml.ct) list= <fun>
-------------------------
Cette fonction represente la regle de typage de la structure hd list
-------------------------
Parametres :
		- x : Liste dont on souhaite obtenir la tete
		- env : Environnement du typage
		- eq : Systeme d'equation du typage
Resultat :
		- Le type de l'expression hd list et un systeme d'equation mis a jour
*)
head x env eq = 
	let (type_x,eq_x) = typer x env eq and 
	type_hd = newvar() in
	type_hd, (union eq_x [(type_x,List(type_hd))])

and

(*
Fonction tail : Asyn.expr ->(string * Type_ml.ct) list ->(Type_ml.ct * Type_ml.ct) list ->Type_ml.ct * (Type_ml.ct * Type_ml.ct) list = <fun>
-------------------------
Cette fonction represente la regle de typage de la structure tl list
-------------------------
Parametres :
		- x : Liste dont on souhaite obtenir la queue
		- env : Environnement du typage
		- eq : Systeme d'equation du typage
Resultat :
		- Le type de l'expression tl list et un systeme d'equation mis a jour
*)
tail x env eq =
	let (type_x,eq_x) = typer x env eq in
	(type_x, (union eq_x [(type_x,List(newvar()))]))

and

(*
Fonction letx : string ->Asyn.expr ->Asyn.expr ->(string * Type_ml.ct) list ->(Type_ml.ct * Type_ml.ct) list ->Type_ml.ct * (Type_ml.ct * Type_ml.ct) list= <fun>
-------------------------
Cette fonction represente la regle de typage de la structure let x = y in z
-------------------------
Parametres :
		- x : Nom de la variable
		- y : Expression
		- z : Expression dans laquelle x est substitue par y
		- env : Environnement du typage
		- eq : Systeme d'equation du typage
Resultat :
		- Le type de l'expression let x = y in z et un systeme d'equation mis a jour
*)
letx x y z env eq =
	let (type_y,eq_y) = typer y env eq in 
	let (type_z,eq_z) = typer z ((x,type_y)::env) eq in
	(type_z, (union eq_y eq_z))

and

(*
Fonction fonction : string ->Asyn.expr ->(string * Type_ml.ct) list ->(Type_ml.ct * Type_ml.ct) list ->Type_ml.ct * (Type_ml.ct * Type_ml.ct) list = <fun>
-------------------------
Cette fonction represente la regle de typage de la structure function x - > y
-------------------------
Parametres :
		- x : Nom de la variable de la fonction
		- y : Expression de la fonction
		- env : Environnement du typage
		- eq : Systeme d'equation du typage
Resultat :
		- Le type de l'expression function x - > y et un systeme d'equation mis a jour
*)
fonction x y env eq =
	let type_fonc = newvar() in
	let (type_y,eq_y) = typer y ((x,type_fonc)::env) eq in
	((F(type_fonc,type_y)),eq_y)

and 

(*
Fonction call : Asyn.expr ->Asyn.expr ->(string * Type_ml.ct) list ->(Type_ml.ct * Type_ml.ct) list -> Type_ml.ct * (Type_ml.ct * Type_ml.ct) list= <fun>
-------------------------
Cette fonction represente la regle de typage de la structure f x
-------------------------
Parametres :
		- x : Nom de la fonction a appliquer
		- y : Paramètre de la fonction
		- env : Environnement du typage
		- eq : Systeme d'equation du typage
Resultat :
		- Le type de l'expression f x et un systeme d'equation mis a jour
*)
call x y env eq = 	
	let type_call = newvar() in
	let (type_x,eq_x) = typer x env eq in 
	let (type_y,eq_y) = typer y env eq in
	(type_call, (union (union eq_x eq_y) [(type_x,F(type_y,type_call))]))
	
and

(*
Fonction letrec : string ->Asyn.expr ->Asyn.expr ->(string * Type_ml.ct) list ->(Type_ml.ct * Type_ml.ct) list ->Type_ml.ct * (Type_ml.ct * Type_ml.ct) list= <fun>
-------------------------
Cette fonction represente la regle de typage de la structure let rec x = y in z
-------------------------
Parametres :
		- x : Nom de la variable
		- y : Expression
		- z : Expression dans laquelle x est substitue par y
		- env : Environnement du typage
		- eq : Systeme d'equation du typage
Resultat :
		- Le type de l'expression let rec x = y in z et un systeme d'equation mis a jour
*)
letrec x y z env eq = 
	let type_fonc = newvar() in
	let (type_y,eq_y) = typer y ((x,type_fonc)::env) eq in 
	let (type_z,eq_z) = typer z ((x,type_fonc)::env) eq in
	(type_z,(union (union eq_y eq_z) [(type_fonc,type_y)]))
	
and

(*
Fonction typer : Asyn.expr ->(string * Type_ml.ct) list ->(Type_ml.ct * Type_ml.ct) list ->Type_ml.ct * (Type_ml.ct * Type_ml.ct) list = <fun>
-------------------------
Cette fonction permet de typer une expressions en renvoyant le type ou la variable de type qui la représente ainsi qu'un systeme d'equation permettant de calculer le type global le plus general
-------------------------
Parametres :
		- exp :  Expression que l'on souhaite typer
		- env : Environnement du typage
		- eq : Systeme d'equation du typage
Resultat :
		- Un couple contenant le type de l'expression exp ainsi qu'un système d'équation représente par une liste de couples.
*)
typer exp env eq =
   match exp with
	Entier x 	-> Int,[]
	|
	Booleen x 	-> Bool,[]
	|
	Ident x 	-> (getFirstLeft env x),[]
 	|
	Plus (x,y) | Moins (x,y) | Mult (x,y) | Div (x,y)	-> binaire x y Int Int env eq
	|
	Neg x 		-> unaire x Int env eq
	|
	Et (x,y) | Ou (x,y)	-> binaire x y Bool Bool env eq
	|
	Non x 		-> unaire x Bool env eq
	|
	Egal (x,y) | Diff (x,y)->  egalDiff x y env eq
	|
	Inf (x,y) | Infeg (x,y)| Sup (x,y)| Supeg (x,y)		-> binaire x y Bool Int env eq
	|
	Hd x 		-> head x env eq
	|
	Tl x 		-> tail x env eq
	|
	Cons (x,y)	-> consList x y env eq
	|
	Nil 		-> List(newvar()),eq
	|
	If (x,y,z) -> ifThenElse x y z env eq
	|
	Let (x,y,z)-> letx x y z env eq
	|
	Fonction (x,y) -> 	fonction x y env eq
	|
	Call (x,y) -> call x y env eq
	|
	Letrec (x,y,z) -> 	letrec x y z env eq
;;
(*

---------------Tests de la fonction typer-----------

# typer (Entier 1) [] [];;
- : Type_ml.ct * (Type_ml.ct * Type_ml.ct) list = (Int, [])

# typer (Ident "x") [("x",Bool);("x",Int)] [];;
- : Type_ml.ct * (Type_ml.ct * Type_ml.ct) list = (Bool, [])

# typer (Plus(Entier 1,Entier 2)) [] [];;
- : Type_ml.ct * (Type_ml.ct * Type_ml.ct) list =
(Int, [(Int, Int); (Int, Int)])

# typer (Neg (Entier 2)) [] [];;
- : Type_ml.ct * (Type_ml.ct * Type_ml.ct) list = (Int, [(Int, Int)])

# typer (Et (Booleen true,Booleen false)) [] [];;
- : Type_ml.ct * (Type_ml.ct * Type_ml.ct) list =
(Bool, [(Bool, Bool); (Bool, Bool)])

# typer (Non (Booleen true)) [] [];;
- : Type_ml.ct * (Type_ml.ct * Type_ml.ct) list = (Bool, [(Bool, Bool)])

# typer (Egal (Entier 1, Entier 2)) [] [];;
- : Type_ml.ct * (Type_ml.ct * Type_ml.ct) list =
(Bool, [(Int, Int); (Int, Int)])

# typer (Egal (Booleen true, Booleen true)) [] [];;
- : Type_ml.ct * (Type_ml.ct * Type_ml.ct) list =
(Bool, [(Bool, Bool); (Bool, Bool)])

# typer (Egal ((Cons (Entier 4, Nil)), (Cons (Entier 4, Nil)))) [] [];;
- : Type_ml.ct * (Type_ml.ct * Type_ml.ct) list =
(Bool,
 [(List (Var 29), List Int); (List (Var 30), List Int); (List Int, List Int);
  (List Int, List Int)])

# typer (Inf (Entier 1, Entier 2)) [] [];;
- : Type_ml.ct * (Type_ml.ct * Type_ml.ct) list =
(Bool, [(Int, Int); (Int, Int)])

# typer (Hd (Cons (Entier 1, Nil))) [] [];;
- : Type_ml.ct * (Type_ml.ct * Type_ml.ct) list =
(Var 4, [(List (Var 3), List Int); (List Int, List (Var 4))])

# typer (Tl (Cons (Entier 1, Nil))) [] [];;
- : Type_ml.ct * (Type_ml.ct * Type_ml.ct) list =
(List Int, [(List (Var 5), List Int); (List Int, List (Var 6))])

# typer (Cons (Entier 1, Nil)) [] [];;
- : Type_ml.ct * (Type_ml.ct * Type_ml.ct) list =
(List Int, [(List (Var 7), List Int)])

# typer (Nil) [] [];;
- : Type_ml.ct * (Type_ml.ct * Type_ml.ct) list = (List (Var 8), [])

# typer (If(Booleen true,Entier 1,Entier 2)) [] [];;
- : Type_ml.ct * (Type_ml.ct * Type_ml.ct) list =
(Int, [(Bool, Bool); (Int, Int)])

# typer (Let ("x", Cons (Entier 1, Cons (Entier 4, Nil)), Ident "x")) [] [];;
- : Type_ml.ct * (Type_ml.ct * Type_ml.ct) list =
(List Int, [(List Int, List Int); (List (Var 10), List Int)])

# typer (Letrec ("aux",Fonction ("l", Fonction ("res", Plus (Ident "l", Ident "res"))), Ident "aux")) [] [];;
- : Type_ml.ct * (Type_ml.ct * Type_ml.ct) list =
(Var 11,
 [(Var 12, Int); (Var 13, Int); (Var 11, F (Var 12, F (Var 13, Int)))])
 
 typer (Letrec ("fact", Fonction ("n", If (Egal (Ident "n", Entier 0), Entier 1, Mult (Ident "n", Call (Ident "fact", Moins (Ident "n", Entier 1))))), Ident "fact")) [] [];;
- : Type_ml.ct * (Type_ml.ct * Type_ml.ct) list =
(Var 14,
 [(Bool, Bool); (Var 14, F (Int, Var 16)); (Var 15, Int); (Var 16, Int);
  (Int, Int); (Var 14, F (Var 15, Int))])
*)

(*
##############################################################################
##                 	TEST DU PROGRAMME DANS SON ENSEMBLE                     ##
##############################################################################
*)

(*
?let x=1 in x;;
int

?let x=1 in 2*x+1;;
int

?let x=1 in x+y;;
Variable indéfinie: y

?let x=1 in (let y = 2 in x+y);;
int

?let rec fact = (function n ->  if n=0 then 1   else n*(fact(n-1)))  in fact;;
(int -> int)

?let rec append=(function x -> (function y -> if x=[] then y else  (hd x)::(appe
nd (tl x) y))) in append ;;
(('a list) -> (('a list) -> ('a list)))

?let rec padovan = (function n -> if (n=0) then 0 else if (n=1) then 0 else if (
n=2) then 1 else (padovan (n-2) + padovan (n-3)))  in padovan ;;
(int -> int)

?let rec fact = (function n -> if n=false then 1 else n*(fact(n-1))) in fact;;
Erreur de type

?let rev = (function l ->     let rec aux = (function l ->  (function res ->
        if l =[] then res       else aux (tl l) ((hd l)::res)   ))    in aux l [
])in rev;;
(('a list) -> ('a list))

?let rec aux = (function l ->  (function res ->  if l =[] then res
else aux (tl l) ((hd l)::res)   ))    in aux;;
(('a list) -> (('a list) -> ('a list)))

?let rec aux = (function l ->  (function res -> l + res ))    in aux;;
(int -> (int -> int))

?let rec test = (function n -> if (n) then 0 else 1) in test ;;
(bool -> int)

?let rec test = (function n -> if (n>1) then 0 else 1) in test ;;
(int -> int)

?let rec test = (function n -> if (n&&true) then 0 else 1) in test ;;
(bool -> int)

?let rec test = (function n -> if (hd n = 2) then 0 else 1) in test ;;
((int list) -> int)

?let x = (function x -> x 1) in x;;
((int -> 'a) -> 'a)

*)

(*---------------Ne pas modifier en dessous de cette ligne-----------*)

(* lit une expression du langage miniml et renvoie son type 

type_de: unit -> string;;

Exemple:
#type_de();;
let f = function x -> x
     in f;;
- : string = "('a -> 'a)"
*)

let type_de () = 
    let tau,eq = typer (read_expr()) [][]
		in string_of_type (resoudre tau eq);;


(* Lit indéfiniment une expression du langage miniml et affiche son type.
   On sort de cette fonction en frappant ^D

top: unit -> unit;;

Exemple:
#top();;
? letrec app = function x ->
                function y -> 
   if x=nil then y else (hd x)::(app (tl x) (y))
in app;;
(('a list) -> (('a list) -> ('a list)))

? function x -> x+y;;
Variable indéfinie: y

?let;;
Erreur syntaxique

?^D
Au revoir
#
*)


let rec top() =
    print_char('?');
    flush(stdout);
    try 
	begin     
	  begin
	    try  print_string(type_de()) with 
		Erreur_type -> print_string "Erreur de type\n";
	      | Undef v -> print_string "Variable indéfinie: ";
		  print_string v;
		  print_newline();
	      | ErreurSyntaxique -> print_string "Erreur syntaxique\n";
	      | Miniml_lexer.FinDeFichier->  raise Au_revoir;      
	  end;
	  print_newline();
	  top();
	end;
    with
      |  Au_revoir -> print_string "Au_revoir\n";
;;


top();;





