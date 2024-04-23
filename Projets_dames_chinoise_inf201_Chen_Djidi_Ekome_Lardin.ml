(*   
         Projet Dames Chinoises - INF201


Lardin Viktor <viktor.lardin@univ-grenoble-alpes.fr>
Chen Thierry <Thierry.chen@univ-grenoble-alpes.fr>
Yanis Djidi <yanis.djidi@etu.univ-grenoble-alpes.fr>
EKOME BIYOGO Michel-Arthur*)




type dimension = int;; (*restreint aux entiers strictement positifs*)


type case = int * int * int;; (*restreint au triplet tels (i,j,k) tels que i+j+k=0*)
type vecteur = int * int * int;; (*restreint au triplet tels (i,j,k) tels que i+j+k=0*)


type couleur = Vert | Jaune | Rouge | Noir | Bleu | Marron (*Les couleurs des joueurs*)
              | Libre
              | Code of string (*une chaine restreinte a 3 caracteres*);;




type case_coloree = case * couleur;;


type configuration = case_coloree list * couleur list * dimension;; (*sans case libre*)
        
type coup = Du of case * case | Sm of case list;;


let indice_valide (x:int) (dim:dimension) : bool =
 x >= -2*dim && x<= 2*dim;;


let est_case ((i,j,k):case):bool=
 (i+j+k=0);;



let est_dans_losange ((i,j,k):case) (dim:dimension) : bool =
  -dim<=j && j<= dim && -dim <= k && k <= dim;;           


(*A MODIFIER en Q3
Choix de definir l'étoile par l'union de trois grand losange
*)
let est_dans_etoile ((i,j,k):case) (dim:dimension) : bool = 
  (est_dans_losange (i,j,k) dim) || (est_dans_losange (k,i,j) dim) || (est_dans_losange (j,k,i) dim);;
assert( est_dans_losange (0,0,0) 2  = true );;
(*Q4*)
let rec tourner_case (m:int)  ((i,j,k):case) :case =
   if m = 0 then (i,j,k)
   else tourner_case (m-1)(-k,-i,-j);;                        


assert ( tourner_case 1 (1,2,3) =(-3,-1,-2));;


(*Q5*)
let translate = fun (c:case)->fun (v :vecteur) ->
 let (i,j,k) = c in
 let (v1,v2,v3) = v in
 ((i+v1,j+v2,k+v3):case);;
(*Q6*)
let diff_case = fun (c:case) -> fun(k:case) ->
 let (c1,c2,c3) = c in
 let (k1,k2,k3) = k in
 ((c1-k1,c2-k2,c3-k3):vecteur);;
(*
1.2 Préparation des coups simples : déplacements unitaires et sauts simples  
Q7*)
let sont_cases_voisines= fun (c:case) -> fun(k:case) ->
 let (d1,d2,d3)= diff_case  k c in
 let d = (abs d1, abs d2, abs d3) in
 d = (1, 1, 0) || d = (1, 0, 1) || d= (0, 1, 1);;
 assert(sont_cases_voisines (3,1,-4)(3,2,-5)=true);;
 assert(sont_cases_voisines (4,1,-5)(1,2,-5)=false);;
 assert(sont_cases_voisines (1, 0,-1)(0,0,0)=true);;
 assert(sont_cases_voisines (1, 0, -1)(1,1,-2)=true);;
 assert(sont_cases_voisines (1, 0,-1)(2,0,-2)=true);;
 assert(sont_cases_voisines (1, 0, -1)(2,-1,-1)=true);;
 assert(sont_cases_voisines (1, 0,-1)(0,1,-1)=true);;
(*Q8*)
type case_option = None | Some of case;;
let calcul_pivot = fun (c:case) -> fun(k:case) ->
 let (c1,c2,c3) = c in
 let (k1,k2,k3) = k in
 let d = diff_case c k in
 let (d1,d2,d3) = d in
 if d1 =0 && (d2+d3) mod 2 =0 then Some(c1,(c2+k2)/2,(c3+k3)/2) else
 if d2 = 0 && (d1+d3) mod 2 = 0 then Some((c1+k1)/2,c2,(c3+k3)/2) else
 if d3 = 0 && (d1+d2) mod 2 = 0 then Some((c1+k1)/2,(c2+k2)/2,c3) else None;;


assert (calcul_pivot (0,-2,2) (0,2,-2)= Some (0,0,0));;
(*Q9*)


let vec_et_dist = fun (c:case) -> fun(k:case) ->
 let i,j,k = diff_case c k in
 let d = max (abs(i)) (max (abs(j)) (abs(k)) )  in
 if d = 0 then ((0,0,0),0 : vecteur*int)
   else (((i/d,j/d,k/d),d): vecteur*int);;


(*
  2.1 Configuration initiale et rotation du plateau
Q10*)


let tourner_list = fun (l:'a list) ->
 match l with
 |[] ->([]:'a list)
 |t::q -> (q@[t]:'a list);;


let rec der_list = fun ( l: 'a list) ->
 match l with
 |[x] -> (x:'a)
 |t::q -> (der_list q : 'a)
 [@@warning "-8"];;
(*Q11


Equation
(1) remplir-segment(0,(i,j,k)) = []
(2) remplir-segment(1,(i,j,k)) = [(i,j,k)]
(2) remplir-segment(m,(i,j,k))=[(i,j,k)] :: remplir-segment(m−1,(i,j+1,k−1))


Fonction pour remplir un segment *)
let rec remplir_segment = fun (n:int) -> fun (c: case) ->
 let (i,j,k) = c in
if n = 0 then ([]:case list) else  ((i,j,k)::(remplir_segment (n-1) (i,j+1,k-1)):case list);;
assert(remplir_segment 3 (-4,1,3)=[(-4,1,3);(-4,2,2);(-4,3,1)]);;
assert(remplir_segment 1 (0,0,0)=[(0,0,0)]);;
assert(remplir_segment 3 (-4,1,3)=[(-4,1,3);(-4,2,2);(-4,3,1)]);;
assert(remplir_segment 4 (-3,-3,6) = [(-3, -3, 6); (-3, -2, 5); (-3, -1, 4); (-3, 0, 3)]);;


(*Q12*)
let rec remplir_triangle_bas = fun(n:int) -> fun(c: case)->
 let (i,j,k) = c in
 match n with
 |0 -> ([]:case list)
 |1-> ([(i,j,k)]:case list)
 |n -> ((remplir_segment n (i,j,k))@(remplir_triangle_bas (n-1) (i-1,j+1,k)):case list);;
assert(remplir_triangle_bas 3 (-4, 1, 3) = [(-4, 1, 3); (-4, 2, 2); (-4, 3, 1); (-5, 2, 3); (-5, 3, 2); (-6, 3, 3)]);;
(*Q13*)
let rec remplir_triangle_haut = fun(n:int) -> fun(c:case) -> let(i,j,k) = c in match n with | n when n <= 0 -> ([]: case list) |_ ->(( remplir_segment n (i,j,k) @ remplir_triangle_haut (n-1) (i+1,j,k-1) ) :case list) ;;

(*Q14*)

let rec colorie = fun (coul : couleur) -> fun(l : case list) -> match l with 
|[]->([]:case_coloree list) 
|t::q -> ([t,coul]@(colorie coul q) : case_coloree list);;
assert( colorie Rouge [(-4, 1, 3); (-4, 2, 2); (-4, 3, 1); (-5, 2, 3); (-5, 3, 2); (-6, 3, 3)] =[((-4, 1, 3), Rouge); ((-4, 2, 2), Rouge); ((-4, 3, 1), Rouge);
((-5, 2, 3), Rouge); ((-5, 3, 2), Rouge); ((-6, 3, 3), Rouge)] )
(*Q15*)
let rec tourner_liste_case = fun(m:int) -> fun(l:case_coloree list) ->
  match l with
  |[] -> []
  |(case,col)::q -> [(tourner_case m case,col)] @ tourner_liste_case m q;;

let tourner_config = fun ( conf : configuration) ->
 let (case_coul,coul,dim) = conf in 
 let long = List.length coul / 6 in (tourner_liste_case long case_coul,tourner_list coul, dim : configuration);;


(*
2.2 Recherche et suppression de case dans une configuration
Q17*)
let rec associe (a:'a) (l:('a*'b) list) (defaut:'b) :'b =
  match l with
    |[]->defaut
    |(case,col)::fin -> if case = a then
                          col
                        else
                          associe a fin defaut;;

let quelle_couleur = fun (c:case)-> fun(conf:configuration) -> let (case_coul,coul,dim) =conf in
  (associe c case_coul Libre : couleur );;

(*Q18*)

let rec supprime_dans_config  (c:case) (conf:configuration) :configuration =
  match conf with
  |([],coul_list,dim) -> conf
  |((case,couleur)::q,coul_list,dim) -> let case_list,coul_list,dim =supprime_dans_config c (q,coul_list,dim) in
                                        if c = case then case_list,coul_list,dim 
                                        else [case,couleur]@ case_list,coul_list,dim ;;
type configuration = case_coloree list * couleur list * dimension;; (*sans case libre*)
let jeux1 =([((-4, 1, 3), Rouge); ((-4, 2, 2), Rouge); ((-4, 3, 1), Rouge);  ((-5, 2, 3), Rouge); ((-5, 3, 2), Rouge); ((-6, 3, 3), Rouge)],[Vert ;Jaune ;Rouge ], 3 :configuration );;
([((-4, 2, 2), Rouge); ((-4, 3, 1), Rouge); ((-5, 2, 3), Rouge);((-5, 3, 2), Rouge); ((-6, 3, 3), Rouge)], [Vert; Jaune; Rouge], 3)
(*jeux d'essais*)
assert( supprime_dans_config (-4,1,3) jeux1 = ([((-4, 2, 2), Rouge); ((-4, 3, 1), Rouge); ((-5, 2, 3), Rouge);((-5, 3, 2), Rouge); ((-6, 3, 3), Rouge)], [Vert; Jaune; Rouge], 3));;
assert( supprime_dans_config (-5,2,3) jeux1 = ([((-4, 1, 3), Rouge); ((-4, 2, 2), Rouge); ((-4, 3, 1), Rouge);((-5, 3, 2), Rouge); ((-6, 3, 3), Rouge)],[Vert ;Jaune ;Rouge ], 3));;
(*AFFICHAGE (fonctionne si les fonctions au dessus sont remplies)*)
(*transfo transforme des coordonnees cartesiennes (x,y) en coordonnees de case (i,j,k)*)
let transfo x y = (y, (x-y)/2,(-x-y)/2);;

let couleur2string (coul:couleur):string =
  match coul with
  | Libre -> " . "
  | Code s -> s  
  | Vert -> " V "
  | Jaune -> " J "
  | Rouge -> " R "
  | Noir -> " N "
  | Bleu -> " B "
  | Marron -> " M ";;

let rec affiche_ligne (n:int) (m:int) (config:configuration) : string =
  let (lcc,_,dim)=config in
    if m = (4 * dim) + 1 then " " (*fin de ligne*)
    else
      let c = transfo m n in
      if not ((n+m) mod 2 = 0) || not (est_dans_etoile c dim) then (*ceci est une inter-case (case inutile d'un damier) ou hors de l'etoile*)
        "   "^ affiche_ligne n (m + 1) config
      else (*ceci est une case ou bien en dehors du plateau*)
       (couleur2string (associe c lcc Libre)) ^ affiche_ligne n (m + 1) config;;


let affiche (config:configuration):unit =
  let (_,_,dim)=config in
    let rec affiche_aux n =
      if n = - 2 * dim - 1 then ()
      else
      begin
      print_endline (affiche_ligne n (-4*dim-1) config);
      print_endline "\n";
      affiche_aux (n - 1)
      end
    in
    affiche_aux (2*dim+1);;

let conf_1=([((0,0,0),Jaune)],[Jaune],2);;
affiche conf_1;;
let conf_reggae=([((0,-1,1),Vert);((0,0,0),Jaune);((0,1,-1),Rouge)],[Vert;Jaune;Rouge],1);;
affiche conf_reggae;;
let conf_vide=([],[],2);;
affiche conf_vide;;


(*A essayer apres avoir fait remplir_init
affiche (remplir_init [Code "Ali";Code "Bob";Code "Jim"] 3);;
*)
