
(*   Projet Dames Chinoises - INF201
-------------------------------------------------------------------

Lardin Viktor <viktor.lardin@univ-grenoble-alpes.fr>
Chen Thierry <Thierry.chen@univ-grenoble-alpes.fr>
Yanis Djidi <yanis.djidi@etu.univ-grenoble-alpes.fr>
Eyad 
-------------------------------------------------------------------
*)


type dimension = int;; (*restreint aux entiers strictement positifs*)


type case = int * int * int;; (*restreint au triplet tels (i,j,k) tels que i+j+k=0*)
type vecteur = int * int * int;; (*restreint au triplet tels (i,j,k) tels que i+j+k=0*)


type couleur = Vert | Jaune | Rouge | Noir | Bleu | Marron (*Les couleurs des joueurs*)
              | Libre
              | Code of string (*une chaine restreinte a 3 caracteres*);;

type case_coloree = case * couleur;;

type configuration = case_coloree list * couleur list * dimension;; (*sans case libre*)
        
type coup = Du of case * case | Sm of case list;;

type case_option = None | Some of case;;

let indice_valide (x:int) (dim:dimension) : bool =
 x >= -2*dim && x<= 2*dim;;


let est_case ((i,j,k):case):bool=
 (i+j+k=0);;
(*Variable pour test*)
(*case*)
let case_0 = (0,0,0) and case_1 = (0,2,-2) and case_2 = (2,0,0) and case_4 =  (-4,1,3) ;;
(*case list*)
let casel_1 =[((-4, 1, 3), Libre); ((-4, 2, 2), Libre); ((-4, 3, 1), Libre);((-5, 2, 3), Libre); ((-5, 3, 2), Libre); ((-6, 3, 3), Libre)]
(*configuration*)
let jeux1 = ([((-4, 1, 3), Rouge); ((-4, 2, 2), Rouge); ((-4, 3, 1), Rouge);  ((-5, 2, 3), Rouge); ((-5, 3, 2), Rouge); ((-6, 3, 3), Rouge)],[Vert ;Jaune ;Rouge ], 3 :configuration )
and jeux2 = (([],[Vert;Rouge],2):configuration)
and jeux3 = (([((3, -6, 3), Jaune); ((3, -5, 2), Jaune); ((3, -4, 1), Jaune);((2, -5, 3), Jaune); ((2, -4, 2), Jaune); ((1, -4, 3), Jaune);
	    ((3, 1, -4), Rouge); ((3, 2, -5), Rouge); ((3, 3, -6), Rouge);((2, 2, -4), Rouge); ((2, 3, -5), Rouge); ((1, 3, -4), Rouge);
	    ((-4, 1, 3), Vert); ((-4, 2, 2), Vert); ((-4, 3, 1), Vert);((-5, 2, 3), Vert); ((-5, 3, 2), Vert); ((-6, 3, 3), Vert)],[Jaune;Vert;Rouge],3):configuration)
and jeux4 = (([((3, -6, 3), Jaune); ((3, -5, 2), Jaune); ((3, -4, 1), Jaune);((2, -5, 3), Jaune); ((2, -4, 2), Jaune); ((1, -4, 3), Jaune);
	    ((3, 1, -4), Rouge); ((3, 2, -5), Rouge); ((3, 3, -6), Rouge);((2, 2, -4), Rouge); ((2, 3, -5), Rouge); ((1, 3, -4), Rouge);
            ((-4, 1, 3), Vert); ((-4, 2, 2), Vert); ((-3, 2, 1), Vert);((-5, 2, 3), Vert); ((-5, 3, 2), Vert); ((-6, 3, 3), Vert)],[Jaune;Vert;Rouge],3):configuration)
and jeux5=  (([((0, 1, -1), Vert); ((-3, 2, 1), Vert); ((-4, 1, 3), Vert);((-5, 2, 3), Vert); ((-6, 3, 3), Vert); ((-5, 3, 2), Vert)],[Vert],3):configuration)
and jeux6 = (([((-6, 3, 3), Jaune); ((-5, 2, 3), Jaune); ((-4, 2, 2), Jaune);((-4, 3, 1), Jaune); ((-2, 2, 0), Jaune); ((1, 0, -1), Jaune)],[Jaune],3):configuration) 
and jeux7 = (([((6, -3, -3), Vert); ((5, -3, -2), Vert); ((5, -2, -3), Vert);((4, -3, -1), Vert); ((4, -2, -2), Vert); ((4, -1, -3), Vert)],[Vert], 3):configuration) 
and jeux8 = (([((3, -2, -1), Vert); ((4, -2, -2), Vert); ((4, -1, -3), Vert);((5, -3, -2), Vert); ((5, -2, -3), Vert); ((6, -3, -3), Vert)],[Vert], 3):configuration) 
and jeux9 = (([((-4, 1, 3), Vert); ((-4, 2, 2), Vert); ((-4, 3, 1), Vert);((-5, 2, 3), Vert); ((-5, 3, 2), Vert); ((-6, 3, 3), Vert); ((-1, -3, 4), Jaune); ((-2, -2, 4), Jaune); ((-3, -1, 4), Jaune);((-2, -3, 5), Jaune); ((-3, -2, 5), Jaune); ((-3, -3, 6), Jaune); ((3, -4, 1), Rouge); ((2, -4, 2), Rouge); ((1, -4, 3), Rouge); ((3, -5, 2), Rouge); ((2, -5, 3), Rouge); ((3, -6, 3), Rouge);((4, -1, -3), Marron); ((4, -2, -2), Marron); ((4, -3, -1), Marron);
	    ((5, -2, -3), Marron); ((5, -3, -2), Marron); ((6, -3, -3), Marron);((1, 3, -4), Noir); ((2, 2, -4), Noir); ((3, 1, -4), Noir);((2, 3, -5), Noir); ((3, 2, -5), Noir); ((3, 3, -6), Noir);((-3, 4, -1), Bleu); ((-2, 4, -2), Bleu); ((-1, 4, -3), Bleu);((-3, 5, -2), Bleu); ((-2, 5, -3), Bleu); ((-3, 6, -3), Bleu)],
	    [Marron; Noir; Bleu; Vert; Jaune; Rouge], 3) : configuration)
and jeux10 =(([((1, 2, -3), Jaune); ((2, -3, 1), Vert); ((3, -4, 1), Vert);((1, -4, 3), Vert); ((3, -5, 2), Vert); ((2, -5, 3), Vert);((3, -6, 3), Vert); ((1, 3, -4), Jaune); ((3, 1, -4), Jaune);((2, 3, -5), Jaune); ((3, 2, -5), Jaune); ((3, 3, -6), Jaune);((-4, 1, 3), Rouge); ((-4, 2, 2), Rouge); ((-4, 3, 1), Rouge);((-5, 2, 3), Rouge); ((-5, 3, 2), Rouge); ((-6, 3, 3), Rouge)],
            [Rouge; Vert; Jaune], 3) : configuration);;
(* list coup*)
let partie2 = ( [Du((3, -2, -1), (4, -3, -1))] : coup list);;
let coup1 = Du((-4, 2, 2), (-3, 1, 2)) 
and coup2 = Sm([(-4, 2, 2); (-2, 2, 0)])
and coup3 = Du((1, 0, -1),(1, -1, 0));;

(*Q2*)
let est_dans_losange = fun((i,j,k):case) -> fun(dim:dimension) ->
  ((-dim<=j && j<= dim && -dim <= k && k <= dim ): bool );;    
                  (*test*)
assert( est_dans_losange (0,0,0) 2  = true );;
(* Q3*)
let est_dans_etoile = fun((i,j,k):case) -> fun(dim:dimension) ->
  (((est_dans_losange (i,j,k) dim) || (est_dans_losange (k,i,j) dim) || (est_dans_losange (j,k,i) dim)) : bool );;

(*Q4*)
let tourner_case = fun(m:int) ->fun(c:case) ->
	let i,j,k = c in
		match m mod 6 with
		|0 -> (i,j,k)
		|1 | -5 -> (-k,-i,-j)
		|2 | -4 -> (j,k,i)
		|3 | -3 -> (-i,-j,-k)
		|4 | -2 -> (k,i,j)
		|5 | -1 -> (-j,-k,-i)
[@@warning "-8"]  
;;  

                  (*test*)
assert ( tourner_case 1 (-6, 3, 3)= (-3, 6, -3));;

(*Q5*)
let translate = fun (c:case)-> fun (v :vecteur) ->
 let (i,j,k) = c and (v1,v2,v3) = v in
 ((i+v1,j+v2,k+v3):case);;

(*Q6*)
let diff_case = fun (c:case) -> fun (k:case) ->
 let (c1,c2,c3) = c and (k1,k2,k3) = k in
 ((c1-k1,c2-k2,c3-k3):vecteur);;

(*1.2 Préparation des coups simples : déplacements unitaires et sauts simples  
Q7*)
let sont_cases_voisines= fun (c:case) -> fun (k:case) ->
 let (d1,d2,d3)= diff_case  k c in
 let d = (abs d1, abs d2, abs d3) in
 d = (1, 1, 0) || d = (1, 0, 1) || d= (0, 1, 1);;
 
                   (*test*)
 assert(sont_cases_voisines (3,1,-4)(3,2,-5)=true
 && sont_cases_voisines (4,1,-5)(1,2,-5)=false
 && sont_cases_voisines (1, 0,-1)(0,0,0)=true
 && sont_cases_voisines (1, 0, -1)(1,1,-2)=true
 && sont_cases_voisines (1, 0,-1)(2,0,-2)=true
 && sont_cases_voisines (1, 0, -1)(2,-1,-1)=true
 && sont_cases_voisines (1, 0,-1)(0,1,-1)=true);;
(*Q8*)

let calcul_pivot = fun (c:case) -> fun(k:case) ->
 let (c1,c2,c3) = c and (k1,k2,k3) = k in
 let d = diff_case c k in
 let (d1,d2,d3) = d in
 if d1 =0 && (d2+d3) mod 2 =0 then Some(c1,(c2+k2)/2,(c3+k3)/2) else
 if d2 = 0 && (d1+d3) mod 2 = 0 then Some((c1+k1)/2,c2,(c3+k3)/2) else
 if d3 = 0 && (d1+d2) mod 2 = 0 then Some((c1+k1)/2,(c2+k2)/2,c3) else
 None;;

                  (*test*)
assert (calcul_pivot (0,-2,2) (0,2,-2) = Some (0,0,0));;
(*Q9*)

let vec_et_dist = fun (c:case) -> fun(k:case) ->
 let i,j,k = diff_case c k in
 let d = max (abs(i)) (max (abs(j)) (abs(k)) )  in
 if d = 0 then ((0,0,0),0 : vecteur*int)
   else (((i/d,j/d,k/d),d): vecteur*int);;

                  (*test*)
assert(vec_et_dist case_1 case_0 =((0,1,-1),2) 
&& vec_et_dist case_1 case_2 =((-1,1,-1),2) 
&& vec_et_dist case_2 case_0 =((1,0,0),2) 
&& vec_et_dist case_2 case_2 = ((case_0),0));;
(* 2.1 Configuration initiale et rotation du plateau
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

                  (*test*)
assert(remplir_segment 3 case_4 =[(-4,1,3);(-4,2,2);(-4,3,1)]
&& remplir_segment 1 case_0 =[(0,0,0)]
&& remplir_segment 3 case_4 =[(-4,1,3);(-4,2,2);(-4,3,1)]
&& remplir_segment 4 (-3,-3,6) = [(-3, -3, 6); (-3, -2, 5); (-3, -1, 4); (-3, 0, 3)]);;

(*Q12*)

let rec remplir_triangle_bas = fun(m:int) -> fun(c:case) ->
  let i,j,k = c in
    match m with
    |m when m<=0 -> []
    |_ -> remplir_segment m c @ remplir_triangle_bas (m-1) (i-1,j+1,k);;


                  (*test*)
assert(remplir_triangle_bas 3 (-4, 1, 3) = [(-4, 1, 3); (-4, 2, 2); (-4, 3, 1); (-5, 2, 3); (-5, 3, 2); (-6, 3, 3)]);;

(*Q13*)
let rec remplir_triangle_haut  = fun (m:int) -> fun (c:case)->
  let i,j,k = c in
  match m with
  |m when m<=0 -> []
  |_ -> remplir_segment m c @ remplir_triangle_haut (m-1) (i+1,j,k-1);;
(*Q14*)
let rec colorie = fun (coul : couleur) -> fun(l : case list) -> 
match l with 
|[]->([]:case_coloree list) 
|t::q -> ([t,coul]@(colorie coul q) : case_coloree list);;

                  (*test*)
assert( colorie Rouge [(-4, 1, 3); (-4, 2, 2); (-4, 3, 1); (-5, 2, 3); (-5, 3, 2); (-6, 3, 3)] =[((-4, 1, 3), Rouge); ((-4, 2, 2), Rouge); ((-4, 3, 1), Rouge); ((-5, 2, 3), Rouge); ((-5, 3, 2), Rouge); ((-6, 3, 3), Rouge)] );;


(*Q15*)

let rec tourner_liste_case = fun(m:int) -> fun(l:case_coloree list) ->
  match l with
  |[] -> ([] : case_coloree list)
  |(case,couleur)::fin -> (([(tourner_case m case,couleur)] @ tourner_liste_case m fin)   : case_coloree list);;
                  (*test*)
assert ( tourner_liste_case 2 casel_1 = [((1, 3, -4), Libre); ((2, 2, -4), Libre); ((3, 1, -4), Libre); ((2, 3, -5), Libre); ((3, 2, -5), Libre); ((3, 3, -6), Libre)] );;


let tourner_config = fun(conf :configuration)  ->
  let case_l,coul,dim = conf in
  let m = 6/(List.length coul) in 
    (tourner_liste_case m case_l,tourner_list coul,dim : configuration );;
		
  		(*test*)


(*Q16*)
let rec coord_case = fun (n:int) ->fun (list_joueurs:couleur list) -> fun (dim:dimension) ->
  match list_joueurs with
  |[] -> ([] : case_coloree list)
  |t::q -> (tourner_liste_case (-1*6/n) (colorie t ( remplir_triangle_bas dim (-dim-1,1,dim))@coord_case n q dim) :  case_coloree list);;

let remplir_init (ljoueurs:couleur list) (dim:dimension) : configuration =
  tourner_config (coord_case (List.length ljoueurs) ljoueurs dim,tourner_list(tourner_list(ljoueurs)),dim);;
 assert(remplir_init [Vert;Jaune;Rouge;Marron;Noir;Bleu] 3 = jeux9);;
(*
2.2 Recherche et suppression de case dans une configuration
Q17*)
let rec associe = fun(a:'a)-> fun(l:('a*'b) list) -> fun(defaut:'b) ->
  match l with
    |[]->defaut
    |(case,couleur)::fin -> if case = a then couleur 
                           else associe a fin defaut;;

let quelle_couleur = fun (c:case)-> fun(conf:configuration) ->
let (case_coul,coul,dim) = conf in
  (associe c case_coul Libre : couleur );;

(*Q18*)

let rec supprime_dans_config  (c:case) (conf:configuration) :configuration =
  match conf with
  |([],coul_list,dim) -> conf
  |((case,couleur)::q,coul_list,dim) -> let case_list,coul_list,dim =supprime_dans_config c (q,coul_list,dim) in
                                        if c = case then case_list,coul_list,dim 
                                        else [case,couleur]@ case_list,coul_list,dim ;;


(*jeux d'essais*)
assert( supprime_dans_config case_4  jeux1 = ([((-4, 2, 2), Rouge); ((-4, 3, 1), Rouge); ((-5, 2, 3), Rouge);((-5, 3, 2), Rouge); ((-6, 3, 3), Rouge)], [Vert; Jaune; Rouge], 3)
&& supprime_dans_config (-5,2,3) jeux1 = ([((-4, 1, 3), Rouge); ((-4, 2, 2), Rouge); ((-4, 3, 1), Rouge);((-5, 3, 2), Rouge); ((-6, 3, 3), Rouge)],[Vert ;Jaune ;Rouge ], 3)
&& supprime_dans_config (3,2,1) jeux2 = jeux2);;
(*
2.3 Jouer un coup unitaire
Q19

let  est_coup_valide = fun(conf :configuration) -> fun(k:coup) ->
         let (case_list,t::q(*coul_list*),dim) = conf in
 match k with 
 |Du(case_1,case_2)-> (sont_cases_voisines case_1 case_2 (*1er verif*) && (quelle_couleur case_1 (case_list,t::q,dim) = Libre ) && est_dans_losange case_2 dim :bool)
[@@warning "-8"];;

(*Q20*)
let appliquer_le_coup = fun(conf :configuration) -> fun(k:coup) ->
  match k with 
  |Du(case_1, case_2) -> let coul = quelle_couleur case_1 conf in
                         let (case_list, coul_list, dim) = supprime_dans_config case_1 conf in 
                         (((case_2, coul) :: case_list, coul_list, dim):configuration)
[@@warning "-8"];;
(*Q21*)
let mettre_a_jour_configuration = fun(conf:configuration)-> fun (k:coup)->
  match (est_coup_valide conf k : bool) with
  |true -> (appliquer_le_coup conf k :configuration)
  |false -> failwith "Ce coup n'est pas valide, le joueur doir rejouer";;

Q22*)
let case_libre  = fun(c:case) -> fun(conf:configuration) ->
  (quelle_couleur c conf = Libre : bool);; (*dit si case a la couleur libre*)

		(*test*)
assert( case_libre case_0 jeux8 = true 
	&&  case_libre case_4 jeux3 = false);;

let rec est_libre_seg = fun (case1:case) -> fun(case2:case) -> fun(conf:configuration) ->
  let (x1,y1,z1) = case1 in
  let (x2,y2,z2) = case2 in
  let (v,u,w),dist = vec_et_dist case1 case2 in 
  match (x2,y2,z2),dist with
  |_,1 -> true
  |_,_ -> case_libre (x2+v,y2+u,z2+w) conf && est_libre_seg (x1,y1,z1) (x2+v,y2+u,z2+w) (conf);;

assert(est_libre_seg (0, 0, 0) (0, 2, -2) jeux1 = true
&& est_libre_seg (0, 0, 0) (0, 1, -1) jeux1 = true
&& est_libre_seg (-4, 1, 3) (-4, 3, 1) jeux1 = false
&& est_libre_seg (-5, 2, 3) (-1, -2, 3) jeux3 = false
&& est_libre_seg (-3, 0, 3) (0, -3, 3) jeux3 = true);;
(*Q23*)
let est_saut = fun(case1:case)-> fun (case2:case) ->fun(conf:configuration) ->  
  if quelle_couleur case2 conf <> Libre  then (false:bool)
  else match  (calcul_pivot case1 case2: case_option)  with 
    | None -> false 
    | Some c -> if quelle_couleur c conf = Libre  then false
                else if est_libre_seg c case2 conf && est_libre_seg case1 c conf  then true  
                else false ;;


assert(est_saut (0, 0, 0) (0, 2, -2) ([((0, 0, 0), Jaune); ((0,1,-1), Rouge)], [Rouge; Jaune], 2) = true
	&& est_saut (-4, 3, 1) (-2, 1, 1) jeux3 = false 
	&& est_saut (-6, 3, 3) (-4, 1, 3) jeux3 = false
	&& est_saut (-2, 2, 0) (0, 0, 0) jeux3 = false
	&& est_saut(-4, 1, 3) (-4, 3, 1) jeux4 = true);;
(*Q24*)
let rec est_saut_multiple = fun(l_c : case list)-> fun(conf:configuration) ->
         match l_c with
         |[x] -> est_case x
         |[x;y] -> est_saut x y conf
         |c::k::q -> est_saut c k conf && est_saut_multiple q conf
         [@@warning "-8"];;
assert( est_saut_multiple [(-4, 1, 3); (-4, 3, 1); (-2, 1, 1)] jeux4 = true 
	&& est_saut_multiple [(-5, 2, 3); (-1, 2, -1); (1, 0, -1)] jeux5 = true 
	&& est_saut_multiple [(-5, 2, 3); (-1, 2, -1); (1, 0, -1)] jeux3 =false
	&& est_saut_multiple [(-5, 2, 3); (-3, 2, 1); (-1, 2, -1); (3, -2, -1)] jeux6 = true);; 

(*Q25*)
let  est_coup_valide = fun(conf :configuration) -> fun(k:coup) ->
         let (case_list,t::q(*coul_list*),dim) = conf in
          match k with 
          |Du(case_1,case_2)-> (est_case case_1 && est_case case_2) (*Etre sure que les case sont valide*)
                               && sont_cases_voisines case_1 case_2 (*1er verif*)
                               && quelle_couleur case_1 conf <> Libre
                               && quelle_couleur case_2 conf = Libre 
                               && est_dans_losange case_2 dim 
         |Sm(list) -> est_saut_multiple list conf 
                      && est_dans_losange (der_list list) dim
         [@@warning "-8"];;
		(*test*)
assert(est_coup_valide jeux3 coup1 = true
&& est_coup_valide jeux4 coup2 = true 
&& est_coup_valide jeux1 coup3 = false);;

let appliquer_le_coup = fun(conf :configuration) -> fun(k:coup) ->
  match k with 
  |Du(case_1, case_2) -> let coul = quelle_couleur case_1 conf in
                         let (case_list, coul_list, dim) = supprime_dans_config case_1 conf in 
                         (((case_2, coul) :: case_list, coul_list, dim):configuration)
|Sm(list) -> let case1 = List.hd list in
             let couleur = quelle_couleur case1 conf in
             let (case_list, coul_list, dim) = supprime_dans_config case1 conf in
             let fin = der_list list in
              ((fin, couleur)::case_list, coul_list, dim);;

let mettre_a_jour_configuration = fun(conf:configuration)-> fun (k:coup)->
  match (est_coup_valide conf k : bool) with
  |true ->  ((appliquer_le_coup conf k ):configuration)
  |false -> failwith "Ce coup n'est pas valide, le joueur doir rejouer";;
(*2.5*)
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

affiche (remplir_init [Code "Ali";Code "Bob";Code "Jim"] 3);;
affiche (remplir_init [Vert;Jaune;Rouge] 3);;
affiche ((remplir_init [Vert;Jaune;Rouge;Noir;Bleu; Marron] 1));;
affiche ((remplir_init [Vert;Jaune;Rouge;Noir;Bleu; Marron] 6));;

(*3 Verifier une partie
Q26*)
let est_dans_Nord= fun (c : case) ->fun(dim : dimension) ->
  let (i,j,k) = c in 
  i>dim && est_dans_etoile c dim ;;

let score = fun (conf: configuration) ->
  let (case_list, coul_list, dim) = conf in 
  let couleur_protagoniste = List.hd coul_list in
  List.fold_left (fun acc ((i, k, j), couleur) (*case_couleur*)-> if couleur = couleur_protagoniste && est_dans_Nord (i, k, j) dim then acc + i 
      else acc) 0 case_list ;; 


let rec list_val_ligne = fun(a:int) -> fun(b:int) ->
    match a with 
    |0 -> []
    |x -> [(b+1)*a](*valeur 1er ligne*)@ list_val_ligne  (a-1) (b+1) ;;(*b nb d'element dans la list*)

let score_gagnant= fun (dim : dimension) ->
  let liste_valeur = list_val_ligne  dim dim in
  List.fold_right (fun i acc -> i+acc) liste_valeur 0 (*On additionne tout les elemnt de la liste*);;
      
(*Q27*)

let gagne = fun(conf:configuration) ->
  let (case_list, coul_list, dim) = conf in 
  score conf = score_gagnant dim ;;
assert( gagne jeux7 = true && gagne jeux3 = false);;

(*Q28*)
let  rec conf_final = fun(conf:configuration) ->fun(lcoup: coup list) ->
(List.fold_left ( fun acc l -> tourner_config (mettre_a_jour_configuration  acc l) ) conf lcoup : configuration);;

assert( conf_final (remplir_init [Vert;Jaune;Rouge] 3) [Du((-4, 2, 2), (-3, 1, 2));Du((-4, 2, 2), (-3, 1, 2))] = jeux10 );;

let est_partie = fun(conf:configuration) ->fun(lcoup: coup list) ->
                  let (lcase,ljoueur,dim) = conf in
                  let (partie : configuration) = conf_final conf lcoup in
                   if gagne partie  then let (case,couleur) = (List.hd lcase) in couleur else Libre ;;

assert( est_partie jeux8 partie2 = Vert && est_partie jeux3 [Du((-4, 3, 1), (-3, 2, 1))] = Libre
);;

(*4 Calcul des coups et stratégie gloutonne 
Q29. Implémenter la fonction coup-possibles: configuration -> case ->
(case*coup) list qui étant données une case 𝑐 et une configuration, retourne des
couples (𝑐′, 𝑐𝑝) tels que 𝑐′ est accessible depuis 𝑐 en effectuant le coup 𝑐𝑝. Tous les coups
ne seront pas listés (il peut y en avoir une infinité! pourquoi?), par contre il faut que
toutes les cases 𝑐′ accessibles en un coup soient présentes exactement une fois dans la
liste.*)
let case_voisine = fun(c:case) ->
	let i,j,k = c in
 	[(i+1,j-1,k);(i,j-1,k+1);(i-1,j,k+1);(i-1,j+1,k);(i,j+1,k-1);(i+1,j,k-1)];;
  
 	
 assert(case_voisine (0,0,0) = [(1, -1, 0); (0, -1, 1); (-1, 0, 1); (-1, 1, 0); (0, 1, -1); (1, 0, -1)]);;

 let rec renvoi_coups_simples = fun(conf : configuration)  -> fun(c : case)  -> fun(l : case list) ->
  match l with 
  |pr::fin -> if est_coup_valide conf (Du(c,pr)) then ((Du(c,pr)),pr)::(renvoi_coups_simples (conf : configuration) c fin ) else renvoi_coups_simples (conf : configuration) c fin  
  [@@warning "-8"];;


let saut_valides (config: configuration) (chemin: case list)
                        (pivot: case): case list option =
            let case_depart = der_list chemin in
            let case_arrivee = translate pivot (diff_case case_depart pivot)
            in
            if (est_saut  case_depart case_arrivee config  &&
                not (List.mem case_arrivee chemin))
            then
                Some(chemin @ [case_arrivee])
            else
                None ;;
	
(*


let coups_possibles (config: configuration) (c: case): coup list =
    (* Obtenir les coups simples (déplacements unitaires) *)
    let coup_unitaires = renvoi_coups_simples conf  c (case_voisine c) in
    (* Obtenir les sauts multiples possibles *)
    let multiples = sauts_mult_possibles( supprime_dans_config(config, c), c) in
    (* Retourner la liste des coups combinés *)
    unitaires @ multiples;;

*)





