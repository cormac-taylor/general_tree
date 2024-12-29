(* Cormac Taylor
I pledge my honor that I have abided by the Stevens Honor System. *)

(* given *)
type 'a gt = Node of 'a*('a gt) list

let t1 = Node(0, [])  (* returns true *)
let t2 = Node(5, [])  (* returns true *)
let t3 = Node(1, [t2])  (* returns true *)
let t4 = Node(1, [Node(2, []); Node(3, []); Node(4, [])]) (* returns true *)
let t5 = Node(1, [Node(2, [Node(6, [])]); Node(3, []); Node(4, [])])  (* returns false *)
let t6 = Node(1, [Node(2, []); Node(3, []); Node(4, [Node(6, [])])])  (* returns false *)
let t7 = Node(1, [Node(2, [Node(7, []); Node(2, []); Node(4, [])]); Node(3, [Node(8, [])]); Node(4, [Node(6, []); Node(1, [])])]) (* returns true *)
let t8 = Node(1, [Node(2, [Node(7, []); Node(2, [Node(1, [])]); Node(4, [])]); Node(3, [Node(8, [Node(7, [Node(5, [])])])]); Node(4, [Node(6, [Node(2, [])]); Node(1, [Node(3, [])])])])  (* returns false *)

let t : int gt =
    Node (33,
        [Node (12 ,[]);
        Node (77,
            [Node (37,
                [Node (14, [])]);
            Node (48, []);
            Node (103, [])])])

let mk_leaf : 'a -> 'a gt =
  fun n -> Node(n,[])

(* implementations *)

(* generic height function : Supports both max and min. *)
let rec general_height : (int -> int -> int) -> 'a gt -> int = 
  fun f (Node(v,l)) -> 
    match l with
    | [] -> 1
    | h::t -> f (1 + (general_height f h)) (general_height f (Node(v,t)))

(* maximum height : The height of a tree is the length
of the longest (in terms of number of nodes) path from the root to a leaf.  *)
let rec height : 'a gt -> int = 
  fun t -> general_height (max) t

(* number of nodes : The size of a general tree consists of
the number of nodes. *)
let rec size : 'a gt -> int = 
  fun (Node(v,l)) -> 
    match l with
    | [] -> 1
    | h::t -> (size h) + (size (Node(v,t)))

(* helps paths_to_leaves *)
let rec paths_to_leaves_helper : int list -> int -> 'a gt -> int list list = 
  fun res i (Node(v,l)) ->
    match l with
    | [] when i = 0 -> [res]
    | [] -> []
    | h::t -> (paths_to_leaves_helper (res @ [i]) 0 h) @ paths_to_leaves_helper res (i+1) (Node(v,t))

(* directions to all leaves : Let n be the largest number of children of any node in t. A path is a
list of numbers in the set {0, 1, . . . , n −1} such that if we follow it on the tree, it leads
to a leaf. The order in which the paths are listed is irrelevant. *) 
let paths_to_leaves : 'a gt -> int list list = 
  fun t -> paths_to_leaves_helper [] 0 t

(* minimum height : helps is_leaf_perfect *)
let rec min_height : 'a gt -> int = 
  fun t -> general_height (min) t

(* indicates if is leaf perfect : A general tree
is said to be leaf perfect if all leaves have the same depth. *)
let is_leaf_perfect : 'a gt -> bool = 
  fun t -> (height t) = (min_height t)

(*  helps pre-order *)
let rec preorder_helper : bool -> 'a gt -> 'a list = 
  fun is_leaf (Node(v,l)) -> 
    match l with
    | [] -> 
      if is_leaf then [v]
      else []
    | h::t -> 
      if is_leaf then [v] @ (preorder_helper true h) @ (preorder_helper false (Node(v,t)))
      else (preorder_helper true h) @ (preorder_helper false (Node(v,t)))

(*  returns the pre-order traversal of a general tree *)
let rec preorder : 'a gt -> 'a list = 
  fun t -> preorder_helper true t

(* helps mirror *)
let rec mirror_helper : 'a gt -> 'a gt -> 'a gt = 
  fun (Node(res_v,res_l)) (Node(v,l)) -> 
    match l with
    | [] -> (Node(res_v,res_l))
    | (Node(vl,lt))::t -> mirror_helper (Node(res_v,(mirror_helper (mk_leaf vl) (Node(vl,lt)) )::res_l)) (Node(v,t))

(* flips tree left-right *)
let rec mirror : 'a gt -> 'a gt = 
  fun (Node(v,l)) -> mirror_helper (mk_leaf v) (Node(v,l))  

(* helps map *)
let rec map_helper : ('a -> 'b) -> 'a gt -> 'b gt -> 'b gt = 
  fun f (Node(v,l)) (Node(res_v,res_l)) -> 
    match l with
    | [] -> (Node(res_v,res_l))
    | (Node(vl,lt))::t -> map_helper f (Node(v,t)) (Node(res_v,res_l @ [(map_helper f (Node(vl,lt)) (mk_leaf (f vl)))]))

(* map for gt : that produces a general tree resulting from t by mapping function f to each
data item in d. *)
let rec map : ('a -> 'b) -> 'a gt -> 'b gt = 
  fun f (Node(v,l)) -> map_helper f (Node(v,l)) (mk_leaf (f v))

(* fold for gt : that encodes the recursion scheme over general trees. *)
let rec fold : ('a -> 'b list -> 'b) -> 'a gt -> 'b = 
  fun f (Node(v,l)) -> f v ((List.map) (fold f) l)

(* given *)
let sum t = fold (fun i rs -> i + List.fold_left (fun i j -> i+j) 0 rs) t
let mem t e = fold (fun i rs -> i=e || List.exists (fun i -> i) rs) t  

(* mirror using fold *)
let rec mirror' : 'a gt -> 'a gt = 
  fun t -> fold (fun i rs -> Node(i, (List.rev rs))) t

(* the maximum number of children that a node in the tree has. *)
let rec degree : 'a gt -> int = 
  fun (Node(v,l)) -> 
    match l with
    | [] -> 0
    | h::t -> max (max (List.length l) (degree h)) (degree (Node(v,t)))
