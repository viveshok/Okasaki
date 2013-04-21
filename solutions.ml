
(* Some notes on syntax from SML to OCaml:
    * signature -> module type
    * structure -> module
    *)

exception AlreadyThere
exception NotFound
exception Empty
exception SmtgWrong

(* Some helper functions *)

let split lst = 
    (* Split a list in two mergesort style, O(N) *)
    let rec aux lst left right =
        match lst, left, right with
        | [], left, right -> (List.rev(right), left)
        | [_], _::left_tl, right -> (List.rev(right), left_tl)
        | _::_::tl, _::left_tl, _::right_tl -> aux tl left_tl right_tl
        | _, _, _ -> raise SmtgWrong
    in aux lst lst (List.rev(lst))
;;


(* CHAPTER 2 *)

(* 2.1 *)
let suffixes lst = 
    let rec aux acc lst =
        match lst with
        | [] -> List.rev ([]::acc)
        | x::xs -> aux (lst::acc) xs
    in aux [] lst
;;

module type ORDERED =
    sig
        type t

        val eq: t -> t -> bool
        val lt: t -> t -> bool
        val leq: t -> t -> bool
    end;;

module OrderedInt =
    struct
        type t = int

        let eq x y = x=y
        let lt x y = x<y
        let leq x y = x<=y
    end;;

module OrderedString =
    struct
        type t = string

        let eq x y = x=y
        let lt x y = x<y
        let leq x y = x<=y
    end;;

module UnbalancedSet = 
    functor (Element: ORDERED) ->
        struct
            type elem = Element.t
            type tree = E | T of tree * elem * tree
            type set = tree

            let empty = E

            (* Older member function, worst case 2d *)
            (*
            let rec member x atree =
                match atree with
                | E -> false
                | T (l, y, r) -> 
                        if Element.lt x y
                        then member x l
                        else if Element.lt y x
                             then member x r
                             else true
            ;;
            *)

            (* 2.2 *)
            (* New member function, worst case d + 1 *)
            let rec member x atree =
                match atree with
                | E -> false
                | T (l, y, r) ->
                        if Element.lt x y
                        then member x l
                        else member x r || x = y
            ;;

            (* Old insert, copied path even for existing elements *)
            (*
            let rec insert x atree =
                match atree with
                | E -> T (E, x, E)
                | T (l, y, r) as s -> 
                        if Element.lt x y
                        then T (insert x l, y, r)
                        else if Element.lt y x
                        then T (l, y, insert x r)
                        else s
            ;;
            *)

            (* 2.3 *)
            (* New insert, no copied path even for existing elements, O(2d) *)
            (*
            let insert x atree =
                try
                    let rec aux x atree =
                        match atree with
                        | E -> T (E, x, E)
                        | T (l, y, r) ->
                                if Element.lt x y
                                then T (aux x l, y, r)
                                else if Element.lt y x
                                then T (l, y, aux x r)
                                else raise AlreadyThere
                    in aux x atree
                with
                    AlreadyThere -> atree
            ;;
            *)

            (* 2.4 *)
            (* New insert, no copied path even for existing elements O(d+1) *)
            let insert x atree =
                try
                    let rec aux x atree =
                        match atree with
                        | E -> T (E, x, E)
                        | T (l, y, E) ->
                                if Element.lt x y
                                then T (aux x l, y, E)
                                else if Element.lt y x
                                     then T (l, y, T (E, x, E))
                                     else raise AlreadyThere
                        | T (l, y, r) ->
                                if Element.lt x y
                                then T (aux x l, y, r)
                                else T (l, y, aux x r)
                    in aux x atree
                with
                    AlreadyThere -> atree
            ;;


        end

module IntSet = UnbalancedSet(OrderedInt);;

(* 2.5 *)
type tree = E | T of tree * int * tree

(* 2.5 (a) *)
let complete x n =
    let rec aux acc = function
        | 0 -> acc
        | n -> aux (T (acc, x, acc)) (n-1)
        in aux E n
;;

(* 2.5 (b) *)
let balancedtree n =
    let rec aux min max =
        match min with
        | min when min>max -> E
        | min when min=max -> T (E, min, E)
        | min -> 
            let middle = (max+min)/2 in
            T ((aux min (middle-1)), middle, (aux (middle+1) max))
    in aux 1 n
;;

(* 2.6 *)
module FiniteMap = 
    functor (Key: ORDERED) ->
        struct
            type key = Key.t
            type 'a elem = key * 'a
            type 'a tree = E | T of 'a tree * 'a elem * 'a tree
            type 'a map = 'a tree

            let empty = E

            let rec bind key value map =
                match map with
                | E -> T (E, (key, value), E)
                | T (l, ((somekey, _) as e), r) when Key.lt key somekey -> T ((bind key value l), e, r)
                | T (l, ((somekey, _) as e), r) when Key.lt somekey key -> T (l, e, (bind key value r))
                | T (l, _, r) -> T (l, (key, value), r)
            ;;

            let rec lookup key map =
                match map with
                | E -> raise NotFound 
                | T (l, (somekey, _), r) when Key.lt key somekey -> lookup key l
                | T (l, (somekey, _), r) when Key.lt somekey key -> lookup key r
                | T (l, (key, value), r) -> value
            ;;

        end

module StringMap = FiniteMap(OrderedString);;

(* CHAPTER 3 *)

(* 3.1 *)
(* By definition, any right child node has a left sibling. Therefore
 * all right subtree will have a size smaller or equal to its
 * sibling left subtree.
 *
 * By induction, the size of a subtree whose root node has rank M is
 * greater or equal to:
     *
     * 1 if M = 0
     * 2 x size of a subtree whose root node has rank M-1
 *
 * the recursive function above can be expressed has
 * 
 *                                                M 
 *                                               ___
 * size of subtree whose root node has rank M >= | | 2 = 2^M
 *                                               i=1
 *
 * reversing this equation around we get:
 * 
 * rank of root node of tree of size N <= log(N)
 *
 *)

(* 3.2 *)

module type HEAP =
    sig
        module Element: ORDERED

        type t

        val empty: t
        val isEmpty: t -> bool

        val insert: Element.t -> t -> t
        val merge: t -> t -> t

        val findMin: t -> Element.t
        val deleteMin: t -> t
    end;;

module LeftistHeap =
    functor (Element: ORDERED) ->
        struct
            module Elem = Element

            type t = E | T of int * Elem.t * t * t

            let empty = E

            let isEmpty = function
                | E -> true
                | _ -> false
            ;;

            let rec rank = function
                | E -> 0
                | T(_, _, _, right_node) -> 1 + rank right_node
            ;;

            let makeT elem a b =
                if rank a >= rank b then T(rank b + 1, elem, a, b)
                else T(rank a + 1, elem, b, a)
            ;;

            let rec merge a b =
                match a, b with
                | a, E -> a
                | E, b -> b
                | T(_, x, a1, b1), T(_, y, a2, b2) ->
                        if Elem.leq x y then makeT x a1 (merge b1 b)
                        else makeT y a2 (merge a b2)
            ;;

            (*
            let insert x a =
                merge (T(1, x, E,E)) a
            ;;
            *)

            (* Exercise 3.2 *)
            let rec insert x heap =
                match heap with
                | E -> T(1, x, E, E)
                | T(_, y, _, _) when Elem.lt x y -> makeT x heap E   
                | T(_, y, a, b) -> makeT y a (insert x b)   
            ;;

            let findMin = function
                | E -> raise Empty
                | T(_, x, _, _) -> x
            ;;

            let deleteMin = function
                | E -> raise Empty
                | T(_, _, a, b) -> merge a b
            ;;

            (* 3.3 *)
            let fromList lst =
                let nodeLst = List.map (fun a -> T(1, a, E, E)) lst in
                let rec aux lst =
                    match split lst with
                    | [a], [] -> a
                    | [], [b] -> b
                    | [T(_, _, E, E) as a], [T(_, _, E, E) as b] -> merge a b
                    | lstA, lstB -> merge (aux lstA) (aux lstB)
                in aux nodeLst
            ;;

        end

(* 3.4 (a) *)
(*
 * By construction any right subtree of a weight-based leftist heap 
 * has size at most half the size of its parent. Therefore a weight-
 * biased leftist heap of size N can have at most log(N) nested right
 * subtree. Hence a right spine can have at most rank log(N) + 1.
 * *)

(* 3.4 (b) *)
module WeightBiasedLeftistHeap =
    functor (Element: ORDERED) ->
        struct
            module Elem = Element

            type t = E | T of int * Elem.t * t * t

            let empty = E

            let isEmpty = function
                | E -> true
                | _ -> false
            ;;

            let rec size = function
                | E -> 0
                | T(_, _, _, right_node) -> 1 + size right_node
            ;;

            let makeT elem a b =
                if size a >= size b then T(size a + 1, elem, a, b)
                else T(size b + 1, elem, b, a)
            ;;

            (* 3.4 (c) *)
            let rec merge a b =
                match a, b with
                | a, E -> a
                | E, b -> b
                | T(s1, x, a1, b1), T(s2, y, a2, b2) when Elem.leq x y ->
                        T(s1+s2, x, (merge b a1), b1)
                | T(s1, x, a1, b1), T(s2, y, a2, b2) ->
                        T(s1+s2, y, (merge a a2), b2)
            ;;

            let rec insert x heap =
                match heap with
                | E -> T(1, x, E, E)
                | T(_, y, _, _) when Elem.lt x y -> makeT x heap E   
                | T(_, y, a, b) -> makeT y a (insert x b)   
            ;;

            let findMin = function
                | E -> raise Empty
                | T(_, x, _, _) -> x
            ;;

            let deleteMin = function
                | E -> raise Empty
                | T(_, _, a, b) -> merge a b
            ;;

        end

(* 3.4 (d) *)
(* In a lazy environment, the heaps would be merged if and only if the minimum
 * of the resulting heap is queried, In a concurrent environment, sub-heaps
 * could be easily merged in different concurrency contexts without sharing
 * much memory.
 * *)

