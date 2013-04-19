
(* Some notes on syntax from SML to OCaml:
    * 'signature' becomes 'module type'
    * 'structure' becomes 'module'
    *)

(* CHAPTER 2 *)

exception AlreadyThere
exception NotFound

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

        val findMin: t -> Element.t option
        val deleteMin: t -> t option
    end;;

