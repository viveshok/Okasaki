
(* 2.1 *)
let suffixes lst = 
    let rec aux acc lst =
        match lst with
        | [] -> List.rev ([]::acc)
        | x::xs -> aux (lst::acc) xs
    in aux [] lst
;;

(* 2.2 *)
module type ORDERED =
    sig
        type t

        val eq: t * t -> bool
        val lt: t * t -> bool
        val leq: t * t -> bool
    end;;

module UnbalancedSet = 
    functor (Element: ORDERED) ->
        struct
            type elem = Element.t
            type tree = E | T of tree * elem * tree
            type set = tree

            let empty = E

            let rec member x atree =
                match atree with
                | E -> false
                | T (l, y, r) -> 
                        if Element.lt (x,y)
                        then member x l
                        else if Element.lt (y,x)
                        then member x r
                        else true
            ;;

            let rec insert x atree =
                match atree with
                | E -> T (E, x, E)
                | T (l, y, r) as s -> 
                        if Element.lt (x,y)
                        then insert x l
                        else if Element.lt (y,x)
                        then insert x r
                        else s
            ;;

        end

