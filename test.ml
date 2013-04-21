
(*
ocamlfind ocamlc -o test -package oUnit -linkpkg -g solutions.ml test.ml
*)

open OUnit;;
open Solutions;;

let suffixes_test1 () = assert_equal [[]] (suffixes []);;
let suffixes_test2 () = assert_equal [[1]; []] (suffixes [1]);;
let suffixes_test3 () = assert_equal [[1; 2; 3; 4]; [2; 3; 4]; [3; 4]; [4]; []]
                                  (suffixes [1; 2; 3; 4]);;

module LeftistIntHeap = LeftistHeap(OrderedInt);;

let heap = LeftistIntHeap.empty;;
let heap2 = LeftistIntHeap.insert 4 heap;;
let heap3 = LeftistIntHeap.insert 2 heap2;;
let heap4 = LeftistIntHeap.insert 1 heap3;;
let heap5 = LeftistIntHeap.insert 3 heap4;;

let heap_test1 () = assert_equal true (LeftistIntHeap.isEmpty heap);;

let heap_test2 () = assert_equal 1 (LeftistIntHeap.findMin heap5);;

let solutions_suite = 
    "solutions suite">:::
        [
            "suffixes test 1">:: suffixes_test1;
            "suffixes test 2">:: suffixes_test2;
            "suffixes test 3">:: suffixes_test3;
            "heap test 1">:: heap_test1;
            "heap test 2">:: heap_test2
        ]
;;

let _ = 
    run_test_tt_main solutions_suite
;;

