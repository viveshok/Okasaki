
(*
ocamlfind ocamlc -o test -package oUnit -linkpkg -g solutions.ml test.ml
*)

open OUnit;;
open Solutions;;

let suffixes_test1 () = assert_equal [[]] (suffixes []);;
let suffixes_test2 () = assert_equal [[1]; []] (suffixes [1]);;
let suffixes_test3 () = assert_equal [[1; 2; 3; 4]; [2; 3; 4]; [3; 4]; [4]; []]
                                  (suffixes [1; 2; 3; 4]);;

let (left1, right1) = split [1; 2; 3; 4; 5];;
let (left2, right2) = split [1; 2; 3; 4];;

let split_test1 () = assert_equal left1 [1; 2; 3];;
let split_test2 () = assert_equal right1 [4; 5];;

let split_test3 () = assert_equal left2 [1; 2]
let split_test4 () = assert_equal right2 [3; 4]

module LeftistIntHeap = LeftistHeap(OrderedInt);;

let heap = LeftistIntHeap.empty;;
let heap2 = LeftistIntHeap.insert 4 heap;;
let heap3 = LeftistIntHeap.insert 2 heap2;;
let heap4 = LeftistIntHeap.insert 1 heap3;;
let heap5 = LeftistIntHeap.insert 3 heap4;;

let heap6 = LeftistIntHeap.fromList [8; 3; 4; (-1); 9];;

let heap7 = LeftistIntHeap.deleteMin heap6;;

let heap_test1 () = assert_equal true (LeftistIntHeap.isEmpty heap);;

let heap_test2 () = assert_equal 1 (LeftistIntHeap.findMin heap5);;

let heap_test3 () = assert_equal false (LeftistIntHeap.isEmpty heap6);;

let heap_test4 () = assert_equal (-1) (LeftistIntHeap.findMin heap6);;

let heap_test5 () = assert_equal 3 (LeftistIntHeap.findMin heap7);;

module WBIntHeap = WeightBiasedLeftistHeap(OrderedInt);;

let wb_heap = WBIntHeap.empty;;
let wb_heap2 = WBIntHeap.insert 4 wb_heap;;
let wb_heap3 = WBIntHeap.insert 2 wb_heap2;;
let wb_heap4 = WBIntHeap.insert 1 wb_heap3;;
let wb_heap5 = WBIntHeap.insert 3 wb_heap4;;

let wb_heap6 = WBIntHeap.deleteMin wb_heap5;;

let wb_heap_test1 () = assert_equal true (WBIntHeap.isEmpty wb_heap);;

let wb_heap_test2 () = assert_equal 1 (WBIntHeap.findMin wb_heap5);;

let wb_heap_test3 () = assert_equal 2 (WBIntHeap.findMin wb_heap6);;

let solutions_suite = 
    "solutions suite">:::
        [
            "suffixes test 1">:: suffixes_test1;
            "suffixes test 2">:: suffixes_test2;
            "suffixes test 3">:: suffixes_test3;
            "split test 1">:: split_test1;
            "split test 2">:: split_test2;
            "split test 3">:: split_test3;
            "split test 4">:: split_test4;
            "heap test 1">:: heap_test1;
            "heap test 2">:: heap_test2;
            "heap test 3">:: heap_test3;
            "heap test 4">:: heap_test4;
            "heap test 5">:: heap_test5;
            "weight biased heap test 1">:: wb_heap_test1;
            "weight biased heap test 2">:: wb_heap_test2;
            "weight biased heap test 3">:: wb_heap_test3

        ]
;;

let _ = 
    run_test_tt_main solutions_suite
;;

