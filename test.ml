
open OUnit;;
open Solutions;;

let suffixes_test1 () = assert_equal [[]] (suffixes []);;
let suffixes_test2 () = assert_equal [[1]; []] (suffixes [1]);;
let suffixes_test3 () = assert_equal [[1; 2; 3; 4]; [2; 3; 4]; [3; 4]; [4]; []]
                                  (suffixes [1; 2; 3; 4]);;

let solutions_suite = 
    "solutions suite">:::
        [
            "suffixes test1">:: suffixes_test1;
            "suffixes test2">:: suffixes_test2;
            "suffixes test3">:: suffixes_test3
        ]
;;

let _ = 
    run_test_tt_main solutions_suite
;;

