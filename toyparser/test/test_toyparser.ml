open Toyparser.Main

let%test "test_eval_1" = parse "1 + 2 + 3 + (1 + 2)" |> eval = Ok 9


(* YOUR TESTS HERE *)

let%test "test_eval_2" = parse "1 + 3 + 3 + (1 + 2)" |> eval = Ok 10

let%test "test_eval_3" = parse "1" |> eval = Ok 1

let%test "test_eval_4" = parse "1 + 0" |> eval = Ok 1

let%test "test_eval_5" = parse "0" |> eval = Ok 0

