open Toyparser.Main

let%test "test_eval_1" = parse "1 + 2 + 3 + (1 + 2)" |> eval = Ok 9


(* YOUR TESTS HERE *)

let%test "test_eval_2" = parse "1 + 3 + 3 + (1 + 2)" |> eval = Ok 10

let%test "test_eval_3" = parse "1" |> eval = Ok 1

let%test "test_eval_4" = parse "1 + 0" |> eval = Ok 1

let%test "test_eval_5" = parse "0" |> eval = Ok 0

let%test "test_eval_6" = parse "1 + (3 - 1) / 2" |> eval = Ok 2

let%test "test_eval_7" = parse "5 - 10 / 5"|> eval = Ok 3

let%test "test_eval_8" = parse "5 * 2 / 5"|> eval = Ok 2

let%test "test_eval_9" = parse " 1 / 0" |> eval = Error(Printf.sprintf("Error: tried to divide 1 by zero"))

let%test "test_eval_10" = parse "-1 - 2 - -3"|> eval = Ok 0

let%test "test_eval_11" = parse "-1 - -2 "|> eval = Ok 1

let%test "test_eval_12" = parse "-1 * -2 "|> eval = Ok 2

let%test "test_eval_13" = parse "-2 / -2 "|> eval = Ok 1

let%test "test_eval_14" = parse "-2 / -2  * -1"|> eval = Ok (-1)

let%test "test_eval_15" = parse "0x01 + 2"|> eval = Ok 3

let%test "test_eval_16" = parse "(0x0D + 3) * 2"|> eval = Ok 32

let%test "test_eval_17" = parse "(0x0F * 2) / 0x00" |> eval = Error(Printf.sprintf("Error: tried to divide 30 by zero"))
