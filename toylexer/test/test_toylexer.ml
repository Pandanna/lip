open Toylexer.Token
open Toylexer.Main

let%test "test_frequencies_1" =
  lexer "x=y; x=x+1" |> frequency 3 = [(CTOK "x", 3); (ASSIGN, 2); (CTOK "y", 1)]

(* YOUR TESTS HERE *)
let%test "test_atok" =
lexer "S" |> frequency 1 = [(ATOK "S", 1)]

let%test "test_btok" =
lexer "aeiou" |> frequency 1 = [(BTOK "aeiou", 1)]

let%test "test_btok" =
lexer "aeiou" |> frequency 1 = [(BTOK "aeiou", 1)]

let%test "test_ctok" =
lexer "sass" |> frequency 1 = [(CTOK "sass", 1)]

let%test "test_dtok" =
lexer "-7" |> frequency 1 = [(DTOK "-7", 1)]

let%test "test_dtok2" =
lexer "-7." |> frequency 1 = [(DTOK "-7.", 1)]

let%test "test_dtok3" =
lexer "-7.111" |> frequency 1 = [(DTOK "-7.111", 1)]

let%test "test_dtok3" =
lexer "7.111" |> frequency 1 != [(DTOK "7.111", 1)]

let%test "test_etok" =
lexer "0x07" |> frequency 1 = [(ETOK "0x07", 1)]

let%test "test_etok2" =
lexer "0x" |> frequency 1 != [(ETOK "0x", 1)]