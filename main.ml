open Asyntax
open Compile 
let _ = 
  let ic = open_in "expre.exp" in
  let lexbuf = Lexing.from_channel ic in
try
  Parser.main Lexer.token lexbuf |> compile_code |> affiche_code
with 
  e -> print_endline (Printexc.to_string e)
