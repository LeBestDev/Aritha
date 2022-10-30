open Asyntax
open Compile 
let _ =
  let nom_fichier = Sys.argv.(1) in
  let ic = open_in nom_fichier in
  let lexbuf = Lexing.from_channel ic in
try
  Parser.main Lexer.token lexbuf |> compile_code |> affiche_code nom_fichier
with 
  e -> print_endline (Printexc.to_string e)
