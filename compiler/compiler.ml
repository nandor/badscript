
let parse path =
  (* Parse a file into an AST. *)
  let input = open_in path in
  let filebuf = Lexing.from_channel input in
  let fail err =
    let open Lexing in
    let st = lexeme_start_p filebuf in
    let n = st.pos_cnum - st.pos_bol + 1 in
    Printf.eprintf "%s:%d,%d: %s\n" path st.pos_lnum n err;
    exit 1
  in
  let ast =
    try Parser.bs_program Lexer.token filebuf
    with
    | Lexer.Error err -> fail err
    | Parser.Error    -> fail "syntax error"
  in
  close_in input;
  ast

let () =
  match Sys.argv with
  | [| _; "-target"; target; "-o"; output; input |] ->
    (match target with
    | "amd64" ->
      let chan = open_out output in
      Gen_amd64.emit (parse input) chan;
      close_out chan
    | "byte" ->
      let chan = open_out_bin output in
      Gen_byte.emit (parse input) chan;
      close_out chan
    | _ ->
      Printf.eprintf "Invalid target: %s\n" target;
      exit 1
    )
  | _ ->
    Printf.eprintf "Usage: %s -target [target] -o [out] [in]\n" (Sys.argv.(0));
    exit 1
