open Ast


let builtins = ["print"]

let builtin_code =
  [ "\t.data"
  ; "Lfmt_int:"
  ; "\t.asciz \"%lld\\n\""
  ; "\t.text"
  ; "__print:"
  ; "\tpushq %rbp"
  ; "\tmovq %rsp, %rbp"
  ; "\tleaq Lfmt_int(%rip), %rdi"
  ; "\tmovq 16(%rbp), %rsi"
  ; "\txorq %rax, %rax"
  ; "\tcallq _printf"
  ; "\txorq %rax, %rax"
  ; "\txorq %rbx, %rbx"
  ; "\tpopq %rbp"
  ; "\tretq"
  ; "\n"
  ; "\t.global _main"
  ; "_main:"
  ; "\tpushq %rbp"
  ; "\tmovq %rsp, %rbp"
  ; "\tcallq main"
  ; "\tmovq $0, %rax"
  ; "\tpopq %rbp"
  ; "\tretq"
  ]

type section = Text

type reg =
  | RAX
  | RBX
  | RSP
  | RBP
  | XMM0

let string_of_reg = function
  | RAX -> "rax"
  | RBX -> "rbx"
  | RSP -> "rsp"
  | RBP -> "rbp"
  | XMM0 -> "xmm0"

type arg =
  | Reg of reg
  | Ind of reg
  | Imm of int
  | Rip of string
  | Rbp of int

let rax = Reg RAX
let rbx = Reg RBX
let rsp = Reg RSP
let rbp = Reg RBP

let tag_int = Imm 0
let tag_fn  = Imm 1

type opcode =
  | Pushq
  | Popq
  | Addq
  | Movq
  | Leaq
  | Callq
  | Ret

type loc =
  | Arg of int

type inst =
  | Inst of opcode * arg list
  | Label of string

type scope =
  | Scope of (string * loc) list * scope
  | Global of string list

type context =
  { mutable insts: inst list
  ; mutable scopes : scope
  }

let emit_inst ctx op args =
  ctx.insts <- Inst(op, args) :: ctx.insts

let rec emit_expr ctx = function
  | Int(n) ->
    emit_inst ctx Movq [tag_int; rax];
    emit_inst ctx Movq [Imm n; rbx]
  | Ident(name) ->
    let rec lookup_ident = function
      | Scope(vars, next) ->
        (match List.assoc_opt name vars with
        | Some (Arg n) ->
          emit_inst ctx Movq [Rbp (n * 16 + 8 + 16); rax];
          emit_inst ctx Movq [Rbp (n * 16 + 0 + 16); rbx]
        | None ->
          lookup_ident next
        )
      | Global globals when List.mem name globals ->
        emit_inst ctx Movq [tag_fn; rax];
        emit_inst ctx Leaq [Rip name; rbx]
      | Global globals when List.mem name builtins ->
        emit_inst ctx Movq [tag_fn; rax];
        emit_inst ctx Leaq [Rip ("__" ^ name); rbx]
      | Global _ ->
        emit_inst ctx Movq [tag_fn; rax];
        emit_inst ctx Movq [Imm 0; rbx]
    in lookup_ident ctx.scopes
  | Call(callee, args) ->
    args |> List.rev |> List.iter (fun arg ->
      emit_expr ctx arg;
      emit_inst ctx Pushq [rax];
      emit_inst ctx Pushq [rbx]
    );
    emit_expr ctx callee;
    emit_inst ctx Callq [Ind RBX];
    emit_inst ctx Addq [Imm ((List.length args) * 16); rsp]

let rec emit_seq ctx = function
  | Expr e    -> emit_expr ctx e
  | Seq(f, s) -> emit_seq ctx f; emit_seq ctx s

let emit_section c section =
  Printf.fprintf c "\t%s\n"
    (match section with
    | Text -> ".text"
    )

let emit_label c name =
  Printf.fprintf c "%s:\n" name

let emit prog c =
  let rec arg_scope n = function
    | arg :: args -> (arg, Arg n) :: arg_scope (n + 1) args
    | [] -> []
  in
  let global = Global (List.map (fun func -> func.name) prog) in
  prog |> List.iter (fun func ->
    emit_section c Text;
    emit_label c func.name;
    let ctx = { insts = []; scopes = Scope(arg_scope 0 func.args, global) } in
    emit_inst ctx Pushq [rsp];
    emit_inst ctx Movq [rsp; rbp];
    emit_seq ctx func.body;
    emit_inst ctx Popq [rbp];
    emit_inst ctx Ret [];
    ctx.insts |> List.rev |> List.iter (function
      | Inst(op, args) ->
        Printf.fprintf c "\t%s\t"
          (match op with
          | Pushq -> "pushq"
          | Popq  -> "popq "
          | Addq  -> "addq "
          | Movq  -> "movq "
          | Leaq  -> "leaq "
          | Callq -> "callq"
          | Ret   -> "ret  "
          );
        args
          |> List.map
            (function
            | Reg reg -> "%" ^ string_of_reg reg
            | Imm imm -> "$" ^ string_of_int imm
            | Ind reg -> "*%" ^ string_of_reg reg
            | Rip name -> name ^ "(%rip)"
            | Rbp off -> string_of_int off ^ "(%rbp)"
            )
          |> String.concat ", "
          |> output_string c;
        Printf.fprintf c "\n";
      | Label n ->
        Printf.fprintf c "%s:\n" n
    );
    Printf.fprintf c "\n";
  );
  output_string c (String.concat "\n" builtin_code);
  output_string c "\n";
