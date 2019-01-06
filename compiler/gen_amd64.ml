open Ast


let builtins = ["print"]

let builtin_code =
  [ "\t.data"
  ; "Lfmt_int:"
  ; "\t.asciz \"%lld\\n\""
  ; "Lfmt_flt:"
  ; "\t.asciz \"%f\\n\""
  ; "Lfmt_ptr:"
  ; "\t.asciz \"%p\\n\""
  ; "Ltype_error:"
  ; "\t.asciz \"What are we doing here, JavaScript?\""
  ; "\t.text"
  ; "__print:"
  ; "\tpushq %rbp"
  ; "\tmovq %rsp, %rbp"
  ; "\tmovq 24(%rbp), %rax"
  ; "\tcmpq $1, %rax"
  ; "\tjle  1f"
  ; "\tleaq Lfmt_ptr(%rip), %rdi"
  ; "\tmovq 16(%rbp), %rsi"
  ; "\tmovq $0, %rax"
  ; "\tjmp 3f"
  ; "1:"
  ; "\tcmpq $0, %rax"
  ; "\tje  0f"
  ; "\tmovaps 16(%rbp), %xmm0"
  ; "\tleaq Lfmt_flt(%rip), %rdi"
  ; "\tmovq $1, %rax"
  ; "\tjmp 3f"
  ; "0:"
  ; "\tmovq 16(%rbp), %rsi"
  ; "\tleaq Lfmt_int(%rip), %rdi"
  ; "\tmovq $0, %rax"
  ; "3:"
  ; "\tcallq _printf"
  ; "\txorq %rax, %rax"
  ; "\txorq %rbx, %rbx"
  ; "\tpopq %rbp"
  ; "\tretq"
  ; ""
  ; "\t.global _main"
  ; "_main:"
  ; "\tpushq %rbp"
  ; "\tmovq %rsp, %rbp"
  ; "\tcallq main"
  ; "\tmovq $0, %rax"
  ; "\tpopq %rbp"
  ; "\tretq"
  ; "\t.global _type_error"
  ; "_type_error:"
  ; "\tpushq %rbp"
  ; "\tmovq %rsp, %rbp"
  ; "\tleaq Ltype_error(%rip), %rdi"
  ; "\tcallq _puts"
  ; "\tpopq %rbp"
  ; "\tjmp _abort"
  ]

type section = Text

type reg =
  | RAX
  | RBX
  | RDX
  | RSI
  | RDI
  | RSP
  | RBP

let string_of_reg = function
  | RAX -> "rax"
  | RBX -> "rbx"
  | RDX -> "rdx"
  | RSI -> "rsi"
  | RDI -> "rdi"
  | RSP -> "rsp"
  | RBP -> "rbp"

type arg =
  | Reg of reg
  | Ind of reg
  | Imm of int
  | Rip of string
  | Rbp of int
  | Sym of string
  | FrameSize

let rax = Reg RAX
let rbx = Reg RBX
let rdx = Reg RDX
let rsi = Reg RSI
let rdi = Reg RDI
let rsp = Reg RSP
let rbp = Reg RBP

let tag_int = Imm 0
let tag_flt = Imm 1
let tag_fn  = Imm 2

type opcode =
  | Pushq
  | Popq
  | Addq
  | Subq
  | Movq
  | Leaq
  | Callq
  | Imulq
  | Idivq
  | Xchgq
  | Cmpq
  | Je
  | Jge
  | Jle
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
  ; mutable scopes: scope
  ; mutable tmp: int
  ; mutable max_tmp: int
  ; mutable const_float: float list
  ; mutable next_label: int
  ; func_name: string
  }

let emit_inst ctx op args =
  ctx.insts <- Inst(op, args) :: ctx.insts

let emit_label ctx label =
  ctx.insts <- Label(label) :: ctx.insts

let make_label ctx =
  let idx = ctx.next_label in
  ctx.next_label <- idx + 1;
  "L" ^ ctx.func_name ^ string_of_int ctx.next_label

let rec emit_expr ctx = function
  | Int(n) ->
    emit_inst ctx Movq [tag_int; rax];
    emit_inst ctx Movq [Imm n; rbx]
  | Float(v) ->
    let n = List.length ctx.const_float in
    ctx.const_float <- v :: ctx.const_float;
    emit_inst ctx Movq [tag_flt; rax];
    emit_inst ctx Movq [Rip ("Lcst" ^ string_of_int n); rbx]
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
  | Binop(op, lhs, rhs) ->
    emit_expr ctx lhs;

    let tmp = ctx.tmp in
    let ptr_tag = -tmp * 16 - 24 in
    let ptr_val = -tmp * 16 - 16 in
    ctx.tmp <- tmp + 1;
    ctx.max_tmp <- max ctx.max_tmp ctx.tmp;

    emit_inst ctx Movq [rax; Rbp ptr_tag];
    emit_inst ctx Movq [rbx; Rbp ptr_val];

    emit_expr ctx rhs;
    ctx.tmp <- tmp;

    emit_inst ctx Movq [Rbp ptr_tag; rsi];
    emit_inst ctx Movq [Rbp ptr_val; rdi];

    let lbl_okay = make_label ctx in
    let lbl_float = make_label ctx in
    let lbl_end = make_label ctx in

    emit_inst ctx Cmpq [tag_flt; rax];
    emit_inst ctx Jle [Sym lbl_okay];
    emit_inst ctx Callq [Sym "_type_error"];
    emit_label ctx lbl_okay;

    (* At this point: tagLHS: rax; tagRHS: rsi; valLHS: rbx; valRHS: rdi *)
    (match op with
    | Add ->
      (* rbx <- rbx + rdi *)
      emit_inst ctx Addq [rdi; rbx]
    | Sub ->
      (* rbx <- rdi - rbx *)
      emit_inst ctx Subq [rbx; rdi];
      emit_inst ctx Movq [rdi; rbx]
    | Div ->
      (* rax <- rdx:rax / rbx; rbx <- rax *)
      emit_inst ctx Xchgq [rax; rdi];
      emit_inst ctx Movq [Imm 0; rdx];
      emit_inst ctx Idivq [rbx];
      emit_inst ctx Movq [rax; rbx];
      emit_inst ctx Xchgq [rax; rdi];
    | Mul ->
      (* rdx:rax <- rax * rdi; rbx <- rax *)
      emit_inst ctx Xchgq [rax; rbx];
      emit_inst ctx Imulq [rdi];
      emit_inst ctx Xchgq [rbx; rax]
    );

    emit_label ctx lbl_end
  | Unop(op, arg) ->
    failwith "unop"

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
    let ctx =
        { insts = []
        ; scopes = Scope(arg_scope 0 func.args, global)
        ; tmp = 0
        ; max_tmp = 0
        ; const_float = []
        ; next_label = 0
        ; func_name = func.name
        }
    in
    emit_inst ctx Pushq [rsp];
    emit_inst ctx Movq [rsp; rbp];
    emit_inst ctx Subq [FrameSize; rsp];
    emit_seq ctx func.body;
    emit_inst ctx Addq [FrameSize; rsp];
    emit_inst ctx Popq [rbp];
    emit_inst ctx Ret [];
    ctx.insts |> List.rev |> List.iter (function
      | Inst(op, args) ->
        Printf.fprintf c "\t%s\t"
          (match op with
          | Pushq  -> "pushq "
          | Popq   -> "popq  "
          | Addq   -> "addq  "
          | Subq   -> "subq  "
          | Movq   -> "movq  "
          | Leaq   -> "leaq  "
          | Callq  -> "callq "
          | Imulq  -> "imulq "
          | Idivq  -> "idivq "
          | Xchgq  -> "xchgq "
          | Cmpq   -> "cmpq  "
          | Je     -> "je    "
          | Jge    -> "jge   "
          | Jle    -> "jle   "
          | Ret    -> "ret   "
          );
        args
          |> List.map
            (function
            | Reg reg -> "%" ^ string_of_reg reg
            | Imm imm -> "$" ^ string_of_int imm
            | Ind reg -> "*%" ^ string_of_reg reg
            | Rip name -> name ^ "(%rip)"
            | Rbp off -> string_of_int off ^ "(%rbp)"
            | FrameSize -> "$" ^ string_of_int (ctx.max_tmp * 16)
            | Sym name -> name
            )
          |> String.concat ", "
          |> output_string c;
        Printf.fprintf c "\n";
      | Label n ->
        Printf.fprintf c "%s:\n" n
    );
    Printf.fprintf c "\n";
    if ctx.const_float <> [] then begin
      Printf.fprintf c "\t.data\n";
      ignore (List.fold_right (fun f n->
        Printf.fprintf c "Lcst%d:\n" n;
        Printf.fprintf c "\t.double\t%f\n" f;
        n + 1
      ) ctx.const_float 0);
      Printf.fprintf c "\n";
    end;
  );
  output_string c "\n";
  output_string c (String.concat "\n" builtin_code);
  output_string c "\n";
