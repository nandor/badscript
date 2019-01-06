open Ast


let builtins = ["print"]

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
  | Jmp
  | Ret

type inst =
  | Inst of opcode * arg list
  | Label of string

type scope =
  | Scope of string * int * scope
  | Args of (string * int) list * scope
  | Global of string list

type context =
  { mutable insts: inst list
  ; mutable scopes: scope
  ; mutable num_tmp: int
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
      | Args(vars, next) ->
        (match List.assoc_opt name vars with
        | Some n ->
          emit_inst ctx Movq [Rbp (n * 16 + 8 + 16); rax];
          emit_inst ctx Movq [Rbp (n * 16 + 0 + 16); rbx]
        | None ->
          lookup_ident next
        )
      | Scope(var, n, _) when var = name ->
        emit_inst ctx Movq [Rbp (-n * 16 - 8 - 16); rax];
        emit_inst ctx Movq [Rbp (-n * 16 - 0 - 16); rbx]
      | Scope(_, _, next) ->
        lookup_ident next
      | Global globals when List.mem name globals ->
        emit_inst ctx Movq [tag_fn; rax];
        emit_inst ctx Leaq [Rip name; rbx]
      | Global globals when List.mem name builtins ->
        emit_inst ctx Movq [tag_fn; rax];
        emit_inst ctx Leaq [Rip ("__" ^ name ^ "$"); rbx]
      | Global _ ->
        emit_inst ctx Movq [tag_int; rax];
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

    let n = ctx.num_tmp in
    let ptr_tag = Rbp (-n * 16 - 8 - 16) in
    let ptr_val = Rbp (-n * 16 - 0 - 16) in
    ctx.num_tmp <- n + 1;
    ctx.max_tmp <- max ctx.max_tmp ctx.num_tmp;

    emit_inst ctx Movq [rax; ptr_tag];
    emit_inst ctx Movq [rbx; ptr_val];

    emit_expr ctx rhs;
    ctx.num_tmp <- n;

    emit_inst ctx Movq [ptr_tag; rsi];
    emit_inst ctx Movq [ptr_val; rdi];

    (* At this point: tagLHS: rax; tagRHS: rsi; valLHS: rbx; valRHS: rdi *)
    (match op with
    | Add -> emit_inst ctx Callq [Sym "__add$"]
    | Sub -> emit_inst ctx Callq [Sym "__sub$"]
    | Div -> emit_inst ctx Callq [Sym "__div$"]
    | Mul -> emit_inst ctx Callq [Sym "__mul$"]
    )
  | Unop(op, arg) ->
    failwith "unop"

let rec emit_seq ctx = function
  | Expr e ->
    emit_expr ctx e
  | Seq(f, s) ->
    emit_seq ctx f;
    emit_seq ctx s
  | Assign(name, expr) ->
    emit_expr ctx expr;
    let rec assign_ident = function
      | Args(vars, next) ->
        (match List.assoc_opt name vars with
        | Some n ->
          emit_inst ctx Callq [Sym "__arg_error$"]
        | None ->
          assign_ident next
        )
      | Scope(var, n, _) when var = name ->
        emit_inst ctx Movq [rax; Rbp (-n * 16 - 8 - 16)];
        emit_inst ctx Movq [rbx; Rbp (-n * 16 - 0 - 16)]
      | Scope(_, _, next) ->
        assign_ident next
      | Global _ ->
        let n = ctx.num_tmp in
        ctx.num_tmp <- n + 1;
        ctx.max_tmp <- max ctx.max_tmp ctx.num_tmp;
        ctx.scopes <- Scope(name, n, ctx.scopes);
        emit_inst ctx Movq [rax; Rbp (-n * 16 - 8 - 16)];
        emit_inst ctx Movq [rbx; Rbp (-n * 16 - 0 - 16)]
    in
    assign_ident ctx.scopes


let emit_section c section =
  Printf.fprintf c "\t%s\n"
    (match section with
    | Text -> ".text"
    )

let emit_label c name =
  Printf.fprintf c "%s:\n" name

let emit prog c =
  let rec arg_scope n = function
    | arg :: args -> (arg, n) :: arg_scope (n + 1) args
    | [] -> []
  in
  let global = Global (List.map (fun func -> func.name) prog) in
  prog |> List.iter (fun func ->
    if func.name = "main" then output_string c "\t.global main\n";
    emit_section c Text;
    emit_label c func.name;
    let ctx =
        { insts = []
        ; scopes = Args(arg_scope 0 func.args, global)
        ; num_tmp = 0
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

    let temp_size = ctx.max_tmp * 16 in
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
          | Jmp    -> "jmp   "
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
            | FrameSize -> "$" ^ string_of_int temp_size
            | Sym name -> name
            )
          |> String.concat ", "
          |> output_string c;
        Printf.fprintf c "\n";
      | Label label ->
        emit_label c label
    );
    Printf.fprintf c "\n";
    if ctx.const_float <> [] then begin
      Printf.fprintf c "\t.data\n";
      ignore (List.fold_right (fun f n->
        emit_label c ("Lcst" ^ string_of_int n);
        Printf.fprintf c "\t.double\t%f\n" f;
        n + 1
      ) ctx.const_float 0);
      Printf.fprintf c "\n";
    end;
  );
  output_string c "\n"
