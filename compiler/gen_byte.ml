open Ast


let builtins = [("print", 1)]

type opcode =
  | ImmInt of int
  | ImmFloat of float
  | Get of int
  | Set of int
  | Global of string
  | Builtin of int
  | Arg of int
  | Call of int
  | Binop of binop
  | Unop of unop
  | Trap
  | Jmp of int
  | Jt of int
  | Frame of int
  | Ret

let sizeof = function
  | ImmInt _   -> 9
  | ImmFloat _ -> 9
  | Get _      -> 5
  | Set _      -> 5
  | Global _   -> 5
  | Builtin _  -> 5
  | Arg _      -> 2
  | Call _     -> 2
  | Binop _    -> 1
  | Unop _     -> 1
  | Trap       -> 1
  | Jmp _      -> 3
  | Jt _       -> 3
  | Frame _    -> 2
  | Ret        -> 1

type inst =
  | Inst of opcode
  | Label of int

type scope =
  | Scope of string * int * scope
  | Args of (string * int) list * scope
  | Global of string list

type context =
  { mutable insts: inst list
  ; mutable scopes: scope
  ; mutable num_locals: int
  ; mutable max_locals: int
  ; mutable next_label: int
  }

let emit_inst ctx op =
  ctx.insts <- Inst(op) :: ctx.insts

let emit_label ctx label =
  ctx.insts <- Label(label) :: ctx.insts

let make_label ctx =
  let idx = ctx.next_label in
  ctx.next_label <- idx + 1;
  idx

let rec emit_expr ctx = function
  | Int(v) ->
    emit_inst ctx (ImmInt v)
  | Float(v) ->
    emit_inst ctx (ImmFloat v)
  | Ident(name) ->
    let rec lookup_ident = function
      | Args(vars, next) ->
        (match List.assoc_opt name vars with
        | Some n -> emit_inst ctx (Arg n)
        | None   -> lookup_ident next
        )
      | Scope(var, n, _) when var = name ->
        emit_inst ctx (Get n)
      | Scope(_, _, next) ->
        lookup_ident next
      | Global globals when List.mem name globals ->
        emit_inst ctx (Global name)
      | Global globals when List.mem_assoc name builtins ->
        emit_inst ctx (Builtin (List.assoc name builtins))
      | Global _ ->
        emit_inst ctx (ImmInt 0)
    in lookup_ident ctx.scopes
  | Call(callee, args) ->
    List.iter (emit_expr ctx) (List.rev args);
    emit_expr ctx callee;
    emit_inst ctx (Call (List.length args))
  | Binop(op, lhs, rhs) ->
    emit_expr ctx lhs;
    emit_expr ctx rhs;
    emit_inst ctx (Binop op)
  | Unop(op, arg) ->
    emit_expr ctx arg;
    emit_inst ctx (Unop op)

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
        | Some n -> emit_inst ctx Trap
        | None   -> assign_ident next
        )
      | Scope(var, n, _) when var = name ->
        emit_inst ctx (Set n)
      | Scope(_, _, next) ->
        assign_ident next
      | Global _ ->
        let n = ctx.num_locals in
        ctx.num_locals <- n + 1;
        ctx.max_locals <- max ctx.max_locals ctx.num_locals;
        ctx.scopes <- Scope(name, n, ctx.scopes);
        emit_inst ctx (Set n)
    in
    assign_ident ctx.scopes
  | While(cond, body) ->
    let lbl_loop_entry = make_label ctx in
    let lbl_loop_exit = make_label ctx in

    let { scopes; num_locals } = ctx in

    emit_label ctx lbl_loop_entry;
    emit_expr ctx cond;
    emit_inst ctx (Jt lbl_loop_exit);

    emit_seq ctx body;

    emit_inst ctx (Jmp lbl_loop_entry);
    emit_label ctx lbl_loop_exit;

    ctx.scopes <- scopes;
    ctx.num_locals <- num_locals

let emit_u8 c n =
  output_byte c n

let emit_u16 c n =
  emit_u8 c (n lsr 0);
  emit_u8 c (n lsr 8)

let emit_i16 c n =
  emit_u16 c (if n < 0 then 65536 + n else n)

let emit_u32 c n =
  emit_u8 c (n lsr  0);
  emit_u8 c (n lsr  8);
  emit_u8 c (n lsr 16);
  emit_u8 c (n asr 24)

let emit_i64 c n =
  emit_u32 c (n lsr  0);
  emit_u32 c (n lsr 32)

let emit_double c f =
  let n = Int64.bits_of_float f in
  emit_u32 c (Int32.to_int (Int64.to_int32 (Int64.shift_right_logical n  0)));
  emit_u32 c (Int32.to_int (Int64.to_int32 (Int64.shift_right_logical n 32)))

let emit prog c =
  let rec arg_scope n = function
    | arg :: args -> (arg, n) :: arg_scope (n + 1) args
    | [] -> []
  in
  let is_main f = f.name = "main" in
  let global = Global (List.map (fun func -> func.name) prog) in
  match List.find_opt is_main prog with
  | None ->
    failwith "missing main function"
  | Some entry ->
    let funcs = entry :: List.filter (fun f -> not (is_main f)) prog in
    let addrs = Hashtbl.create 16 in
    let _, funcs = List.fold_left
      (fun (s, funcs) func ->
        Hashtbl.add addrs func.name s;

        let ctx =
            { insts = []
            ; scopes = Args(arg_scope 0 func.args, global)
            ; num_locals = 0
            ; max_locals = 0
            ; next_label = 0
            }
        in

        (* Emit the function into a buffer. *)
        emit_seq ctx func.body;
        emit_inst ctx (ImmInt 0);
        emit_inst ctx Ret;

        (* Attach a frame setup instructions. *)
        let insts = Inst (Frame ctx.max_locals) :: List.rev ctx.insts in

        (* Find the locations of labels *)
        let labels = Hashtbl.create 16 in
        ignore (List.fold_left (fun s ins ->
          match ins with
          | Inst i ->
            s + sizeof i
          | Label n ->
            Hashtbl.add labels n s;
            s
          ) 0 insts
        );

        (* Relocate jumps. *)
        let jump_offset n s = Hashtbl.find labels n - (s + 1) in
        let size, code = List.fold_left (fun (s, cs) ins ->
          match ins with
          | Inst (Jmp n) -> (s + sizeof (Jmp n), Jmp (jump_offset n s) :: cs)
          | Inst (Jt n)  -> (s + sizeof (Jt n), Jt (jump_offset n s) :: cs)
          | Inst i       -> (s + sizeof i, i :: cs)
          | Label n      -> (s, cs)
          ) (0, []) insts
        in

        (* Return name, size and code *)
        (s + size, code :: funcs)
      ) (0, []) funcs
    in
    funcs |> List.iter (fun func ->
      func |> List.iter (function
        | ImmInt(n)   -> emit_u8 c 0x00; emit_i64 c n
        | ImmFloat(f) -> emit_u8 c 0x01; emit_double c f

        | Get(n)      -> emit_u8 c 0x10; emit_u32 c n
        | Set(n)      -> emit_u8 c 0x11; emit_u32 c n
        | Global(s)   -> emit_u8 c 0x12; emit_u32 c (Hashtbl.find addrs s)
        | Builtin(n)  -> emit_u8 c 0x13; emit_u32 c n
        | Arg(n)      -> emit_u8 c 0x14; emit_u8 c n

        | Call(n)     -> emit_u8 c 0x20; emit_u8 c n
        | Trap        -> emit_u8 c 0x21
        | Jmp(n)      -> emit_u8 c 0x22; emit_i16 c n
        | Jt(n)       -> emit_u8 c 0x23; emit_i16 c n
        | Ret         -> emit_u8 c 0x24
        | Frame(n)    -> emit_u16 c n

        | Binop Add   -> emit_u8 c 0x30
        | Binop Sub   -> emit_u8 c 0x31
        | Binop Mul   -> emit_u8 c 0x32
        | Binop Div   -> emit_u8 c 0x33
        | Binop Ne    -> emit_u8 c 0x34

        | Unop Neg    -> emit_u8 c 0x40
        )
    )
