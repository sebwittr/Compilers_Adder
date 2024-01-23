open Printf
open Sexp

(* Abstract syntax of (a small subset of) x86 assembly instructions *)

let word_size = 8

type reg =
  | RAX
  | RSP

type arg =
  | Const of int64
  | Reg of reg
  | RegOffset of int * reg (* int is # words of offset *)

type instruction =
  | IMov of arg * arg
  | IAdd of arg * arg
  | IRet

(* Concrete syntax of the Adder language:

   ‹expr›:
     | NUMBER
     | IDENTIFIER
     | ( let ( ‹bindings› ) ‹expr› )
     | ( add1 ‹expr› )
     | ( sub1 ‹expr› )
   ‹bindings›:
     | ( IDENTIFIER ‹expr› )
     | ( IDENTIFIER ‹expr› ) ‹bindings›
*)

(* Abstract syntax of the Adder language *)

type prim1 =
  | Add1
  | Sub1

type 'a expr =
  | Number of int64 * 'a
  | Id of string * 'a
  | Let of (string * 'a expr) list * 'a expr * 'a
  | Prim1 of prim1 * 'a expr * 'a

(* Function to convert from unknown s-expressions to Adder exprs
   Throws a SyntaxError message if there's a problem
*)
exception SyntaxError of string

let rec expr_of_sexp (s : pos sexp) : pos expr =
  match s with
  | Sym(x, p) -> Id(x, p)
  | Int(i, p) -> Number(i, p)
  | Bool(_, p) -> raise (SyntaxError ("Unexpected boolean literal at " ^ pos_to_string p false))
  | Nest (l, p) -> expr_of_sexps l p
and expr_of_sexps (sexps : pos sexp list) (nest_pos : pos) : pos expr =
  match sexps with
  | Sym("let", _) :: Nest(bindings, _) :: body :: [] -> Let(bindings_of_sexps bindings, expr_of_sexp body, nest_pos)
  | Sym("add1", _) :: e :: [] -> Prim1(Add1, expr_of_sexp e, nest_pos)
  | Sym("sub1", _) :: e :: [] -> Prim1(Sub1, expr_of_sexp e, nest_pos)
  | _ -> raise (SyntaxError ("Unexpected expression at " ^ pos_to_string nest_pos false))
and bindings_of_sexps (sexps : pos sexp list) : (string * pos expr) list =
  match sexps with
  | [] -> raise (SyntaxError "Unexpected empty bindings at ")
  | Nest([Sym(x, _); e], _) :: [] -> [(x, expr_of_sexp e)]
  | Nest([Sym(x, _); e], _) :: rest -> (x, expr_of_sexp e) :: bindings_of_sexps rest
  | e::_ -> raise (SyntaxError ("Unexpected expression in let at " ^ pos_to_string (sexp_info e) false))
;;

(* Functions that implement the compiler *)

(* The next four functions convert assembly instructions into strings,
   one datatype at a time.  Only one function has been fully implemented
   for you. *)
let reg_to_asm_string (r : reg) : string =
  match r with
  | RAX -> "rax"
  | RSP -> "rsp"
;;

let arg_to_asm_string (a : arg) : string =
  match a with
  | Const n -> sprintf "%Ld" n
  | Reg r -> reg_to_asm_string r
  | RegOffset (offset, reg) ->
    sprintf "[%s-%d]" (reg_to_asm_string reg) (offset * word_size)
;;

let instruction_to_asm_string (i : instruction) : string =
  match i with
  | IMov (dest, value) -> sprintf "\tmov %s, %s" (arg_to_asm_string dest) (arg_to_asm_string value)
  | IAdd (dest, value) -> sprintf "\tadd %s, %s" (arg_to_asm_string dest) (arg_to_asm_string value)
  | IRet -> "\tret"
;;

let to_asm_string (is : instruction list) : string =
  List.fold_left (fun s i -> sprintf "%s\n%s" s (instruction_to_asm_string i)) "" is
;;

(* A helper function for looking up a name in a "dictionary" and
   returning the associated value if possible.  This is definitely
   not the most efficient data structure for this, but it is nice and simple... *)
let rec find (ls : (string * 'a) list) (x : string) : 'a option =
  match ls with
  | [] -> None
  | (y, v) :: rest ->
      if y = x then
        Some v
      else
        find rest x
;;

(* The exception to be thrown when some sort of problem is found with names *)
exception BindingError of string

(* The actual compilation process.  The `compile` function is the primary function,
   and uses `compile_env` as its helper.  In a more idiomatic OCaml program, this
   helper would likely be a local definition within `compile`, but separating it out
   makes it easier for you to test it. *)
let rec compile_env
    (p : pos expr) (* the program, currently annotated with source location info *)
    (stack_index : int) (* the next available stack index *)
    (env : (string * int) list) : instruction list =
  (* the current binding environment of names to stack slots *)

  (* the instructions that would execute this program *)
  match p with
  | Number (n, _) -> [IMov (Reg RAX, Const n)]
  | Id(name, p) -> 
    (match (find env name) with
     | None -> raise (BindingError ("Unbound identifier " ^ name ^ " at " ^ pos_to_string p false))
     | Some offset -> [IMov (Reg RAX, RegOffset (offset, RSP))])
  | Let(bindings, e, _) -> compile_let bindings e stack_index env []
  | Prim1(Add1, e, _) -> (compile_env e stack_index env) @ [IAdd (Reg RAX, Const 1L)]
  | Prim1(Sub1, e, _) -> (compile_env e stack_index env) @ [IAdd (Reg RAX, Const (Int64.neg 1L))]
and compile_let (bindings : (string * pos expr) list) (body : pos expr) (stack_index : int) (env : (string * int) list) (seen : string list) : instruction list =
  match bindings with
  | [] -> compile_env body stack_index env
  | (name, e) :: rest -> if (List.mem name seen) then raise (BindingError ("Duplicate binding for " ^ name)) else
    (compile_env e stack_index env) @
    [IMov(RegOffset (stack_index, RSP), Reg RAX)] @
    compile_let rest body (stack_index + 1) ((name, (stack_index + 1)) :: env) (name :: seen)
;;

let compile (p : pos expr) : instruction list =
  compile_env p 1 [] (* Start at the first stack slot, with an empty environment *)
;;

(* The entry point for the compiler: a function that takes a expr and
   creates an assembly-program string representing the compiled version *)
let compile_to_string (prog : pos expr) : string =
  let prelude = "\nsection .text\nglobal our_code_starts_here\nour_code_starts_here:" in
  let asm_string = to_asm_string (compile prog @ [IRet]) in
  let asm_program = sprintf "%s%s\n" prelude asm_string in
  asm_program
;;
