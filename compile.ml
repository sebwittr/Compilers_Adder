open Printf
open Sexp

(* Abstract syntax of (a small subset of) x86 assembly instructions *)

type reg =
  | EAX
  | ESP

type arg =
  | Const of int
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
  | Number of int * 'a
  | Id of string * 'a
  | Let of (string * 'a expr) list * 'a expr * 'a
  | Prim1 of prim1 * 'a expr * 'a

(* Function to convert from unknown s-expressions to Adder exprs
   Throws a SyntaxError message if there's a problem
 *)
exception SyntaxError of string
let expr_of_sexp (s : pos sexp) : pos expr =
  (* COMPLETE THIS FUNCTION *)
  failwith (sprintf "Converting sexp not yet implemented at pos %s"
                    (pos_to_string (sexp_info s) true))

(* Functions that implement the compiler *)

let reg_to_asm_string (r : reg) : string =
  (* COMPLETE THIS FUNCTION *)
  failwith "Not yet implemented"

let arg_to_asm_string (a : arg) : string =
  match a with
  | Const(n) -> sprintf "%d" n
  | _ ->
     failwith "Other asms not yet implemented"

let instruction_to_asm_string (i : instruction) : string =
  match i with
  | IMov(dest, value) ->
     sprintf "\tmov %s, %s" (arg_to_asm_string dest) (arg_to_asm_string value)
  | _ ->
     failwith "Other instructions not yet implemented" 

let to_asm_string (is : instruction list) : string =
  List.fold_left (fun s i -> sprintf "%s\n%s" s (instruction_to_asm_string i)) "" is

let rec find (ls : (string * int) list) (x : string) =
  match ls with
  | [] -> None
  | (y, v)::rest ->
     if y = x then Some(v) else find rest x

(* The exception to be thrown when some sort of problem is found with names *)
exception BindingError of string
(* The actual compilation process *)
let rec compile_env
          (p : pos expr)
          (stack_index : int)
          (env : (string * int) list)
        : instruction list =
  match p with
  | Number(n, _) ->
     [
       IMov(Reg(EAX), Const(n))
     ]
  | _ ->
     failwith "Other exprs not yet implemented"

let compile (p : pos expr) : instruction list =
  compile_env p 1 []

(* The entry point for the compiler: a function that takes a expr and
creates an assembly-program string representing the compiled version *)

let compile_to_string (prog : pos expr) =
  let prelude = "
section .text
global our_code_starts_here
our_code_starts_here:" in
  let asm_string = (to_asm_string ((compile prog) @ [IRet])) in
  let asm_program = sprintf "%s%s\n" prelude asm_string in
  asm_program

