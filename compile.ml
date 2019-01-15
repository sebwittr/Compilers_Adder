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

(* The next four functions convert assembly instructions into strings,
   one datatype at a time.  Only one function has been fully implemented
   for you. *)
let reg_to_asm_string (r : reg) : string =
  (* COMPLETE THIS FUNCTION *)
  failwith "Not yet implemented"

let arg_to_asm_string (a : arg) : string =
  match a with
  | Const(n) -> sprintf "%d" n
  (* COMPLETE THIS FUNCTION *)
  | _ ->
     failwith "Other args not yet implemented"

let instruction_to_asm_string (i : instruction) : string =
  match i with
  | IMov(dest, value) ->
     sprintf "\tmov %s, %s" (arg_to_asm_string dest) (arg_to_asm_string value)
  (* COMPLETE THIS FUNCTION *)
  | _ ->
     failwith "Other instructions not yet implemented" 

let to_asm_string (is : instruction list) : string =
  List.fold_left (fun s i -> sprintf "%s\n%s" s (instruction_to_asm_string i)) "" is

(* A helper function for looking up a name in a "dictionary" and 
   returning the associated value if possible.  This is definitely
   not the most efficient data structure for this, but it is nice and simple... *)
let rec find (ls : (string * 'a) list) (x : string) : 'a option =
  match ls with
  | [] -> None
  | (y, v)::rest ->
     if y = x then Some(v) else find rest x

(* The exception to be thrown when some sort of problem is found with names *)
exception BindingError of string
(* The actual compilation process.  The `compile` function is the primary function,
   and uses `compile_env` as its helper.  In a more idiomatic OCaml program, this
   helper would likely be a local definition within `compile`, but separating it out
   makes it easier for you to test it. *)
let rec compile_env
          (p : pos expr)         (* the program, currently annotated with source location info *)
          (stack_index : int)    (* the next available stack index *)
          (env : (string * int) list) (* the current binding environment of names to stack slots *)
        : instruction list =     (* the instructions that would execute this program *)
  match p with
  | Number(n, _) ->
     [
       IMov(Reg(EAX), Const(n))
     ]
  | _ ->
     failwith "Other exprs not yet implemented"

let compile (p : pos expr) : instruction list =
  compile_env p 1 [] (* Start at the first stack slot, with an empty environment *)

(* The entry point for the compiler: a function that takes a expr and
   creates an assembly-program string representing the compiled version *)
let compile_to_string (prog : pos expr) : string =
  let prelude = "
section .text
global our_code_starts_here
our_code_starts_here:" in
  let asm_string = (to_asm_string ((compile prog) @ [IRet])) in
  let asm_program = sprintf "%s%s\n" prelude asm_string in
  asm_program

