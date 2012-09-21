(** Translation from array-free goto Boogie programs to NTS. *)

open Prelude
open Printf
(* open Operators *)

(* open Nts_types *)
open Ntsint.Nts_int

let ident = id
(* let nondet = sprintf "__nondet_%n" *)

module T = BplAst.Type
(* module TT = NtsAst.Type *)
let rec type_ = function
	| T.Bool -> failwith
	| T.Int -> failwith
	| T.T (x,_) -> failwith
	| T.Map (_,ts,t) -> failwith
	| t -> failwith
		  << sprintf "Illegal type `%s' ."
		  <| T.to_string t

module L = BplAst.Literal
(* module LL = NtsAst.Literal *)
let lit = function
	| L.True -> failwith
	| L.False -> failwith
	| L.Num n -> failwith
	| L.Bv _ -> failwith "Illegal type BV for concurrent prorams."

module Bop = BplAst.BinaryOp
(* module BBop = BplAst.BinaryOp *)
let oper = function
	| Bop.Iff -> failwith
	| Bop.Imp -> failwith
	| Bop.Or -> failwith
	| Bop.And -> failwith
	| Bop.Eq -> failwith
	| Bop.Neq -> failwith
	| Bop.Lt -> failwith
	| Bop.Gt -> failwith
	| Bop.Lte -> failwith
	| Bop.Gte -> failwith
	| Bop.Plus -> failwith
	| Bop.Minus -> failwith
	| Bop.Times -> failwith
	| Bop.Div -> failwith
	| Bop.Mod -> failwith
	
	| op -> failwith
		<< sprintf "Illegal oper `%s' for concurrent programs."
		<| Bop.to_string op

module E = BplAst.Expression
(* module EE = NtsAst.Expression *)
let quant q = match q with
	| E.Forall -> failwith
	| E.Exists -> failwith
let rec expr e = match e with
	| E.Id i -> failwith
	| E.Lit c -> failwith
	| E.Not e -> failwith
	| E.Neg e -> failwith
	| E.Bin (op,e,f) -> failwith
	| E.Sel (e,es) -> failwith
	| E.Upd (e,es,f) -> failwith
	| E.Old e -> failwith
	| E.FnApp (f,es) -> failwith
	| E.Q (q,_,xs,_,_,e) -> failwith

let rec decider d = match d with
	| None -> failwith
	| Some e -> failwith

module Lv = BplAst.Lvalue
(* module LLv = NtsAst.Lvalue *)
let rec lval lv = match lv with
	| Lv.Id i -> failwith
	| Lv.Sel (x,es) -> failwith

module S = BplAst.Statement
module Ls = BplAst.LabeledStatement
(* module SS = NtsAst.Statement *)
(* module LLs = NtsAst.LabeledStatement *)

let rec stmt s =
	match s with
	| ls,s -> begin
		  match List.map ident ls, s with
                  | ls, S.Goto ids -> []


		  (** ToDo -- need name of return variable here. *)
                  | ls, S.Return -> []
		  | ls, S.Assign (xs,es) -> []
                  | ls, S.Assert (_,e) -> []
                  | ls, S.Assume (_,e) -> []
                  | ls, S.Call (_,p,es,lvs) -> []
		  | _, s -> failwith
				<< sprintf "Illegal statement `%s' for array-free goto Boogie programs."
				<| S.to_string s

	          end (* matching the above begin *)

module A = BplAst.Attribute
(* module AA = NtsAst.Attribute *)

let attr (a,vs) = ident a, List.map (fun v -> Right v) vs

module D = BplAst.Declaration
(* module DD = NtsAst.Declaration *)
let program _ = {
        nts_system_name = ""; 
        nts_global_vars = [];
        nts_automata = Hashtbl.create 1;
        nts_gvars_init = None;
}

