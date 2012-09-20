(** Translation from array-free goto Boogie programs to NTS. *)

open Prelude
open Printf
open Operators

(* include nts modules 
open Nts_types
open NtsInt.Nts_int
*)

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
	| e -> failwith
		  << sprintf "Illegal expression `%s' for concurrent programs."
		  <| E.to_string e

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

		  | ls, S.Assign (xs,es)
				when List.for_all (function E.Choice -> true | _ -> false) es
					&& List.for_all (function Lv.Id _ -> true | _ -> false) xs
                                        -> []
                  | ls, S.Assign (xs,es) -> []
                  | ls, S.Assert (_,e) -> []
                  | ls, S.Assume (_,e) -> []
                  | ls, S.Call (_,p,es,lvs) -> []
		  | _, s -> failwith
				<< sprintf "Illegal statement `%s' for array-free goto Boogie programs."
				<| S.to_string s

	  end

module A = BplAst.Attribute
(* module AA = NtsAst.Attribute *)

let attr (a,vs) = ident a, List.map (fun v -> Right v) vs

module D = BplAst.Declaration
(* module DD = NtsAst.Declaration *)

let rec decl prg d =
	match d with
	| D.Type (ax,x,tx,Some t) ->
		[]
	| D.Type (ax,x,tx,None) ->
		[]
	| D.Var (ax,x,t) ->
		[]
	| D.Const (ax,x,t,e) ->
                []
	| D.Func (ax,f,(ps,t,e)) ->
		[]
        | D.Proc (ax,n,p) -> []
	| D.Inv e ->
		  eprintf (
			  "Warning: losing invariant declaration in translation"
			  ^^ " to Boogie:\n %s\n" )
			  (E.to_string e);
		  []

and proc pgm ax n (ps,ts,reqs,ens,ds,ss) =

	let ts = List.map (Tup2.map id type_) ts in
	let rs = List.mapi
		(fun i (x,t) ->
			 match x with
			 | None -> sprintf "__ret_%n" i
			 | Some x -> x )
		ts in


	(* Translate returns to assign to return variables before the
	   expression-free Boogie return statement. *)
	let bpl_ss = 
		List.flatten
 		<< List.map stmt
		<< Ls.map_stmts
			(function
			 | Ls.S (ls,S.Return es) when List.length es > 0 ->
				   assert (List.length es = List.length ts);
				   [ Ls.S (ls, S.Assign (List.map Lv.ident rs,
										 es, None)) ;
					 Ls.S ([], S.Return es) ]

			 | s -> s :: [] )
		<| ss in
	let mods = failwith in
	let params = 
		List.map (function
				  | (D.Var (_,x,t) | D.Const (_,x,t,None)) ->
						ident x, type_ t
				  | _ -> failwith "!" ) ps in
	let mod_params = 
		List.filter (((flip List.mem) mods) << fst) params in

	let param_to_init x =
		if List.mem x (List.map fst mod_params)
		then sprintf "%s__init" x
		else x in

	let reqs = List.map (expr << E.map_ident param_to_init) reqs
	and ens = List.map expr ens in

	let inline_depth = Options.get_int "recursion-depth" in

	(* NOTE: don't inline [main], nor any procedure marked as "modular". *)
	let do_inline =
		n <> "main"
		&& not (List.exists ((=) "modular" << fst) ax)
	in
        (* I erased everything from here *)

		

let program pgm = 
	List.flatten 
	<< List.map (decl pgm) 
	<| pgm
