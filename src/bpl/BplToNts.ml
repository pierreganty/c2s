(** Translation from array-free goto Boogie programs to NTS. *)

open Prelude
open Printf
(* open Operators *)

open Nts_types 
open Ntsint.Nts_int

let ident = id
(* let nondet = sprintf "__nondet_%n" *)

module T = BplAst.Type
(* module TT = NtsAst.Type *)
let rec type_ = function
	| T.Bool -> failwith
	| T.Int -> failwith
        | T.Bv _ -> failwith "Illegal type BV for array-free goto Boogie prorams."
	| T.T (x,_) -> failwith
	| T.Map (_,ts,t) -> failwith "Not implemented"
	(*| t -> failwith
		  << sprintf "Illegal type `%s' ."
		  <| T.to_string t*)

module L = BplAst.Literal
(* module LL = NtsAst.Literal *)
let lit = function
	| L.True -> failwith
	| L.False -> failwith
	| L.Num n -> failwith
	| L.Bv _ -> failwith "Illegal type BV for array-free goto Boogie prorams."

module Bop = BplAst.BinaryOp
(* module BBop = NtsAst.BinaryOp *)
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
                  | ls, S.Goto ids -> [ ]
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

module P = BplAst.Procedure

let los = function (* returns the list of statements *)
        | D.Proc (_,_,p) -> P.stmts p
        | _ -> failwith "should be a procedure"

let create_trans_list ls =
        let ret_hash = Hashtbl.create 97 in
       (* let create_trans (prev_ls,prev_s) (ls,s) = *)
        let inc_function x s =
                match s with 
                | [],_ -> failwith "empty list"
                | labels,_ ->  (* a statement is a list of ids and a statement *)
                                let src_state = control_of_id_param (!x) in
                                if not (Hashtbl.mem ret_hash src_state) then
                                        begin
                                                let new_rel_hash = Hashtbl.create 97 in
                                                let dest_state =
                                                        control_of_id_param (List.hd labels) in
                                                Hashtbl.add new_rel_hash dest_state [];
                                                Hashtbl.add ret_hash src_state new_rel_hash
                                        end 
                                (* else 
                                        begin
                                        end *)
                                ; x := (List.hd labels)
                (* ; (ls,s) *)
        in (* List.fold_left create_trans (List.hd ls) ls; ret_hash *)
        let init_label ls = 
                match ls with 
                | [] -> failwith "another empty list"
                | x::_ -> begin
                        match x with
                        | labels,_ when ( (List.length labels) > 0) -> (List.hd labels)
                        | _ -> failwith "yet another empty list"
                end 
                in
        let start_label = init_label ls in
        let x = ref (start_label) in 
        List.iter (inc_function x) ls ; ret_hash

        

let create_automaton proc = 
        let lofstms = los proc in
        match lofstms with 
        |[] -> failwith "stmt list empty"
        | _ -> 
        {
        nts_automata_name = (D.name proc); 
        anot = anot_parser ();
        init_states = Hashtbl.create 1;
        final_states = Hashtbl.create 1;
        error_states = Hashtbl.create 1;
        input_vars = [];
        output_vars = [];
        local_vars = [];
        transitions = create_trans_list lofstms;
}

let get_proc_names pgm =
        let lprocs = BplAst.Program.procs pgm in
        let ret_hash = Hashtbl.create 97 in
        List.iter (fun c -> Hashtbl.add ret_hash (D.name c)
        (create_automaton c) ) lprocs; ret_hash

let get_global_vars_names pgm =
        let lvars = BplAst.Program.global_vars pgm in 
        List.map ( fun d -> NtsGenVar(NtsIVar( (D.name d)),NtsUnPrimed)) lvars

let program pgm = 
        (* debug *)
        let lprocs = BplAst.Program.procs pgm in
        let list_of_stmts = 
                if (List.length lprocs) = 0 then 
                        failwith "miek!!"
                else 
                los (List.hd lprocs) in 
        let _ = BplAst.LabeledStatement.print_seq list_of_stmts in
        (* end debug *)
        let pnames = get_proc_names pgm in 
        let gnames = get_global_vars_names pgm in
        let tmp =
{
        nts_system_name = "foo"; 
        nts_global_vars = gnames;
        nts_automata = pnames;
        nts_gvars_init = None;
        nts_system_threads = None;
} in
        printf "%s" (pprint_nts tmp) ;
        tmp
        


