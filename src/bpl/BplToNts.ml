(** Translation from array-free goto Boogie programs to NTS. *)

open Prelude
open Printf
(* open Operators *)

open Nts_types 
open Ntsint.Nts_int

let ident = id
(* let nondet = sprintf "__nondet_%n" *)

module T = BplAst.Type
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
module Bop = BplAst.BinaryOp

let bool_binop = function
        (* nts_gen_bool_binop *)
	| Bop.Or -> CntGenBOr
	| Bop.And -> CntGenBAnd
        | _ -> failwith "wrong guy"
let cnt_binop = function
        (* cnt_binop *)
	| Bop.Eq -> CntEq
	| Bop.Neq -> CntNeq
	| Bop.Lt -> CntLt
	| Bop.Gt -> CntGt
	| Bop.Lte -> CntLeq
	| Bop.Gte -> CntGeq
        | _ -> failwith "wrong guy"
let arithm_binop = function
        (* nts_gen_arithm_binop *)
	| Bop.Plus -> CntGenSum
	| Bop.Minus -> CntGenMinus
	| Bop.Times -> CntGenProd
	| Bop.Div -> CntGenDiv
	| Bop.Mod -> CntGenMod
	| op -> failwith
		<< sprintf "Illegal oper `%s' for concurrent programs."
		<| Bop.to_string op

module E = BplAst.Expression
let quant q = match q with
	| E.Forall | E.Exists -> failwith "Illegal to have quantifiers"
let rec gen_relation e = match e with
	(* | E.Id i -> CntGenVar(NtsGenVar(NtsIVar(i),NtsUnPrimed))*)
	| E.Lit c -> begin match c with 
                        |L.True -> CntGenTrue
                        |L.False -> CntGenFalse
                        |_ -> failwith "Illegal boolean cst" end
	| E.Bin (op,e,f) -> begin match op with 
                        | Bop.Eq| Bop.Neq| Bop.Lt| Bop.Gt| Bop.Lte| Bop.Gte
                        -> CntGenRel(cnt_binop op, gen_arithm_expr e, gen_arithm_expr f) 
                        | Bop.Or| Bop.And
                        -> CntGenRelComp(bool_binop op, gen_relation e, gen_relation f)
                        | Bop.Plus| Bop.Minus| Bop.Times| Bop.Div| Bop.Mod
                        -> failwith "Illegal arithm_binop"
                        | _ -> failwith "Illegal op or comp"
                        end
        | E.Not c -> CntGenNot(gen_relation c)
        | _ -> failwith 
        	<< sprintf "Illegal `%s'." 
                <| PrettyPrinting.render (E.print e)
and gen_arithm_expr e = match e with 
	| E.Id i -> CntGenVar(NtsGenVar(NtsIVar(i),NtsUnPrimed))
	| E.Lit c -> begin match c with 
                        | L.Num n -> CntGenCst(Big_int.big_int_of_int n)
                        | L.True | L.False -> failwith "Boolean variables not supported, turn them into integers"
                        | _ -> failwith "Illegal numerical constant"
                     end 
	| E.Bin (op,e,f) -> begin match op with 
                        | Bop.Plus| Bop.Minus| Bop.Times| Bop.Div| Bop.Mod
                        -> CntGenArithmBOp(arithm_binop op, gen_arithm_expr e, gen_arithm_expr f)
                        | Bop.Or| Bop.And | Bop.Eq| Bop.Neq| Bop.Lt| Bop.Gt| Bop.Lte| Bop.Gte
                        -> failwith "Illegal"
                        | _ -> failwith "Illegal op or comp"
        end 
        | _ -> failwith "Illegal stuff"


module Lv = BplAst.Lvalue
module S = BplAst.Statement
module Ls = BplAst.LabeledStatement
module A = BplAst.Attribute
module D = BplAst.Declaration
module P = BplAst.Procedure

let create_trans_list ls =
        let ret_hash = Hashtbl.create 97 in
        let inc_function x s =
        match !x with 
        | [],_ -> failwith "empty list of labels"
        | lbl::_,S.Goto(dests) -> (* the particular case of gotos *)
                        let src_state = control_of_id_param lbl in
                        if not (Hashtbl.mem ret_hash src_state) then
                                begin
                                        let new_rel_hash = Hashtbl.create 97 in
                                        let add_to_dest_table z = 
                                                let dest_state = control_of_id_param z in
                                                Hashtbl.add
                                                new_rel_hash
                                                dest_state [CntGenHavoc([])];
                                        in List.iter (fun d ->
                                                add_to_dest_table
                                                d ) dests 
                                        ; Hashtbl.add ret_hash src_state new_rel_hash
                                end 
                        ; x := s 
        | lbl::_,S.Return -> x:=s (* skip the return transition: it is triggered only if returns appears before the last *)
        | lbl::_,S.Call(_,n,params,_) ->  (* a statement is a list of ids and a statement *)
                        let expr_builder = List.map gen_arithm_expr params in
                        let transit =
                                CntGenCall(n,None,expr_builder)::CntGenHavoc([])::[]
                        in 
                        let src_state = control_of_id_param lbl in
                        if not (Hashtbl.mem ret_hash src_state) then
                                begin
                                        match s with 
                                        | [],_ -> failwith "empty list of labels"
                                        | lbl::_,_ -> 
                                        let new_rel_hash = Hashtbl.create 97 in
                                        let dest_state =
                                                control_of_id_param
                                                lbl in
                                        Hashtbl.add new_rel_hash dest_state transit;
                                        Hashtbl.add ret_hash src_state new_rel_hash
                                end 
                        ; x := s
        | lbl::_,S.Havoc(lov) ->  (* an havoc statement of a list of variables *)
                        let get_nts_id = (fun  c ->
                                NtsGenVar(NtsIVar(c),NtsUnPrimed)) in 
                        let nts_ids = List.map get_nts_id lov in 
                        let src_state = control_of_id_param lbl in
                        if not (Hashtbl.mem ret_hash src_state) then
                                begin
                                        match s with 
                                        | [],_ -> failwith "empty list of labels"
                                        | lbl::_,_ -> 
                                        let new_rel_hash = Hashtbl.create 97 in
                                        let dest_state =
                                                control_of_id_param
                                                lbl in
                                        Hashtbl.add new_rel_hash
                                        dest_state
                                        [CntGenHavoc(nts_ids)];
                                        Hashtbl.add ret_hash src_state new_rel_hash
                                end 
                        ; x := s
        | lbl::_,S.Assume(_,rel) ->  (* assume statement *)
                        let relation = gen_relation rel in
                        let transit = CntGenGuard(relation)::CntGenHavoc([])::[] in 
                        let src_state = control_of_id_param lbl in
                        if not (Hashtbl.mem ret_hash src_state) then
                                begin
                                        match s with 
                                        | [],_ -> failwith "empty list of labels"
                                        | lbl::_,_ -> 
                                        let new_rel_hash = Hashtbl.create 97 in
                                        let dest_state =
                                                control_of_id_param
                                                lbl in
                                        Hashtbl.add new_rel_hash
                                        dest_state transit;
                                        Hashtbl.add ret_hash src_state new_rel_hash
                                end 
                        ; x := s
        | lbl::_,S.Assign(lhs,rhs) ->  (* an assignment *)
                        let arithm_expr = match rhs with
                                | expr::[] -> gen_arithm_expr expr 
                                | _ -> failwith "Illegal:
                                multiple expressions in rhs of assignment" in
                        let raw_var = match lhs with
                                | var::[] -> Lv.name var
                                | _ -> failwith "Illegal: multiples ids in lhs of assignment" in
                        let primed_var = NtsGenVar(NtsIVar(raw_var),NtsPrimed) in
                        let unprimed_var = NtsGenVar(NtsIVar(raw_var),NtsUnPrimed) in
                        let src_state = control_of_id_param lbl in
                        let transit = CntGenGuard(CntGenRel(CntEq,CntGenVar(primed_var),arithm_expr))::CntGenHavoc([unprimed_var])::[] in
                        if not (Hashtbl.mem ret_hash src_state) then
                                begin
                                        match s with 
                                        | [],_ -> failwith "empty list of labels"
                                        | lbl::_,_ -> 
                                        let new_rel_hash = Hashtbl.create 97 in
                                        let dest_state =
                                                control_of_id_param
                                                lbl in
                                        Hashtbl.add new_rel_hash dest_state transit;
                                        Hashtbl.add ret_hash src_state new_rel_hash
                                end 
                        ; x := s
        | _,_ -> failwith "Illegal (or perhaps a return)"
        in
        let x = ref (List.hd ls) in List.iter (inc_function x) (List.tl ls) ; ret_hash
        (* TODO untested if ls = 1 statement or if the first statement is a goto
         * or if returns in the middle *)

type switch = Localvars | InParams

let create_final_states_list ls =
        let ret_hash = Hashtbl.create 97 in
        let final s =
        match s with 
        | [],_ -> failwith "empty list of labels"
        | lbl::_,S.Return -> Hashtbl.add ret_hash (control_of_id_param lbl) () 
        | _,_ -> ()
        in
        List.iter final ls ; ret_hash

let create_automaton impl = 
        let lostmts = function (* returns the list of statements *)
                | D.Impl(_,_,p) -> P.stmts p
                | _ -> failwith "should be an implementation"
        in
        let get_param_name (name,_) = NtsGenVar(NtsIVar(name),NtsUnPrimed)
        in 
        let get_lvar_name v =
                match v with
                | D.Var(_,name,_,_) -> NtsGenVar(NtsIVar(name),NtsUnPrimed)
                | _ -> failwith "non lvar"
        in 
        let get d c =
                match c with
                | D.Impl (ax,n,p) -> begin match p with
                                 | (_,ps,_,_,lv,_) -> begin match d with 
                                                          | Localvars -> List.map get_lvar_name lv
                                                          | InParams ->  List.map get_param_name ps
                                                     end 
                                     end
                | _ -> failwith "Should be an implementation"
        in
        let build_table_of_initial_state =
                let ret_hash = Hashtbl.create 97 in
                Hashtbl.add ret_hash (control_of_id_param "anon0") () ; ret_hash
                (* very dangerous *)
        in
        let listst = lostmts impl in
                        {
                                nts_automata_name = (D.name impl); 
                                anot = anot_parser ();
                                init_states = build_table_of_initial_state;
                                final_states = create_final_states_list listst;
                                error_states = Hashtbl.create 1;
                                input_vars = get InParams impl;
                                output_vars = [];
                                local_vars = get Localvars impl;
                                transitions = create_trans_list listst;
                        } 

let build_automaton_from_impl pgm =
        let limpls = BplAst.Program.impls pgm in
        let ret_hash = Hashtbl.create 97 in
        List.iter (fun c -> Hashtbl.add ret_hash (D.name c)
        (create_automaton c) ) limpls; ret_hash

let get_global_vars_names pgm =
        let lvars = BplAst.Program.global_vars pgm in 
        List.map ( fun d -> NtsGenVar(NtsIVar( (D.name d)),NtsUnPrimed)) lvars

let get_global_const_names pgm =
        let consts = BplAst.Program.global_consts pgm in 
        List.map ( fun d -> NtsGenVar(NtsIVar( (D.name d)),NtsUnPrimed)) consts

let from_axiom_to_rel d =
        match d with 
        | D.Axiom(_,rel) -> gen_relation rel
        | _ -> failwith "Illegal"

let get_init_condition pgm =
        let axioms = BplAst.Program.axioms pgm in 
        List.map from_axiom_to_rel axioms

let program pgm = 
        let new_label =
        let count = ref 0 in
        fun () -> 
                count := !count+1;
                Printf.sprintf "L%n" (!count)
        in 
        let decorate c =
                match c with
                | _::_,s -> [c]
                | [],s -> [[new_label()],s]
                
        in
        let lablledpgm = BplAst.Program.map_stmts (fun c -> decorate c) pgm in
        (* DEBUG let document = BplAst.Program.print lablledpgm in
        let _ = printf "%s" (PrettyPrinting.render document) in *)
        let pnames = build_automaton_from_impl lablledpgm in 
        let gnames = get_global_vars_names lablledpgm in
        let cnames = get_global_const_names lablledpgm in
        let initial_condition = get_init_condition lablledpgm in
        let tmp =
{
        nts_system_name = "foo"; 
        nts_global_vars = gnames@cnames;
        nts_automata = pnames;
        nts_gvars_init = Some(initial_condition);
        nts_system_threads = None;
} 
        in
        let outputstring =
         let post_proc_output nts_string =
          let nodots = Str.global_replace (Str.regexp "\\.") "_" nts_string in
          let noseq = Str.global_replace (Str.regexp "Seq_Main") "main" nodots in
          let renamealt = Str.global_replace (Str.regexp "Main") "altmain"
          noseq in renamealt 
        in (post_proc_output  (pprint_nts tmp)) ^ "
boogie_si_record_int {
in k: int;

initial anon0;
final exit;
 anon0 -> exit { havoc() }
}
        "
        in printf "%s" outputstring
        ;
        tmp
