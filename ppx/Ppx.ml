open! Ppxlib
open Ast_builder.Default

let prefix = "__optimist"

let default_loc = ref Location.none

let gen_name ~label i = String.concat "_" [ prefix; label; string_of_int i ]

let gen_bindings ~label l =
  let aux i binding =
    {
      binding with
      pvb_pat = pvar ~loc:binding.pvb_expr.pexp_loc (gen_name ~label i);
    }
  in
  List.mapi aux l


let gen_binds_await e_loc l e =
  let rec aux i bindings =
    match bindings with
    | [] -> e
    | binding :: t ->
      let name =
        evar ~loc:binding.pvb_expr.pexp_loc (gen_name ~label:"lwt" i)
      in
      let fun_ =
        let loc = e_loc in
        [%expr fun [%p binding.pvb_pat] -> [%e aux (i + 1) t]]
      in
      let new_exp =
        let loc = e_loc in
        [%expr
          let module Reraise = struct
            external reraise : exn -> 'a = "%reraise"
          end in
          Lwt.backtrace_bind
            (fun exn -> try Reraise.reraise exn with exn -> exn)
            [%e name] [%e fun_]]
      in
      { new_exp with pexp_attributes = binding.pvb_attributes }
  in
  aux 0 l


let gen_binds_await_ok e_loc l body =
  let rec aux i bindings =
    match bindings with
    | [] -> body
    | binding :: t ->
      let monad_evar =
        evar ~loc:binding.pvb_expr.pexp_loc (gen_name ~label:"lwt_result" i)
      in
      let value_evar =
        evar ~loc:binding.pvb_expr.pexp_loc (gen_name ~label:"value" i)
      in
      let value_pvar =
        pvar ~loc:binding.pvb_expr.pexp_loc (gen_name ~label:"value" i)
      in
      let fun_ =
        let loc = e_loc in
        [%expr
          fun [%p value_pvar] ->
            match [%e value_evar] with
            | Ok [%p binding.pvb_pat] -> [%e aux (i + 1) t]
            | Error err -> Lwt.return (Error err)]
      in
      let new_exp =
        let loc = e_loc in
        [%expr
          let module Reraise = struct
            external reraise : exn -> 'a = "%reraise"
          end in
          Lwt.backtrace_bind
            (fun exn -> try Reraise.reraise exn with exn -> exn)
            [%e monad_evar] [%e fun_]]
      in
      { new_exp with pexp_attributes = binding.pvb_attributes }
  in
  aux 0 l


let gen_binds_await_some e_loc l body =
  let rec aux i bindings =
    match bindings with
    | [] -> body
    | binding :: t ->
      let monad_evar =
        evar ~loc:binding.pvb_expr.pexp_loc (gen_name ~label:"lwt_option" i)
      in
      let option_evar =
        evar ~loc:binding.pvb_expr.pexp_loc (gen_name ~label:"value" i)
      in
      let option_pvar =
        pvar ~loc:binding.pvb_expr.pexp_loc (gen_name ~label:"value" i)
      in
      let fun_ =
        let loc = e_loc in
        [%expr
          fun [%p option_pvar] ->
            match [%e option_evar] with
            | Some [%p binding.pvb_pat] -> [%e aux (i + 1) t]
            | None -> Lwt.return None]
      in
      let new_exp =
        let loc = e_loc in
        [%expr
          let module Reraise = struct
            external reraise : exn -> 'a = "%reraise"
          end in
          Lwt.backtrace_bind
            (fun exn -> try Reraise.reraise exn with exn -> exn)
            [%e monad_evar] [%e fun_]]
      in
      { new_exp with pexp_attributes = binding.pvb_attributes }
  in
  aux 0 l


let gen_binds_ok e_loc l e =
  let rec aux i bindings =
    match bindings with
    | [] -> e
    | binding :: t ->
      let monad_evar =
        evar ~loc:binding.pvb_expr.pexp_loc (gen_name ~label:"result" i)
      in
      let fun_ =
        let loc = e_loc in
        [%expr fun [%p binding.pvb_pat] -> [%e aux (i + 1) t]]
      in
      let new_exp =
        let loc = e_loc in
        [%expr Result.bind [%e monad_evar] [%e fun_]]
      in
      { new_exp with pexp_attributes = binding.pvb_attributes }
  in
  aux 0 l


let gen_binds_some e_loc l e =
  let rec aux i bindings =
    match bindings with
    | [] -> e
    | binding :: t ->
      let monad_evar =
        evar ~loc:binding.pvb_expr.pexp_loc (gen_name ~label:"option" i)
      in
      let fun_ =
        let loc = e_loc in
        [%expr fun [%p binding.pvb_pat] -> [%e aux (i + 1) t]]
      in
      let new_exp =
        let loc = e_loc in
        [%expr Option.bind [%e monad_evar] [%e fun_]]
      in
      { new_exp with pexp_attributes = binding.pvb_attributes }
  in
  aux 0 l


let gen_bind_for_kind = function
  | `ok -> gen_binds_ok
  | `some -> gen_binds_some
  | `await -> gen_binds_await
  | `await_ok -> gen_binds_await_ok
  | `await_some -> gen_binds_await_some


let rec gen_sequnece ~loc ~kind ~label expr1 expr2 =
  let expr_binding =
    let pat = pvar ~loc:!default_loc (gen_name ~label 0) in
    value_binding ~loc:!default_loc ~pat ~expr:expr1
  in
  let value_binding =
    let cons =
      constructor_declaration ~loc:!default_loc
        ~name:(Loc.make ~loc:!default_loc "()")
        ~args:(Pcstr_tuple []) ~res:None
    in
    let pat = pconstruct cons None in
    value_binding ~loc:!default_loc ~pat ~expr:expr2
  in
  let bind = gen_bind_for_kind kind in
  let expr2 =
    match expr2.pexp_desc with
    | Pexp_sequence (expr1', expr2') ->
      gen_sequnece ~loc ~kind ~label expr1' expr2'
    | _ -> expr2
  in
  pexp_let ~loc:!default_loc Nonrecursive [ expr_binding ]
    (bind loc [ value_binding ] expr2)


let map_expression mapper kind exp attributes _ext_loc =
  default_loc := exp.pexp_loc;
  let pexp_attributes = attributes @ exp.pexp_attributes in
  let label =
    match kind with
    | `ok -> "result"
    | `some -> "option"
    | `await_ok -> "lwt_result"
    | `await -> "lwt"
    | `await_some -> "lwt_option"
  in
  match exp.pexp_desc with
  | Pexp_let (Nonrecursive, vbl, e) ->
    let bind = gen_bind_for_kind kind in
    let new_exp =
      pexp_let ~loc:!default_loc Nonrecursive (gen_bindings ~label vbl)
        (bind exp.pexp_loc vbl e)
    in
    Some (mapper#expression { new_exp with pexp_attributes })
  | Pexp_match (expr, cases) ->
    let expr_binding =
      let pat = pvar ~loc:!default_loc (gen_name ~label 0) in
      value_binding ~loc:!default_loc ~pat ~expr
    in
    let value_binding =
      let pat = pvar ~loc:!default_loc (gen_name ~label:"value" 0) in
      value_binding ~loc:!default_loc ~pat ~expr
    in
    let new_match =
      pexp_match ~loc:!default_loc
        (evar ~loc:!default_loc (gen_name ~label:"value" 0))
        cases
    in
    let bind = gen_bind_for_kind kind in
    let new_exp =
      pexp_let ~loc:!default_loc Nonrecursive [ expr_binding ]
        (bind exp.pexp_loc [ value_binding ] new_match)
    in
    Some (mapper#expression { new_exp with pexp_attributes })
  | Pexp_ifthenelse (cond, if_true, if_false) ->
    let expr_binding =
      let pat = pvar ~loc:!default_loc (gen_name ~label 0) in
      value_binding ~loc:!default_loc ~pat ~expr:cond
    in
    let value_binding =
      let pat = pvar ~loc:!default_loc (gen_name ~label:"value" 0) in
      value_binding ~loc:!default_loc ~pat ~expr:cond
    in
    let new_ifthenelse =
      pexp_ifthenelse ~loc:!default_loc
        (evar ~loc:!default_loc (gen_name ~label:"value" 0))
        if_true if_false
    in
    let bind = gen_bind_for_kind kind in
    let new_exp =
      pexp_let ~loc:!default_loc Nonrecursive [ expr_binding ]
        (bind exp.pexp_loc [ value_binding ] new_ifthenelse)
    in
    Some (mapper#expression { new_exp with pexp_attributes })
  | Pexp_sequence (expr1, expr2) ->
    let new_exp = gen_sequnece ~loc:exp.pexp_loc ~kind ~label expr1 expr2 in
    Some (mapper#expression { new_exp with pexp_attributes })
  | _ -> None


class mapper =
  object (self)
    inherit Ast_traverse.map as super

    method! expression expr =
      match expr with
      (* let%ok ... *)
      | {
       pexp_desc =
         Pexp_extension
           ( { txt = "ok"; loc = ext_loc },
             PStr [ { pstr_desc = Pstr_eval (exp, _); _ } ] );
       _;
      } -> (
        match map_expression self `ok exp expr.pexp_attributes ext_loc with
        | Some expr' -> expr'
        | None -> expr)
      (* let%some ... *)
      | {
       pexp_desc =
         Pexp_extension
           ( { txt = "some"; loc = ext_loc },
             PStr [ { pstr_desc = Pstr_eval (exp, _); _ } ] );
       _;
      } -> (
        match map_expression self `some exp expr.pexp_attributes ext_loc with
        | Some expr' -> expr'
        | None -> expr)
      (* let%await.ok ... *)
      | {
       pexp_desc =
         Pexp_extension
           ( { txt = "await.ok"; loc = ext_loc },
             PStr [ { pstr_desc = Pstr_eval (exp, _); _ } ] );
       _;
      } -> (
        match
          map_expression self `await_ok exp expr.pexp_attributes ext_loc
        with
        | Some expr' -> expr'
        | None -> expr)
      (* let%await.some ... *)
      | {
       pexp_desc =
         Pexp_extension
           ( { txt = "await.some"; loc = ext_loc },
             PStr [ { pstr_desc = Pstr_eval (exp, _); _ } ] );
       _;
      } -> (
        match
          map_expression self `await_some exp expr.pexp_attributes ext_loc
        with
        | Some expr' -> expr'
        | None -> expr)
      (* let%await ...  *)
      | {
       pexp_desc =
         Pexp_extension
           ( { txt = "await"; loc = ext_loc },
             PStr [ { pstr_desc = Pstr_eval (exp, _); _ } ] );
       _;
      } -> (
        match map_expression self `await exp expr.pexp_attributes ext_loc with
        | Some expr' -> expr'
        | None -> expr)
      | _ -> super#expression expr
  end

let () =
  let mapper = new mapper in
  Driver.register_transformation "ppx_optimist" ~impl:mapper#structure
    ~intf:mapper#signature
