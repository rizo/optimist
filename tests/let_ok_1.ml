(* Let expressions *)

let ok_1 : (int, 'e) result =
  let%ok x = Ok 1 in
  Ok (x + 1)


let ok_2 : (int, 'e) result =
  let%ok x = Ok 1 and y = Ok 2 in
  Ok (x + y)


let some_1 : int option =
  let%some x = Some 42 in
  Some x


let some_2 : int option =
  let%some x = Some 1 and y = Some 2 in
  Some (x + y)


let await_1 : int Lwt.t =
  let%await x = Lwt.return 1 in
  Lwt.return (x + 1)


let await_2 : int Lwt.t =
  let%await x = Lwt.return 1 and y = Lwt.return 2 in
  Lwt.return (x + y)


let await_ok_1 : (int, 'e) result Lwt.t =
  let%await.ok x = Lwt.return (Ok 1) in
  Lwt.return (Ok (x + 1))


let await_ok_2 : (int, 'e) result Lwt.t =
  let%await.ok x = Lwt.return (Ok 1) and y = Lwt.return (Ok 2) in
  Lwt.return (Ok (x + y))


let await_some_1 : int option Lwt.t =
  let%await.some x = Lwt.return (Some 1) in
  Lwt.return (Some (x + 1))


let await_some_2 : int option Lwt.t =
  let%await.some x = Lwt.return (Some 1) and y = Lwt.return (Some 2) in
  Lwt.return (Some (x + y))


(* Match expressions *)

let match_ok_1 : (int, string) result =
  match%ok Ok 1 with
  | 1 -> Ok 2
  | _ -> Error "No"


let match_some_1 : int option =
  match%some Some 1 with
  | 1 -> Some 2
  | _ -> None


let match_await_1 : int Lwt.t =
  match%await Lwt.return 1 with
  | 1 -> Lwt.return 2
  | _ -> Lwt.return 0


let match_await_ok_1 : (int, string) result Lwt.t =
  match%await.ok Lwt.return (Ok 1) with
  | 1 -> Lwt.return (Ok 2)
  | _ -> Lwt.return (Error "No")


let match_await_some_1 : int option Lwt.t =
  match%await.some Lwt.return (Some 1) with
  | 1 -> Lwt.return (Some 2)
  | _ -> Lwt.return None


(* If expressions *)

let if_ok_1 : (int, string) result = if%ok Ok true then Ok 1 else Error "No"

let if_some_1 : int option = if%some Some true then Some 1 else None

let if_await_1 : int Lwt.t =
  if%await Lwt.return true then Lwt.return 1 else Lwt.return 0


let if_await_ok_1 : (int, string) result Lwt.t =
  if%await.ok Lwt.return (Ok true) then Lwt.return (Ok 1)
  else Lwt.return (Error "No")


let if_await_some_1 : int option Lwt.t =
  if%await.some Lwt.return (Some true) then Lwt.return (Some 1)
  else Lwt.return None


(* Sequences *)

let ok_sequence_1 : (int, string) result =
  [%ok
    Ok ();
    Ok 1]


let ok_sequence_2 : (int, string) result =
  [%ok
    Ok (ignore 1);
    Ok (ignore 2);
    Ok (ignore 3);
    Ok 4]


let some_sequence_1 : int option =
  [%some
    Some ();
    Some 1]


let some_sequence_2 : int option =
  [%some
    Some (ignore 1);
    Some (ignore 2);
    Some (ignore 3);
    Some 4]


let await_sequence_1 : unit Lwt.t =
  let%await () = Lwt.return () in
  Lwt.return ()


let await_sequence_2 : int Lwt.t =
  [%await
    Lwt.return ();
    Lwt.return 1]


let await_sequence_3 : int Lwt.t =
  [%await
    Lwt.return (ignore 1);
    Lwt.return (ignore 2);
    Lwt.return (ignore 3);
    Lwt.return 4]
