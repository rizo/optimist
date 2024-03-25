let ok_1 : (int, 'e) result =
  let __optimist_result_0 = Ok 1 in
  Result.bind __optimist_result_0 (fun x -> Ok (x + 1))

let ok_2 : (int, 'e) result =
  let __optimist_result_0 = Ok 1 and __optimist_result_1 = Ok 2 in
  Result.bind __optimist_result_0 (fun x -> Result.bind __optimist_result_1 (fun y -> Ok (x + y)))

let some_1 : int option =
  let __optimist_option_0 = Some 42 in
  Option.bind __optimist_option_0 (fun x -> Some x)

let some_2 : int option =
  let __optimist_option_0 = Some 1 and __optimist_option_1 = Some 2 in
  Option.bind __optimist_option_0 (fun x -> Option.bind __optimist_option_1 (fun y -> Some (x + y)))

let await_1 : int Lwt.t =
  let __optimist_lwt_0 = Lwt.return 1 in
  let module Reraise = struct
    external reraise : exn -> 'a = "%reraise"
  end in
  Lwt.backtrace_bind
    (fun exn -> try Reraise.reraise exn with exn -> exn)
    __optimist_lwt_0
    (fun x -> Lwt.return (x + 1))

let await_2 : int Lwt.t =
  let __optimist_lwt_0 = Lwt.return 1 and __optimist_lwt_1 = Lwt.return 2 in
  let module Reraise = struct
    external reraise : exn -> 'a = "%reraise"
  end in
  Lwt.backtrace_bind
    (fun exn -> try Reraise.reraise exn with exn -> exn)
    __optimist_lwt_0
    (fun x ->
      let module Reraise = struct
        external reraise : exn -> 'a = "%reraise"
      end in
      Lwt.backtrace_bind
        (fun exn -> try Reraise.reraise exn with exn -> exn)
        __optimist_lwt_1
        (fun y -> Lwt.return (x + y))
    )

let await_ok_1 : (int, 'e) result Lwt.t =
  let __optimist_lwt_result_0 = Lwt.return (Ok 1) in
  let module Reraise = struct
    external reraise : exn -> 'a = "%reraise"
  end in
  Lwt.backtrace_bind
    (fun exn -> try Reraise.reraise exn with exn -> exn)
    __optimist_lwt_result_0
    (fun __optimist_value_0 ->
      match __optimist_value_0 with
      | Ok x -> Lwt.return (Ok (x + 1))
      | Error err -> Lwt.return (Error err)
    )

let await_ok_2 : (int, 'e) result Lwt.t =
  let __optimist_lwt_result_0 = Lwt.return (Ok 1) and __optimist_lwt_result_1 = Lwt.return (Ok 2) in
  let module Reraise = struct
    external reraise : exn -> 'a = "%reraise"
  end in
  Lwt.backtrace_bind
    (fun exn -> try Reraise.reraise exn with exn -> exn)
    __optimist_lwt_result_0
    (fun __optimist_value_0 ->
      match __optimist_value_0 with
      | Ok x ->
        let module Reraise = struct
          external reraise : exn -> 'a = "%reraise"
        end in
        Lwt.backtrace_bind
          (fun exn -> try Reraise.reraise exn with exn -> exn)
          __optimist_lwt_result_1
          (fun __optimist_value_1 ->
            match __optimist_value_1 with
            | Ok y -> Lwt.return (Ok (x + y))
            | Error err -> Lwt.return (Error err)
          )
      | Error err -> Lwt.return (Error err)
    )

let await_some_1 : int option Lwt.t =
  let __optimist_lwt_option_0 = Lwt.return (Some 1) in
  let module Reraise = struct
    external reraise : exn -> 'a = "%reraise"
  end in
  Lwt.backtrace_bind
    (fun exn -> try Reraise.reraise exn with exn -> exn)
    __optimist_lwt_option_0
    (fun __optimist_value_0 ->
      match __optimist_value_0 with
      | Some x -> Lwt.return (Some (x + 1))
      | None -> Lwt.return None
    )

let await_some_2 : int option Lwt.t =
  let __optimist_lwt_option_0 = Lwt.return (Some 1)
  and __optimist_lwt_option_1 = Lwt.return (Some 2) in
  let module Reraise = struct
    external reraise : exn -> 'a = "%reraise"
  end in
  Lwt.backtrace_bind
    (fun exn -> try Reraise.reraise exn with exn -> exn)
    __optimist_lwt_option_0
    (fun __optimist_value_0 ->
      match __optimist_value_0 with
      | Some x ->
        let module Reraise = struct
          external reraise : exn -> 'a = "%reraise"
        end in
        Lwt.backtrace_bind
          (fun exn -> try Reraise.reraise exn with exn -> exn)
          __optimist_lwt_option_1
          (fun __optimist_value_1 ->
            match __optimist_value_1 with
            | Some y -> Lwt.return (Some (x + y))
            | None -> Lwt.return None
          )
      | None -> Lwt.return None
    )

let match_ok_1 : (int, string) result =
  let __optimist_result_0 = Ok 1 in
  Result.bind __optimist_result_0 (fun __optimist_value_0 ->
      match __optimist_value_0 with
      | 1 -> Ok 2
      | _ -> Error "No"
  )

let match_some_1 : int option =
  let __optimist_option_0 = Some 1 in
  Option.bind __optimist_option_0 (fun __optimist_value_0 ->
      match __optimist_value_0 with
      | 1 -> Some 2
      | _ -> None
  )

let match_await_1 : int Lwt.t =
  let __optimist_lwt_0 = Lwt.return 1 in
  let module Reraise = struct
    external reraise : exn -> 'a = "%reraise"
  end in
  Lwt.backtrace_bind
    (fun exn -> try Reraise.reraise exn with exn -> exn)
    __optimist_lwt_0
    (fun __optimist_value_0 ->
      match __optimist_value_0 with
      | 1 -> Lwt.return 2
      | _ -> Lwt.return 0
    )

let match_await_ok_1 : (int, string) result Lwt.t =
  let __optimist_lwt_result_0 = Lwt.return (Ok 1) in
  let module Reraise = struct
    external reraise : exn -> 'a = "%reraise"
  end in
  Lwt.backtrace_bind
    (fun exn -> try Reraise.reraise exn with exn -> exn)
    __optimist_lwt_result_0
    (fun __optimist_value_0 ->
      match __optimist_value_0 with
      | Ok __optimist_value_0 -> (
        match __optimist_value_0 with
        | 1 -> Lwt.return (Ok 2)
        | _ -> Lwt.return (Error "No")
      )
      | Error err -> Lwt.return (Error err)
    )

let match_await_some_1 : int option Lwt.t =
  let __optimist_lwt_option_0 = Lwt.return (Some 1) in
  let module Reraise = struct
    external reraise : exn -> 'a = "%reraise"
  end in
  Lwt.backtrace_bind
    (fun exn -> try Reraise.reraise exn with exn -> exn)
    __optimist_lwt_option_0
    (fun __optimist_value_0 ->
      match __optimist_value_0 with
      | Some __optimist_value_0 -> (
        match __optimist_value_0 with
        | 1 -> Lwt.return (Some 2)
        | _ -> Lwt.return None
      )
      | None -> Lwt.return None
    )

let if_ok_1 : (int, string) result =
  let __optimist_result_0 = Ok true in
  Result.bind __optimist_result_0 (fun __optimist_value_0 ->
      if __optimist_value_0 then Ok 1 else Error "No"
  )

let if_some_1 : int option =
  let __optimist_option_0 = Some true in
  Option.bind __optimist_option_0 (fun __optimist_value_0 ->
      if __optimist_value_0 then Some 1 else None
  )

let if_await_1 : int Lwt.t =
  let __optimist_lwt_0 = Lwt.return true in
  let module Reraise = struct
    external reraise : exn -> 'a = "%reraise"
  end in
  Lwt.backtrace_bind
    (fun exn -> try Reraise.reraise exn with exn -> exn)
    __optimist_lwt_0
    (fun __optimist_value_0 -> if __optimist_value_0 then Lwt.return 1 else Lwt.return 0)

let if_await_ok_1 : (int, string) result Lwt.t =
  let __optimist_lwt_result_0 = Lwt.return (Ok true) in
  let module Reraise = struct
    external reraise : exn -> 'a = "%reraise"
  end in
  Lwt.backtrace_bind
    (fun exn -> try Reraise.reraise exn with exn -> exn)
    __optimist_lwt_result_0
    (fun __optimist_value_0 ->
      match __optimist_value_0 with
      | Ok __optimist_value_0 ->
        if __optimist_value_0 then Lwt.return (Ok 1) else Lwt.return (Error "No")
      | Error err -> Lwt.return (Error err)
    )

let if_await_some_1 : int option Lwt.t =
  let __optimist_lwt_option_0 = Lwt.return (Some true) in
  let module Reraise = struct
    external reraise : exn -> 'a = "%reraise"
  end in
  Lwt.backtrace_bind
    (fun exn -> try Reraise.reraise exn with exn -> exn)
    __optimist_lwt_option_0
    (fun __optimist_value_0 ->
      match __optimist_value_0 with
      | Some __optimist_value_0 ->
        if __optimist_value_0 then Lwt.return (Some 1) else Lwt.return None
      | None -> Lwt.return None
    )

let ok_sequence_1 : (int, string) result =
  let __optimist_result_0 = Ok () in
  Result.bind __optimist_result_0 (fun () -> Ok 1)

let ok_sequence_2 : (int, string) result =
  let __optimist_result_0 = Ok (ignore 1) in
  Result.bind __optimist_result_0 (fun () ->
      let __optimist_result_0 = Ok (ignore 2) in
      Result.bind __optimist_result_0 (fun () ->
          let __optimist_result_0 = Ok (ignore 3) in
          Result.bind __optimist_result_0 (fun () -> Ok 4)
      )
  )

let some_sequence_1 : int option =
  let __optimist_option_0 = Some () in
  Option.bind __optimist_option_0 (fun () -> Some 1)

let some_sequence_2 : int option =
  let __optimist_option_0 = Some (ignore 1) in
  Option.bind __optimist_option_0 (fun () ->
      let __optimist_option_0 = Some (ignore 2) in
      Option.bind __optimist_option_0 (fun () ->
          let __optimist_option_0 = Some (ignore 3) in
          Option.bind __optimist_option_0 (fun () -> Some 4)
      )
  )

let await_sequence_1 : unit Lwt.t =
  let __optimist_lwt_0 = Lwt.return () in
  let module Reraise = struct
    external reraise : exn -> 'a = "%reraise"
  end in
  Lwt.backtrace_bind
    (fun exn -> try Reraise.reraise exn with exn -> exn)
    __optimist_lwt_0
    (fun () -> Lwt.return ())

let await_sequence_2 : int Lwt.t =
  let __optimist_lwt_0 = Lwt.return () in
  let module Reraise = struct
    external reraise : exn -> 'a = "%reraise"
  end in
  Lwt.backtrace_bind
    (fun exn -> try Reraise.reraise exn with exn -> exn)
    __optimist_lwt_0
    (fun () -> Lwt.return 1)

let await_sequence_3 : int Lwt.t =
  let __optimist_lwt_0 = Lwt.return (ignore 1) in
  let module Reraise = struct
    external reraise : exn -> 'a = "%reraise"
  end in
  Lwt.backtrace_bind
    (fun exn -> try Reraise.reraise exn with exn -> exn)
    __optimist_lwt_0
    (fun () ->
      let __optimist_lwt_0 = Lwt.return (ignore 2) in
      let module Reraise = struct
        external reraise : exn -> 'a = "%reraise"
      end in
      Lwt.backtrace_bind
        (fun exn -> try Reraise.reraise exn with exn -> exn)
        __optimist_lwt_0
        (fun () ->
          let __optimist_lwt_0 = Lwt.return (ignore 3) in
          let module Reraise = struct
            external reraise : exn -> 'a = "%reraise"
          end in
          Lwt.backtrace_bind
            (fun exn -> try Reraise.reraise exn with exn -> exn)
            __optimist_lwt_0
            (fun () -> Lwt.return 4)
        )
    )
