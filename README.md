# Optimist

An OCaml ppx that helps you write happier code.


## Supported rewrite rules

```
let%ok ... = ... in ...
let%some ... = ... in ...
let%await ... = ... in ...
let%await.ok ... = ... in ...
let%await.some ... = ... in ...

match%ok ... with ...
match%some ... with ...
match%await ... with ...
match%await.ok ... with ...
match%await.some ... with ...

if%ok ... then ... else ...
if%some ... then ... else ...
if%await ... then ... else ...
if%await.ok ... then ... else ...
if%await.some ... then ... else ...
```


## Lwt backtrace propagation

The main difference between using `(let*)` and `let%await` (or the standard
`let%lwt`) is that, with the ppx version, the async exceptions will correctly
propagate their backtraces.

The following example demonstrates the fully expanded code for `%await.ok`

```
let%await.ok p = m in e

-->

let module Reraise =
  struct
    external reraise : exn -> 'a = "%reraise"
  end
in
Lwt.backtrace_bind
  (fun exn -> try Reraise.reraise exn with exn -> exn)
  m
  (fun p_result ->
    match with
    | Ok p -> e
    | Error e -> Lwt.return (Error e))
```

