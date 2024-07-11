open! Core
open Async

let try_login ?(user = "postgres") ?password ?(database = "postgres") () =
  let get_user postgres =
    let u_d = Set_once.create () in
    let%bind result =
      Postgres_async.query
        postgres
                "SELECT CURRENT_USER, current_database()"
        ~handle_row:(fun ~column_names:_ ~values ->
        match values with
        | [| Some u; Some d |] -> Set_once.set_exn u_d [%here] (u, d)
        | _ -> failwith "bad query response")
    in
    Or_error.ok_exn result;
    return (Set_once.get_exn u_d [%here])
  in
  let%bind result =
    Postgres_async.with_connection
      ~ssl_mode:Postgres_async.Ssl_mode.Disable
      ~server:(Tcp.Where_to_connect.of_host_and_port { host = "0.0.0.0"; port = 5432})
      ~user
      ?password
      ~database
      ~on_handler_exception:`Raise
      get_user
  in
  (* we can't print any more than "login failed" because the error messages are not stable
     wrt. postgres versions. *)
  (match result with
   | Ok (u, d) -> printf "OK; user:%s database:%s\n" u d
   | Error x -> printf "%s" (Sexp.to_string (Error.sexp_of_t x)); printf "Login failed\n");
  return ()
;;
let () =
  ignore(
  upon(
  (* [%expect {| OK; user:postgres database:postgres |}]; *)
  let%bind () = try_login ~password:"mysecretpassword" () in
  (* [%expect {| OK; user:postgres database:postgres |}]; *)
  Deferred.return ()) (fun () -> upon (Clock.after (sec 0.1)) (fun () -> print_endline "lol"))
)
let () = print_endline "lol"
let () = never_returns (Scheduler.go ())
