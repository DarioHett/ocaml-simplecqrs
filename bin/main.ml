open! Core
open Async


let query_exn postgres string =
  let%bind result = Postgres_async.query_expect_no_data postgres string in
  Or_error.ok_exn result;
  return ()
;;

let print_notifications ?saw_notification postgres ~channel =
  let%bind result =
    Postgres_async.listen_to_notifications postgres ~channel ~f:(fun ~pid:_ ~payload ->
      Option.iter saw_notification ~f:(fun bvar -> Bvar.broadcast bvar ());
      print_s [%message "notification" ~channel ~payload])
  in
  Or_error.ok_exn result;
  return ()
;;

let try_login ?(user = "postgres") ?password ?(database = "postgres") () =
    let saw_notification = Bvar.create () in
    let print_notifications = print_notifications ~saw_notification in
    let _ = Postgres_async.with_connection
      ~ssl_mode:Postgres_async.Ssl_mode.Disable
      ~server:(Tcp.Where_to_connect.of_host_and_port { host = "localhost"; port = 5432})
      ~user
      ?password
      ~database
      ~on_handler_exception:`Raise
      (fun postgres ->
        let%bind () = print_notifications postgres ~channel:"a"  in
        let rec loop () = 
          let sync1 = Bvar.wait saw_notification in
          let%bind () = sync1 in
          loop () in
        loop ()) in
  return ()
;;
let () =
    ignore(
    (* [%expect {| OK; user:postgres database:postgres |}]; *)
    (* let%bind () = try_login ~password:"5606edd27bf95d6f5ad7ffd237443055" () in *)
    let%bind () = try_login ~password:"mysecretpassword" () in
    (* [%expect {| OK; user:postgres database:postgres |}]; *)
    Deferred.return ()) 
  

let () = print_endline "lol"
let () = never_returns (Scheduler.go ())
