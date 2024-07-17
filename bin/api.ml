open Core
open Async
open Async_kernel
open Simplecqrs

(* Wiring *)
let cmd_reader, cmd_writer = Pipe.create ()

let () =
  let channel = "cmds" in
  let run () =
    let%bind _ =
      Postgres_async.with_connection
        ~ssl_mode:Postgres_async.Ssl_mode.Disable
        ~server:
          (Tcp.Where_to_connect.of_host_and_port { host = "localhost"; port = 5432 })
        ~user:"postgres"
        ~password:"simplecqrs"
        ~database:"postgres"
        ~on_handler_exception:`Raise
        (fun conn ->
          let _ =
            Clock.every' ~stop:(Pipe.closed cmd_reader) (sec 1.) (fun () ->
              Pipe.read cmd_reader
              >>= function
              | `Eof -> Deferred.return ()
              | `Ok cmd -> Postgres.query_exn conn (sprintf "NOTIFY %s, '%s'" channel cmd))
          in
          Deferred.never ())
    in
    return ()
  in
  ignore (run ())
;;

let () =
  let module Body = Cohttp_async.Body in
  let module Server = Cohttp_async.Server in
  ignore
    (Cohttp_async.Server.create
       ~on_handler_error:`Raise
       (Async.Tcp.Where_to_listen.of_port 8000)
       (fun ~body _ req ->
          match req |> Cohttp.Request.meth with
          | `POST ->
            let%bind body = Body.to_string body in
            let cmd =
              try
                Yojson.Safe.from_string body
                |> Commands.command_of_yojson
                |> fun x -> Some x
              with
              | _ -> None
            in
            let headers =
              Cohttp.Header.add_list
                (Cohttp.Header.init ())
                [ "Content-Type", "text/plain"; "Access-Control-Allow-Origin", "*" ]
            in
            (match cmd with
             | Some _ ->
               Pipe.write_without_pushback cmd_writer body;
               Server.respond `OK ~body:(`String "Command dispatched.") ~headers
             | None ->
               Server.respond
                 `Conflict
                 ~body:(`String "Could not interpret body as command.")
                 ~headers)
          | _ -> Server.respond `Method_not_allowed)
     >>= fun _ -> Deferred.never ())
;;

let () = never_returns (Scheduler.go ())
