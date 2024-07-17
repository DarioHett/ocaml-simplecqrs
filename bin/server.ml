open Core
open Async
open Async_kernel
open Simplecqrs
open Simplecqrs.Eventstorage

(* Wiring *)
let cmd_reader, cmd_writer = Pipe.create ()
(* let inventory_detail_reader, inventory_detail_writer = Pipe.create ()
let inventory_list_reader, inventory_list_writer = Pipe.create () *)
let event_to_postgres_reader, event_to_postgres_writer = Pipe.create ()
(* 
let inventory_list_task reader =
  let handler = Readmodel.register_handlers_inventory_list_view () in
  let rec loop () =
    Pipe.read reader
    >>= function
    | `Eof ->
      print_endline "Stopped inventory_list_task.";
      Deferred.return ()
    | `Ok evt ->
      handler evt;
      loop ()
  in
  loop ()
;;

let inventory_detail_task reader =
  let handler = Readmodel.register_handlers_inventory_detail_view () in
  let rec loop () =
    Pipe.read reader
    >>= function
    | `Eof ->
      print_endline "Stopped inventory_detail_task.";
      Deferred.return ()
    | `Ok evt ->
      handler evt;
      loop ()
  in
  loop ()
;; *)

let () =
  let recv_cmd_loop conn =
    let bvar = Bvar.create () in
    let recv_cmds = Postgres.receive_notifications ~bvar in
    let%bind () = recv_cmds conn ~channel:"cmds" in
    let rec loop () =
      let sync_cmd = Bvar.wait bvar in
      let%bind c = sync_cmd in
      let cmd =
        try
          Yojson.Safe.from_string c |> Commands.command_of_yojson |> fun x -> Some x
        with
        | _ -> None
      in
      (match cmd with
       | Some c -> Pipe.write_without_pushback cmd_writer c
       | None -> print_endline c);
      Clock.after (sec 1.) >>= fun () -> loop ()
    in
    loop ()
  in
  let send_evts conn () =
    Pipe.read event_to_postgres_reader
    >>= function
    | `Eof -> Deferred.return ()
    | `Ok evt ->
      evt
      |> Events.yojson_of_event
      |> Yojson.Safe.to_string
      (* |> (fun any -> print_endline any; any) *)
      |> sprintf "NOTIFY %s, '%s'" "evts"
      |> Postgres.query_exn conn
  in
  let run () =
    Postgres_async.with_connection
      ~ssl_mode:Postgres_async.Ssl_mode.Disable
      ~server:(Tcp.Where_to_connect.of_host_and_port { host = "localhost"; port = 5432 })
      ~user:"postgres"
      ~password:"simplecqrs"
      ~database:"postgres"
      ~on_handler_exception:`Raise
      (fun conn ->
        let rec loop () =
          let _ = Clock.every' (sec 1.) (send_evts conn) in
          Clock.after (sec 1.) >>= (fun () -> loop ()) in
        ignore (recv_cmd_loop conn);
        loop ()
        )
  in
  ignore (run ())
;;

let () =
  let event_bus =
    let subscribe = Fn.flip Eventbus.subscribe in
    Eventbus.init ()
    (* |> subscribe (fun evt -> Pipe.write_without_pushback inventory_detail_writer evt)
    |> subscribe (fun evt -> Pipe.write_without_pushback inventory_list_writer evt) *)
    |> subscribe (fun evt -> Events.to_string evt |> print_endline)
    |> subscribe (fun evt -> Pipe.write_without_pushback event_to_postgres_writer evt)
  in
  let handler = Commandhandler.handler (EventStorage.save ~bus:event_bus) in
  (* Set up task *)
  let () =
    Clock.every' ~stop:(Pipe.closed cmd_reader) (sec 1.) (fun () ->
      Pipe.read cmd_reader
      >>= function
      | `Eof -> Deferred.return ()
      | `Ok cmd -> Deferred.return (handler cmd))
  in
  ignore ()
;;

(* Start view tasks *)
(* let () = ignore (inventory_detail_task inventory_detail_reader)
let () = ignore (inventory_list_task inventory_list_reader) *)
let () = never_returns (Scheduler.go ())
