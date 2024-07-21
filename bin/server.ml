open Core
open Async
open Async_kernel
open Simplecqrs
open Simplecqrs.Eventstorage

(* Wiring *)
let cmd_reader, cmd_writer = Pipe.create ()
let event_to_postgres_reader, event_to_postgres_writer = Pipe.create ()

let () =
  let recv_cmd_loop conn =
    let bvar = Bvar.create () in
    let recv_cmds = Postgres.receive_notifications ~bvar in
    let%bind () = recv_cmds conn ~channel:"cmds" in
    let rec loop () =
      let sync_cmd = Bvar.wait bvar in
      let%bind c = sync_cmd in
      (* print_string c; *)
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
      |> sprintf "NOTIFY %s, '%s'" "evts"
      |> Postgres.query_exn conn
  in
  let startup conn () =
    Postgres.query_exn
      conn
      {| CREATE table if not exists events
(
    id     SERIAL PRIMARY KEY,
    agg_id varchar NOT NULL,
    version   BIGINT NOT NULL,
    data      JSONB  NOT NULL,
    UNIQUE (agg_id, version)
); |}
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
        let module EventStorage = Eventstorage.Make(struct let _storage = Queue.create () end) in
        let () =
          let event_bus =
            let subscribe = Fn.flip Eventbus.subscribe in
            Eventbus.init ()
            |> subscribe (fun evt -> Events.to_string evt |> print_endline)
            |> subscribe (fun evt ->
              Pipe.write_without_pushback event_to_postgres_writer evt)
          in
          let handle_descriptors (evtd : event_desciptor) =
            let q =
              sprintf
                {|INSERT INTO public.events (agg_id, "version", "data") VALUES('%s', %d, '%s');|}
                evtd.agg_id
                evtd.version
                (Events.yojson_of_event evtd.event_data |> Yojson.Safe.to_string)
            in
            upon (Postgres.query_exn conn q) (fun _ -> ())
          in
          let handler =
            Commandhandler.handler (module EventStorage) (EventStorage.save ~bus:event_bus ~handle_descriptors)
          in
          (* Set up task *)
          let () =
            Clock.every' ~stop:(Pipe.closed cmd_reader) (sec 1.) (fun () ->
              Pipe.read cmd_reader
              >>= function
              | `Eof -> Deferred.return ()
              | `Ok cmd -> Deferred.return (handler cmd))
          in
          ignore ()
        in
        let rec loop () =
          let _ = Clock.every' (sec 1.) (send_evts conn) in
          Clock.after (sec 1.) >>= fun () -> loop ()
        in
        (* upon (startup conn ()) (fun _ -> ()); *)
        ignore (recv_cmd_loop conn);
        loop ())
  in
  ignore (run ())
;;

let () = never_returns (Scheduler.go ())
