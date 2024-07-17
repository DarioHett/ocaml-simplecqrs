open Core
open Async
open Async_kernel
open Simplecqrs

(* Wiring *)
let inventory_detail_reader, inventory_detail_writer = Pipe.create ()
let inventory_list_reader, inventory_list_writer = Pipe.create ()

let inventory_list_task reader =
  let handler = Readmodel.register_handlers_inventory_list_view () in
  let rec loop () =
    Pipe.read reader
    >>| (function
           | `Eof -> print_endline "Stopped inventory_list_task."
           | `Ok evt -> handler evt)
    >>= loop
  in
  loop ()
;;

let inventory_detail_task reader =
  let handler = Readmodel.register_handlers_inventory_detail_view () in
  let rec loop () =
    Pipe.read reader
    >>| function
    | `Eof ->
      print_endline "Stopped inventory_detail_task.";
    | `Ok evt ->
      handler evt;
       >>= loop
  in
  loop ()
;;

let () =
  let recv_evt_loop conn =
    let bvar = Bvar.create () in
    let recv_evt = Postgres.receive_notifications ~bvar in
    let%bind () = recv_evt conn ~channel:"evts" in
    let rec loop () =
      let sync_evt = Bvar.wait bvar in
      let%bind e = sync_evt in
      let evt =
        try Yojson.Safe.from_string e |> Events.event_of_yojson |> fun x -> Some x with
        | _ -> None
      in
      (match evt with
       | Some e ->
         Pipe.write_without_pushback inventory_detail_writer e;
         Pipe.write_without_pushback inventory_list_writer e
       | None -> print_endline e);
      loop ()
    in
    loop ()
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
        let rec loop () = Clock.after (sec 1.) >>= fun () -> loop () in
        ignore (recv_evt_loop conn);
        loop ())
  in
  ignore (run ())
;;

(* Start view tasks *)
let () = ignore (inventory_detail_task inventory_detail_reader)
let () = ignore (inventory_list_task inventory_list_reader)
let () = never_returns (Scheduler.go ())
