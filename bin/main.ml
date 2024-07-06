open Core
open Async
open Async_kernel
open Simplecqrs
open Simplecqrs.Eventstorage

(* Wiring *)
let cmd_reader, cmd_writer = Pipe.create ()
let inventory_detail_reader, inventory_detail_writer = Pipe.create ()
let inventory_list_reader, inventory_list_writer = Pipe.create ()

let () =
  (*  *)
  let event_bus =
    let subscribe = Fn.flip Eventbus.subscribe in
    Eventbus.init ()
    |> subscribe (fun evt -> Pipe.write_without_pushback inventory_detail_writer evt)
    |> subscribe (fun evt -> Pipe.write_without_pushback inventory_list_writer evt)
    |> subscribe (fun evt -> Events.to_string evt |> print_endline)
  in
  let handler = Commandhandler.handler (EventStorage.save ~bus:event_bus) in
  (* Set up tas *)
  let rec loop () =
    Pipe.read cmd_reader
    >>= function
    | `Eof -> Deferred.return ()
    | `Ok cmd ->
      handler cmd;
      loop ()
  in
  ignore (loop ())
;;

(* Start view tasks *)
let () = ignore (Readmodel.inventory_detail_task inventory_detail_reader)
let () = ignore (Readmodel.inventory_list_task inventory_list_reader)

let () =
  let host_and_port =
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port 8000)
      (fun _ reader writer ->
         Deferred.create (fun finished ->
           let rec loop () =
             upon (Reader.read_line reader) (function
               | `Ok x ->
                 let cmd =
                   try
                     Yojson.Safe.from_string x
                     |> Commands.command_of_yojson
                     |> fun x -> Some x
                   with
                   | _ -> None
                 in
                 let _ =
                   match cmd with
                   | Some c ->
                     Pipe.write_without_pushback cmd_writer c;
                     "Command dispatched.\n" |> Writer.write writer
                   | None -> "Command not recognized.\n" |> Writer.write writer
                 in
                 loop ()
               | `Eof ->
                 Ivar.fill finished ();
                 print_endline "Closed connection.")
           in
           loop ()))
  in
  upon host_and_port (fun _ -> ())
;;

let () = never_returns (Scheduler.go ())
