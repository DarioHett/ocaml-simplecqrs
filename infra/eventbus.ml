open App
open Core
open Domain
open Async

module InMemoryEventbus : Eventbus_intf.S = struct
  type t = { subscribers : (Events.event -> unit Deferred.t) list }

  let init () = { subscribers = [] }

  let subscribe t ~f:subscriber =
    { subscribers = List.append t.subscribers [ subscriber ] }
  ;;

  let _reader, _writer = Pipe.create ()
  let publish event = Pipe.write _writer event

  let run t () =
    let rec loop () =
      let%bind event = Pipe.read _reader in
      match event with
      | `Ok e ->
        let notification_tasks = List.map t.subscribers ~f:(fun f -> f e) in
        Deferred.all notification_tasks >>= fun _ -> loop ()
      | `Eof -> return ()
    in
    ignore (loop ())
  ;;
end

let%expect_test "InMemoryEventbus" =
  let subscribable e =
    e |> Events.to_string |> print_endline;
    return ()
  in
  let subscribers = InMemoryEventbus.init () in
  let subscribers = InMemoryEventbus.subscribe subscribers ~f:subscribable in
  let event = Domain.Events.InventoryItemCreated ("ItemId", "ItemName") in
  ignore (InMemoryEventbus.run subscribers ());
  let%bind () = InMemoryEventbus.publish event in
  let () = [%expect "Item ItemName created (id:ItemId)"] in
  return ()
;;
