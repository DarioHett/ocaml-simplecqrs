open App
open Async
open Core
open Domain

module InMemoryInventoryItemRepo : Repository_intf.S with type t = Inventoryitem.t =
struct
  type t = Inventoryitem.t

  type event_desciptor =
    { agg_id : string
    ; version : int
    ; event_data : Events.event
    }

  let _storage : event_desciptor Base.Queue.t = Queue.create ()
  (* let _pipes : (Events.event Pipe.Reader.t * Events.event Pipe.Writer.t) = Pipe.create () *)

  let publish (item : t) (expected_version : int) =
    let open Inventoryitem in
    let last_event_descriptor =
      _storage |> Queue.filter ~f:(fun ed -> String.equal ed.agg_id item.id) |> Queue.last
    in
    let events_opt =
      match last_event_descriptor with
      | None ->
        (match expected_version with
         | 0 -> Some (InventoryItem.get_uncommited_changes item)
         | _ -> None)
      | Some ed ->
        (match Int.equal ed.version expected_version with
         | true -> Some (InventoryItem.get_uncommited_changes item)
         | false -> None)
    in
    match events_opt with
    | None -> Deferred.return (Or_error.error_string "Version mismatch")
    | Some events ->
      let eds =
        List.mapi events ~f:(fun i e ->
          { agg_id = item.id; version = item.version + i + 1; event_data = e })
      in
      Queue.enqueue_all _storage eds;
      Deferred.return (Ok ())
  ;;

  let get_by_id id =
    let open Inventoryitem in
    let events =
      _storage
      |> Queue.filter ~f:(fun ed -> String.equal ed.agg_id id)
      |> Queue.map ~f:(fun ed -> ed.event_data)
      |> Queue.to_list
    in
    match events with
    | [] -> Deferred.return None
    | evts -> Deferred.return (Some (InventoryItem.load evts))
  ;;
end

module InMemoryInventoryItemRepoWithEventbus (Eventbus : Eventbus_intf.S) :
  Repository_intf.S with type t = Inventoryitem.t = struct
  type t = Inventoryitem.t

  type event_desciptor =
    { agg_id : string
    ; version : int
    ; event_data : Events.event
    }

  let _storage : event_desciptor Base.Queue.t = Queue.create ()

  let publish (item : t) (expected_version : int) =
    let open Inventoryitem in
    let last_event_descriptor =
      _storage |> Queue.filter ~f:(fun ed -> String.equal ed.agg_id item.id) |> Queue.last
    in
    let events_opt =
      match last_event_descriptor with
      | None ->
        (match expected_version with
         | 0 -> Some (InventoryItem.get_uncommited_changes item)
         | _ -> None)
      | Some ed ->
        (match Int.equal (ed.version-1) expected_version with
         | true -> Some (InventoryItem.get_uncommited_changes item)
         | false -> None)
    in
    match events_opt with
    | None -> Deferred.return (Or_error.error_string "Version mismatch")
    | Some events ->
      let eds =
        List.mapi events ~f:(fun i e ->
          { agg_id = item.id; version = item.version + i + 1; event_data = e })
      in
      Queue.enqueue_all _storage eds;
      let publication_tasks = List.map events ~f:Eventbus.publish in
      Deferred.all publication_tasks >>| fun _ -> Ok ()
  ;;

  let get_by_id id =
    let open Inventoryitem in
    let events =
      _storage
      |> Queue.filter ~f:(fun ed -> String.equal ed.agg_id id)
      |> Queue.map ~f:(fun ed -> ed.event_data)
      |> Queue.to_list
    in
    match events with
    | [] -> Deferred.return None
    | evts -> Deferred.return (Some (InventoryItem.load evts))
  ;;
end

(*
   module type PostgresConn_intf = sig
  val conn : Postgres_async.t
end

module PostgresStore (PostgresConn : PostgresConn_intf) :
  Repository_intf.S with type t = Inventoryitem.t = struct
  include PostgresConn

  type t = Inventoryitem.t

  type event_desciptor =
    { agg_id : string
    ; version : int
    ; event_data : Events.event
    }

  let _storage : Events.event Base.Queue.t = Queue.create ()
  (* let _pipes : (Events.event Pipe.Reader.t * Events.event Pipe.Writer.t) = Pipe.create () *)

  let publish (item : t) =
    let open Inventoryitem in
    let events =
      _storage
      |> Queue.filter ~f:(fun itm -> String.equal (Events.id itm) item.id)
      |> Queue.to_list
    in
    let old_item = InventoryItem.load events in
    match Int.equal (old_item.version + 1) item.version with
    | true ->
      Queue.enqueue_all _storage (InventoryItem.get_uncommited_changes item);
      Deferred.return (Ok ())
    | false -> Deferred.return (Or_error.error_string "Version mismatch")
  ;;

  let get_by_id id =
    let open Inventoryitem in
    let events =
      _storage
      |> Queue.filter ~f:(fun itm -> String.equal (Events.id itm) id)
      |> Queue.to_list
    in
    match events with
    | [] -> Deferred.return None
    | evts -> Deferred.return (Some (InventoryItem.load evts))
  ;;
end *)
