open Events
open Domain
open Core

type event_desciptor =
  { agg_id : string
  ; version : int
  ; event_data : event
  }

module type Repository = sig
  type t

  val save : t -> int -> bus:Eventbus.t -> unit
  val get_history_by_id : string -> t option
  val show_item_history : string -> unit
  val replay : unit -> event list
end

module EventStorage : Repository = struct
  type t = InventoryItem.t

  let _storage = Queue.create ()

  let event_descriptors id =
    Queue.filter_map _storage ~f:(fun (ed : event_desciptor) ->
      match String.equal (Events.id ed.event_data) id with
      | true -> Some ed
      | false -> None)
    |> Queue.to_list
  ;;

  let save (aggregate : InventoryItem.t) expected_version ~bus =
    let original_data = event_descriptors aggregate.id in
    let changes =
      match List.rev original_data with
      | [] -> InventoryItem.get_uncommited_changes aggregate
      | last_item_descriptor :: _ ->
        (match Int.equal last_item_descriptor.version expected_version with
         | true -> InventoryItem.get_uncommited_changes aggregate
         | false -> [])
    in
    Eventbus.publish bus changes;
    let descriptors =
      List.mapi changes ~f:(fun i evt ->
        { agg_id = aggregate.id; version = i + 1; event_data = evt })
    in
    List.iter descriptors ~f:(Queue.enqueue _storage)
  ;;

  let get_history_by_id id =
    let history = event_descriptors id |> List.map ~f:(fun ed -> ed.event_data) in
    match history with
    | [] -> None
    | _ -> Some (InventoryItem.load_from_history history)
  ;;

  let show_item_history id =
    let history = event_descriptors id |> List.map ~f:(fun ed -> ed.event_data) in
    match history with
    | [] -> print_endline ""
    | _ -> history |> List.iter ~f:(fun evt -> evt |> Events.to_string |> print_endline)
  ;;

  let replay () = _storage |> Queue.map ~f:(fun ed -> ed.event_data) |> Queue.to_list
end

include EventStorage
