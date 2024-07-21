open Events
open Domain
open Core

type event_desciptor =
  { agg_id : string
  ; version : int
  ; event_data : event
  }

let to_string evtd = sprintf
                "agg_id:%s, version:%d, event:%s"
                evtd.agg_id
                evtd.version
                (Events.yojson_of_event evtd.event_data |> Yojson.Safe.to_string)

module type Repository = sig
  type t

  val save : t -> bus:Eventbus.t -> handle_descriptors:(event_desciptor -> unit) -> unit
  val get_history_by_id : string -> t option
  val show_item_history : string -> unit
  val replay : unit -> event list
end

module type Storage_intf = sig
  val _storage : event_desciptor Base.Queue.t
end

module Make(Storage : Storage_intf) : Repository with type t = InventoryItem.t = struct
  type t = InventoryItem.t
  include Storage

  let event_descriptors id =
    Queue.filter_map _storage ~f:(fun (ed : event_desciptor) ->
      match String.equal ed.agg_id id with
      | true -> Some ed
      | false -> None)
    |> Queue.to_list
  ;;

  let save (aggregate : InventoryItem.t) ~bus ~handle_descriptors =
    let changes = InventoryItem.get_uncommited_changes aggregate
    in
    Eventbus.publish bus changes;
    let descriptors =
      List.mapi changes ~f:(fun i evt ->
        { agg_id = aggregate.id; version = aggregate.version + i + 1; event_data = evt })
    in
    List.iter descriptors ~f:(Queue.enqueue _storage);
    List.iter descriptors ~f:handle_descriptors
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
