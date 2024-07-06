open Core
open Events

(* Currently unused type. *)
type create_type =
  | LoadHistory
  | New

module type AggregateRoot = sig
  type t

  val get_uncommited_changes : t -> event list
  val mark_changes_as_committed : t -> t
  val apply_change : t -> event -> t
  val load_from_history : event list -> t
  val init : unit -> t
  val create : string -> string -> t
  val change_name : t -> string -> t
  val remove : t -> int -> t
  val check_in : t -> int -> t
  val deactivate : t -> t
end

module InventoryItem : AggregateRoot = struct
  type t =
    { changes : event list
    ; id : string
    ; version : int
    ; name : string
    ; activated : bool
    }

  let get_uncommited_changes t = t.changes

  let mark_changes_as_committed t =
    { changes = []
    ; id = t.id
    ; version = t.version
    ; name = t.name
    ; activated = t.activated
    }
  ;;

  let apply t = function
    | InventoryItemCreated (id, name) ->
      { changes = t.changes; id; version = 0; name; activated = true }
    | InventoryItemDeactivated _ ->
      { changes = t.changes
      ; id = t.id
      ; version = t.version + 1
      ; name = t.name
      ; activated = false
      }
    | _ ->
      { changes = t.changes
      ; id = t.id
      ; version = t.version + 1
      ; name = t.name
      ; activated = t.activated
      }
  ;;

  let apply_change t evt =
    let new_t = apply t evt in
    { changes = List.append new_t.changes [ evt ]
    ; id = new_t.id
    ; version = new_t.version
    ; name = new_t.name
    ; activated = new_t.activated
    }
  ;;

  let init () = { changes = []; id = ""; version = -1; name = ""; activated = false }
  let load_from_history history = List.fold_left history ~init:(init ()) ~f:apply
  let create id name = InventoryItemCreated (id, name) |> apply_change (init ())

  let change_name t = function
    | "" -> failwith "must give name"
    | name -> InventoryItemRenamed (t.id, name) |> apply_change t
  ;;

  let remove t count =
    match count <= 0 with
    | true -> failwith "cant remove negative count from inventory"
    | false -> ItemsRemovedFromInventory (t.id, count) |> apply_change t
  ;;

  let check_in t count =
    match count <= 0 with
    | true -> failwith "must have a count greater than 0 to add to inventory"
    | _ -> ItemsCheckedInToInventory (t.id, count) |> apply_change t
  ;;

  let deactivate t =
    match t.activated with
    | false -> failwith "already deactivated"
    | true -> InventoryItemDeactivated t.id |> apply_change t
  ;;
end
