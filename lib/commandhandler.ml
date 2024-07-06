open Core
open Domain
open Commands
open Eventstorage

let handler (save : InventoryItem.t -> int -> unit) (msg : command) =
  let fetchitem id (f : InventoryItem.t -> InventoryItem.t) =
    EventStorage.get_history_by_id id |> Option.map ~f
  in
  let item, version =
    match msg with
    | CreateInventoryItem (id, name) ->
      let itm = fetchitem id (fun i -> i) in
      (match itm with
       | Some _ -> None, -1
       | None ->
         let itm = InventoryItem.create id name in
         Some itm, 0)
    | DeactivateInventoryItem (id, version) ->
      let itm = fetchitem id InventoryItem.deactivate in
      itm, version
    | RemoveItemsFromInventory (id, version, count) ->
      let itm = fetchitem id (fun itm -> InventoryItem.remove itm count) in
      itm, version
    | CheckInItemsToInventory (id, version, count) ->
      let itm = fetchitem id (fun itm -> InventoryItem.check_in itm count) in
      itm, version
    | RenameInventoryItem (id, version, new_name) ->
      let itm = fetchitem id (fun itm -> InventoryItem.change_name itm new_name) in
      itm, version
  in
  match item with
  | Some item -> save item version
  | None -> ignore ()
;;
