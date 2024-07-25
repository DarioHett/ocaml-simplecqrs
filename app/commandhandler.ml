open Core
open Domain
open Commands
open Async_kernel
open Inventoryitem

let handle_empty_item = function
  | CreateInventoryItem (id, version, name) ->
    (match version with
     | 0 -> Some (InventoryItem.create id name)
     | _ -> None)
  | _ -> None
;;

let handle_existing_item old_item = function
  | CreateInventoryItem _ -> None
  | DeactivateInventoryItem (_, _) -> 
    Some (InventoryItem.deactivate old_item)
  | RemoveItemsFromInventory (_, _, count) -> Some (InventoryItem.remove old_item count)
  | CheckInItemsToInventory (_, _, count) -> Some (InventoryItem.check_in old_item count)
  | RenameInventoryItem (_, _, new_name) ->
    Some (InventoryItem.change_name old_item new_name)
;;

let with_item item cmd =
  let new_item =
    match item with
    | None -> handle_empty_item cmd
    | Some old_item ->
      (match Int.equal (old_item.version) (Commands.version cmd) with
       | true -> handle_existing_item old_item cmd
       | false -> None)
  in
  new_item
;;

let handle repo (cmd : command) =
  let module Repository = (val repo : Repository_intf.S with type t = Inventoryitem.t) in
  let%bind aggregate = cmd |> Commands.id |> Repository.get_by_id in
  let new_aggregate_opt = with_item aggregate cmd in
  match new_aggregate_opt with
  | None -> return ()
  | Some new_aggregate ->
    let expected_version = Commands.version cmd in
    (match%bind Repository.publish new_aggregate expected_version with
     | Ok () -> return ()
     | Error _ -> return ())
;;
