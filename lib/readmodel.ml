open Core
open Events

type inventory_item_details_dto =
  { id : string
  ; name : string
  ; count : int
  }

let inventory_item_detals_dto_to_string t =
  sprintf "Item Details: %s %s %d" t.id t.name t.count

type inventory_item_list_dto =
  { id : string
  ; name : string
  }
let inventory_item_list_dto_to_string t =
  sprintf "List Item: %s %s" t.id t.name

type t =
  { inventory_items : (string, inventory_item_list_dto) Hashtbl.t
  ; inventory_item_details : (string, inventory_item_details_dto) Hashtbl.t
  }

let in_memory_database : t =
  { inventory_items = Hashtbl.create (module String)
  ; inventory_item_details = Hashtbl.create (module String)
  }
;;

let register_handlers_inventory_list_view () =
  let db_items = in_memory_database.inventory_items in
  let known_events e =
    match e with
    | InventoryItemCreated (id, name) -> 
      let found = { id; name } in 
      inventory_item_list_dto_to_string found |> print_endline;
      Hashtbl.set db_items ~key:id ~data:found
    | InventoryItemDeactivated id -> Hashtbl.remove db_items id
    | InventoryItemRenamed (id, new_name) ->
      let found = Hashtbl.find_exn db_items id in
      inventory_item_list_dto_to_string found |> print_endline;
      Hashtbl.set db_items ~key:id ~data:{ id = found.id; name = new_name }
    | _ -> ignore ()
  in
  known_events
;;


let register_handlers_inventory_detail_view () =
  let db_items = in_memory_database.inventory_item_details in
  let known_events e =
    match e with
    | InventoryItemCreated (id, name) ->
      let found = { id; name; count = 0 } in
        inventory_item_detals_dto_to_string found |> print_endline;
      Hashtbl.set db_items ~key:id ~data:found
    | InventoryItemDeactivated id -> Hashtbl.remove db_items id
    | InventoryItemRenamed (id, new_name) ->
      let found = Hashtbl.find db_items id in
      (begin match found with
       | Some found ->
          let updated = { id = found.id; name = new_name; count = found.count } in
        inventory_item_detals_dto_to_string updated |> print_endline;
         Hashtbl.set
           db_items
           ~key:id
           ~data:updated
       | None -> ignore ()
       end)
    | ItemsCheckedInToInventory (id, count) ->
      let found = Hashtbl.find db_items id in
      (match found with
       | Some found ->
          let updated = { id = found.id; name = found.name; count = found.count + count } in
        inventory_item_detals_dto_to_string updated |> print_endline;
         Hashtbl.set
           db_items
           ~key:id
           ~data:updated
       | None -> ignore ())
    | ItemsRemovedFromInventory (id, count) ->
      let found = Hashtbl.find db_items id in
      (match found with
       | Some found ->
        let updated = { id = found.id; name = found.name; count = found.count - count } in
        inventory_item_detals_dto_to_string updated |> print_endline;
         Hashtbl.set
           db_items
           ~key:id
           ~data:updated
       | None -> ignore ())
  in
  known_events
;;

