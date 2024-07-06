open Core
open Async
open Events

type inventory_item_details_dto =
  { id : string
  ; name : string
  ; count : int
  }

type inventory_item_list_dto =
  { id : string
  ; name : string
  }

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
    | InventoryItemCreated (id, name) -> Hashtbl.set db_items ~key:id ~data:{ id; name }
    | InventoryItemDeactivated id -> Hashtbl.remove db_items id
    | InventoryItemRenamed (id, new_name) ->
      let found = Hashtbl.find_exn db_items id in
      Hashtbl.set db_items ~key:id ~data:{ id = found.id; name = new_name }
    | _ -> ignore ()
  in
  known_events
;;

let inventory_list_task reader =
  let handler = register_handlers_inventory_list_view () in
  let rec loop () =
    Pipe.read reader
    >>= function
    | `Eof ->
      print_endline "Stopped inventory_list_task.";
      Deferred.return ()
    | `Ok evt ->
      handler evt;
      loop ()
  in
  loop ()
;;

let register_handlers_inventory_detail_view () =
  let db_items = in_memory_database.inventory_item_details in
  let known_events e =
    match e with
    | InventoryItemCreated (id, name) ->
      Hashtbl.set db_items ~key:id ~data:{ id; name; count = 0 }
    | InventoryItemDeactivated id -> Hashtbl.remove db_items id
    | InventoryItemRenamed (id, new_name) ->
      let found = Hashtbl.find db_items id in
      (match found with
       | Some found ->
         Hashtbl.set
           db_items
           ~key:id
           ~data:{ id = found.id; name = new_name; count = found.count }
       | None -> ignore ())
    | ItemsCheckedInToInventory (id, count) ->
      let found = Hashtbl.find db_items id in
      (match found with
       | Some found ->
         Hashtbl.set
           db_items
           ~key:id
           ~data:{ id = found.id; name = found.name; count = found.count + count }
       | None -> ignore ())
    | ItemsRemovedFromInventory (id, count) ->
      let found = Hashtbl.find db_items id in
      (match found with
       | Some found ->
         Hashtbl.set
           db_items
           ~key:id
           ~data:{ id = found.id; name = found.name; count = found.count - count }
       | None -> ignore ())
  in
  known_events
;;

let inventory_detail_task reader =
  let handler = register_handlers_inventory_detail_view () in
  let rec loop () =
    Pipe.read reader
    >>= function
    | `Eof ->
      print_endline "Stopped inventory_detail_task.";
      Deferred.return ()
    | `Ok evt ->
      handler evt;
      loop ()
  in
  loop ()
;;
