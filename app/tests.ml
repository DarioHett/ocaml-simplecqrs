open Core
open Domain
open Async

let%expect_test "Create -> With_item" =
  let cmd = Commands.CreateInventoryItem ("ItemId", 0, "ItemName") in
  let new_item = Commandhandler.with_item None cmd in
  Inventoryitem.yojson_of_t (Option.value_exn new_item)
  |> Yojson.Safe.to_string
  |> print_endline;
  let () =
    [%expect
      {| {"changes":[["InventoryItemCreated","ItemId","ItemName"]],"id":"ItemId","version":0,"name":"ItemName","activated":true} |}]
  in
  return ()
;;

let%expect_test "Create -> Rename -> With_item" =
  let open Inventoryitem in
  let itm = InventoryItem.create "ItemId" "ItemName" in
  let cmd = Commands.RenameInventoryItem ("ItemId", 0, "NewItemName") in
  let new_item = Commandhandler.with_item (Some itm) cmd in
  Inventoryitem.yojson_of_t (Option.value_exn new_item)
  |> Yojson.Safe.to_string
  |> print_endline;
  let () =
    [%expect
      {| {"changes":[["InventoryItemCreated","ItemId","ItemName"],["InventoryItemRenamed","ItemId","NewItemName"]],"id":"ItemId","version":1,"name":"NewItemName","activated":true} |}]
  in
  return ()
;;

let%expect_test "Wrong version: Create -> Rename -> With_item" =
  let open Inventoryitem in
  let itm = InventoryItem.create "ItemId" "ItemName" in
  let cmd = Commands.RenameInventoryItem ("ItemId", 1, "NewItemName") in
  let new_item = Commandhandler.with_item (Some itm) cmd in
  Option.is_none new_item |> Bool.to_string |> print_endline;
  let () = [%expect {| true |}] in
  return ()
;;

let%expect_test "Correct version: Create -> Rename -> With_item" =
  let open Inventoryitem in
  let itm = InventoryItem.create "ItemId" "ItemName" in
  let cmd = Commands.RenameInventoryItem ("ItemId", 0, "NewItemName") in
  let new_item = Commandhandler.with_item (Some itm) cmd in
  Option.is_none new_item |> Bool.to_string |> print_endline;
  let () = [%expect {| false |}] in
  return ()
;;
