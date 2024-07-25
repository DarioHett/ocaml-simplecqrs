open App
open Domain
open Infra.Repositories

open Core
open Async

(* Beware: State of `InMemoryStore` is shared across tests. *)
let%expect_test "Create -> Save -> Get" =
  let open Inventoryitem in
  let itm = InventoryItem.create "ItemId" "ItemName"  in
  let%bind succ = InMemoryInventoryItemRepo.publish itm 0 in
  let () = Or_error.ok_exn succ in
  let%bind itm_opt = InMemoryInventoryItemRepo.get_by_id "ItemId" in
  let itm2 = Option.value_exn itm_opt in
  Inventoryitem.yojson_of_t (itm2) |> Yojson.Safe.to_string |> print_endline;
  let () = [%expect {| {"changes":[],"id":"ItemId","version":0,"name":"ItemName","activated":true} |}] in 
  return ()

let%expect_test "Command -> Handle -> Get" =
  let cmd = Commands.CreateInventoryItem ("ItemId2", 0, "ItemName") in
  let module Application = App.Services.InventoryItemService (InMemoryInventoryItemRepo) in
  let%bind () = Application.handle cmd in
  let%bind itm_opt = InMemoryInventoryItemRepo.get_by_id "ItemId2" in
  let itm = Option.value_exn itm_opt in
  Inventoryitem.yojson_of_t (itm) |> Yojson.Safe.to_string |> print_endline;
  let () = [%expect {| {"changes":[],"id":"ItemId2","version":0,"name":"ItemName","activated":true} |}] in 
  return ()


let%expect_test "Fail: Command -> Handle -> Get" =
  let cmd = Commands.CreateInventoryItem ("ItemId3", 1, "ItemName") in
  let module Application = App.Services.InventoryItemService (InMemoryInventoryItemRepo) in
  let%bind () = Application.handle cmd in
  let%bind itm_opt = InMemoryInventoryItemRepo.get_by_id "ItemId3" in
  Option.is_some itm_opt |> Bool.to_string |> print_endline;
  let () = [%expect {| false |}] in 
  return ()
