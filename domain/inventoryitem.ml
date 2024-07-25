open Core
open Events
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t =
  { changes : event list
  ; id : string
  ; version : int
  ; name : string
  ; activated : bool
  }
[@@deriving yojson]

module InventoryItem : Inventoryitem_intf.S with type t := t = struct
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
    | InventoryItemRenamed (_, name) ->
      { changes = t.changes
      ; id = t.id
      ; version = t.version + 1
      ; name
      ; activated = t.activated
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
  let load history = List.fold_left history ~init:(init ()) ~f:apply

  let%expect_test _ =
    let itm = load [] |> yojson_of_t |> Yojson.Safe.to_string in
    print_endline itm;
    [%expect {|
    {"changes":[],"id":"","version":-1,"name":"","activated":false}
  |}]
  ;;

  let%expect_test _ =
    let evt = InventoryItemCreated ("ItemId", "ItemName") in
    let itm = load [ evt ] |> yojson_of_t |> Yojson.Safe.to_string in
    print_endline itm;
    [%expect
      {|
    {"changes":[],"id":"ItemId","version":0,"name":"ItemName","activated":true}
  |}]
  ;;

  let create id name = InventoryItemCreated (id, name) |> apply_change (init ())

  let%expect_test _ =
    let itm = create "ItemId" "ItemName" |> yojson_of_t |> Yojson.Safe.to_string in
    print_endline itm;
    [%expect
      {|
    {"changes":[["InventoryItemCreated","ItemId","ItemName"]],"id":"ItemId","version":0,"name":"ItemName","activated":true}
  |}]
  ;;

  let change_name t = function
    | "" -> t
    | name -> InventoryItemRenamed (t.id, name) |> apply_change t
  ;;

  let remove t count =
    match count <= 0 with
    | true -> t
    | false -> ItemsRemovedFromInventory (t.id, count) |> apply_change t
  ;;

  let check_in t count =
    match count <= 0 with
    | true -> t
    | _ -> ItemsCheckedInToInventory (t.id, count) |> apply_change t
  ;;

  let deactivate t =
    match t.activated with
    | false -> t
    | true -> InventoryItemDeactivated t.id |> apply_change t
  ;;

  let%expect_test _ =
    let itm = create "ItemId" "ItemName" |> deactivate in
    let itm = itm |> yojson_of_t |> Yojson.Safe.to_string in
    print_endline itm;
    [%expect
      {| {"changes":[["InventoryItemCreated","ItemId","ItemName"],["InventoryItemDeactivated","ItemId"]],"id":"ItemId","version":1,"name":"ItemName","activated":false} |}]
  ;;

  let%expect_test _ =
    let itm = create "ItemId" "ItemName" |> mark_changes_as_committed |> deactivate in
    let itm = itm |> yojson_of_t |> Yojson.Safe.to_string in
    print_endline itm;
    [%expect
      {| {"changes":[["InventoryItemDeactivated","ItemId"]],"id":"ItemId","version":1,"name":"ItemName","activated":false} |}]
  ;;

  let%expect_test _ =
    let history = [ InventoryItemCreated ("ItemId", "ItemName") ] in
    let itm = load history |> deactivate |> yojson_of_t |> Yojson.Safe.to_string in
    print_endline itm;
    [%expect
      {| {"changes":[["InventoryItemDeactivated","ItemId"]],"id":"ItemId","version":1,"name":"ItemName","activated":false} |}]
  ;;
end
