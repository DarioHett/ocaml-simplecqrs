open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type event =
  | InventoryItemCreated of string * string
  | InventoryItemDeactivated of string
  | InventoryItemRenamed of string * string
  | ItemsCheckedInToInventory of string * int
  | ItemsRemovedFromInventory of string * int
[@@deriving yojson]

let id = function
  | InventoryItemCreated (id, _) -> id
  | InventoryItemDeactivated id -> id
  | InventoryItemRenamed (id, _) -> id
  | ItemsCheckedInToInventory (id, _) -> id
  | ItemsRemovedFromInventory (id, _) -> id
;;

let to_string = function
  | InventoryItemCreated (i, n) -> "Item " ^ n ^ " created (id:" ^ i ^ ")"
  | InventoryItemDeactivated i -> "Item deactivated (id:" ^ i ^ ")"
  | InventoryItemRenamed (i, n) -> "Item renamed to " ^ n ^ " (id:" ^ i ^ ")"
  | ItemsCheckedInToInventory (i, c) ->
    "Check-in " ^ Int.to_string c ^ " of item (id:" ^ i ^ ")"
  | ItemsRemovedFromInventory (i, c) ->
    "Removed " ^ Int.to_string c ^ " of item (id:" ^ i ^ ")"
;;
