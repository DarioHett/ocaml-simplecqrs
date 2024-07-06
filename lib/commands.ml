open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type command =
  | CreateInventoryItem of string * string
  | DeactivateInventoryItem of string * int
  | RenameInventoryItem of string * int * string
  | CheckInItemsToInventory of string * int * int
  | RemoveItemsFromInventory of string * int * int
[@@deriving yojson]
