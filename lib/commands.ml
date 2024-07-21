open Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* `agg_id` * `version` * ... *)
type command =
  | CreateInventoryItem of string * string
  | DeactivateInventoryItem of string * int
  | RenameInventoryItem of string * int * string
  | CheckInItemsToInventory of string * int * int
  | RemoveItemsFromInventory of string * int * int
[@@deriving yojson]
