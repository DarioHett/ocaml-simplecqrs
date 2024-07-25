open Ppx_yojson_conv_lib.Yojson_conv.Primitives

(* `agg_id` * `version` * ... *)
type command =
  | CreateInventoryItem of string * int * string
  | DeactivateInventoryItem of string * int
  | RenameInventoryItem of string * int * string
  | CheckInItemsToInventory of string * int * int
  | RemoveItemsFromInventory of string * int * int
[@@deriving yojson]

let id = function
  | CreateInventoryItem (id, _, _) -> id
  | DeactivateInventoryItem (id, _) -> id
  | RenameInventoryItem (id, _, _) -> id
  | CheckInItemsToInventory (id, _, _) -> id
  | RemoveItemsFromInventory (id, _, _) -> id
;;

let version = function
  | CreateInventoryItem (_, version, _) -> version
  | DeactivateInventoryItem (_, version) -> version
  | RenameInventoryItem (_, version, _) -> version
  | CheckInItemsToInventory (_, version, _) -> version
  | RemoveItemsFromInventory (_, version, _) -> version
;;

let of_string string =
  try string |> Yojson.Safe.from_string |> command_of_yojson |> Some with
  | _ -> None
;;

let to_string cmd = cmd |> yojson_of_command |> Yojson.Safe.to_string

let%expect_test "Create -> With_item" =
  let cmd = CreateInventoryItem ("ItemId", 0, "ItemName") in
  cmd |> yojson_of_command |> Yojson.Safe.to_string |> print_endline;
  [%expect "[\"CreateInventoryItem\",\"ItemId\",0,\"ItemName\"]"]
;;
