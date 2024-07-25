open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type query = LatestItemEvent of string [@@deriving yojson]

let of_string string =
  try string |> Yojson.Safe.from_string |> query_of_yojson |> Some with
  | _ -> None
;;

let to_string cmd = cmd |> yojson_of_query |> Yojson.Safe.to_string

let%expect_test "LatestItemEvent Json" =
  let query = LatestItemEvent "ItemId" in
  query |> yojson_of_query |> Yojson.Safe.to_string |> print_endline;
  [%expect "[\"LatestItemEvent\",\"ItemId\"]"]
;;
