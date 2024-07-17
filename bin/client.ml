open Core
open Async
open Simplecqrs

let () = 
  let module Client = Cohttp_async.Client in
  let module Body = Cohttp_async.Body in
  let cmds = [Commands.CreateInventoryItem ("iphone2", "002"); Commands.CreateInventoryItem ("iphone1", "001"); Commands.CheckInItemsToInventory ("iphone1", 1, 1)] in
  List.iter ~f:(fun cmd -> 
    let json = cmd |> Commands.yojson_of_command |> Yojson.Safe.to_string in
    let request = Client.post (Uri.of_string "http://localhost:8000") ~body:(`String json) in
    let handler = fun (_, body) -> Body.to_string body >>| print_endline in
    ignore(request >>= handler)) cmds

let () = never_returns (Scheduler.go ())
