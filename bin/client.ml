open Core
open Async
open Simplecqrs

let () =
  let module Client = Cohttp_async.Client in
  let module Body = Cohttp_async.Body in
  let f cmd = 
      let json = cmd |> Commands.yojson_of_command |> Yojson.Safe.to_string in
      let request =
        Client.post (Uri.of_string "http://localhost:8000") ~body:(`String json)
      in
      let handler (_, body) = Body.to_string body >>| print_endline in
      request >>= handler in
  let cmds =
    [Commands.CreateInventoryItem ("001", "iphone1");
     Commands.CreateInventoryItem ("002", "iphone2")
    ; Commands.CheckInItemsToInventory ("001", 1, 1)
    ; Commands.CheckInItemsToInventory ("002", 1, 1)
    ; Commands.CheckInItemsToInventory ("002", 2, 1)
    ; Commands.RenameInventoryItem ("001", 2, "Samsung1")
    ; Commands.CreateInventoryItem ("003", "iphone3")
    ] in
  let rec process cmds =
    match cmds with
    | [] -> return ()
    | hd::tl -> f hd >>= fun () -> process tl in
  ignore(process cmds)
;;

let () = never_returns (Scheduler.go ())
