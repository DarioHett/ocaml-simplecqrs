open Core
open Async
open Simplecqrs

let () = 
  let json = Commands.CreateInventoryItem ("iphone", "001") |> Commands.yojson_of_command |> Yojson.Safe.to_string in
  let handler = (fun _ _ writer -> (Clock.after (sec 1.0)) >>| (fun () -> (Writer.write writer json))) in
  let server = Tcp.Where_to_connect.of_host_and_port { host = "localhost"; port = 8000} in
  ignore(Tcp.with_connection server handler)

let () = never_returns (Scheduler.go ())
