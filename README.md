Simple CQRS on OCaml 5.2.0

Based on Greg Young's CQRS: https://github.com/gregoryyoung/m-r/tree/master/SimpleCQRS
and Tuomas Hietanen's F# version: https://github.com/Thorium/SimpleCQRS-FSharp.

Interface is a simple TCP server.

Start server:
```
dune exec -- bin/main.exe
```

In separate shell, send command via netcat:
```
nc localhost 8000 
["CreateInventoryItem", "0001", "iPhone"]
Command dispatched.
["CheckInItemsToInventory", "0001", 1, 10]
Command dispatched.
["CheckInItemsToInventory", 1, 10]  <- Missing `id` field.
Command not recognized.
```

Server shows only dispatched commands:
```
dune exec -- bin/main.exe
Item iPhone created (id:0001)       
Check-in 10 of item (id:0001)
```
