let main ~stdout =
  Eio.Flow.copy_string "Hello, world!\n" stdout

let () =
  Eio_main.run @@ fun (Env env) ->
  main ~stdout:(env#stdout)
