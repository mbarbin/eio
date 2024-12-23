let main ~stdout:(Eio.Flow.Sink.T stdout) =
  Eio.Flow.copy_string "Hello, world!\n" stdout

let () =
  Eio_main.run @@ fun env ->
  main ~stdout:(env#stdout)
