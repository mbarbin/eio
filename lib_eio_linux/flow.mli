val of_fd : Eio_unix.Fd.t -> Eio_unix.Flow.r

val stdin : Eio_unix.Source.r
val stdout : Eio_unix.Sink.r
val stderr : Eio_unix.Sink.r

val secure_random : Eio.Flow.Source.r