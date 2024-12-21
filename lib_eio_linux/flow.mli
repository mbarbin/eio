val of_fd : Eio_unix.Fd.t -> Eio_unix.Flow.t

val stdin : Eio_unix.Source.t
val stdout : Eio_unix.Sink.t
val stderr : Eio_unix.Sink.t

val secure_random : Eio.Flow.source