## Setting up the environment

```ocaml
# #require "eio_main";;
```

```ocaml
open Eio.Std

let run fn =
  Eio_main.run @@ fun _ ->
  fn ()
```

# Testing path utils

```ocaml
# let test ~cwd path =
    let pp_path fmt (_, p) = Format.printf "%S" p in
    let pp_split fmt = function
      | None -> Format.pp_print_string fmt "None"
      | Some (dir, basename) ->
        Format.fprintf fmt "Some (%a, %S)" pp_path dir basename
    in
    let path = Eio.Path.(cwd / path) in
    Format.printf "-----\n";
    Format.printf "path:     %a\n" pp_path path;
    Format.printf "split:    %a\n" pp_split (Eio.Path.split path);
    Format.printf "basename: %S\n" (Eio.Path.basename path);
    Format.printf "dirname:  %S\n" (Eio.Path.dirname path);
val test : cwd:[> Eio.Fs.dir_ty ] Eio.Path.t -> string -> unit = <fun>
```

```ocaml
# Eio_main.run @@ fun env ->
  let cwd = Eio.Stdenv.cwd env in
  List.iter (fun path -> test ~cwd path)
   [ "foo/bar"
   ; "/foo/bar"
   ; "/foo/bar/baz"
   ; "/foo/bar//baz/"
   ; "bar"
   ; "."
   ; ""
   ; "/"
   ; ""
   ; "./."
   ; "./"
   ; "/."
   ; "./.foo"
   ; ".foo"
   ; "/bar"
   ; "./foo/bar"
   ; "foo/bar//"
   ; "///"
   ];
  Format.pp_print_flush Format.std_formatter ()
-----
path:     "foo/bar"
split:    Some ("foo", "bar")
basename: "bar"
dirname:  "foo"
-----
path:     "/foo/bar"
split:    Some ("/foo", "bar")
basename: "bar"
dirname:  "/foo"
-----
path:     "/foo/bar/baz"
split:    Some ("/foo/bar", "baz")
basename: "baz"
dirname:  "/foo/bar"
-----
path:     "/foo/bar//baz/"
split:    Some ("/foo/bar", "baz")
basename: "baz"
dirname:  "/foo/bar"
-----
path:     "bar"
split:    Some ("", "bar")
basename: "bar"
dirname:  "."
-----
path:     "."
split:    Some ("", ".")
basename: "."
dirname:  "."
-----
path:     ""
split:    None
basename: ""
dirname:  "."
-----
path:     "/"
split:    None
basename: "/"
dirname:  "/"
-----
path:     ""
split:    None
basename: ""
dirname:  "."
-----
path:     "./."
split:    Some (".", ".")
basename: "."
dirname:  "."
-----
path:     "./"
split:    Some ("", ".")
basename: "."
dirname:  "."
-----
path:     "/."
split:    Some ("/", ".")
basename: "."
dirname:  "/"
-----
path:     "./.foo"
split:    Some (".", ".foo")
basename: ".foo"
dirname:  "."
-----
path:     ".foo"
split:    Some ("", ".foo")
basename: ".foo"
dirname:  "."
-----
path:     "/bar"
split:    Some ("/", "bar")
basename: "bar"
dirname:  "/"
-----
path:     "./foo/bar"
split:    Some ("./foo", "bar")
basename: "bar"
dirname:  "./foo"
-----
path:     "foo/bar//"
split:    Some ("foo", "bar")
basename: "bar"
dirname:  "foo"
-----
path:     "///"
split:    None
basename: "/"
dirname:  "/"
- : unit = ()
```
