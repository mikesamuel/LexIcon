include DisableGenericCompare

type t =
  | DevNull
  | Stdout
  | Stderr
  | LogFile of Path.t

let of_flag ?(base=None) s = match s with
  | ""  -> DevNull
  | "-" -> Stdout
  | _   -> match base with
      | None   -> LogFile (Path.of_string s)
      | Some b -> LogFile (Path.join_str b s)

let compare x y = match x, y with
  | DevNull,   DevNull   -> 0
  | DevNull,   _         -> ~-1
  | _,         DevNull   -> 1
  | Stdout,    Stdout    -> 0
  | Stdout,    _         -> ~-1
  | _,         Stdout    -> 1
  | Stderr,    Stderr    -> 0
  | Stderr,    _         -> ~-1
  | _,         Stderr    -> 1
  | LogFile p, LogFile q -> Path.compare p q

let stringer out x = match x with
  | DevNull   -> out "DevNull"
  | Stdout    -> out "Stdout"
  | Stderr    -> out "Stderr"
  | LogFile p -> Stringer.ctor "LogFile" Path.stringer out p

let with_logger x f i = match x with
  | DevNull   -> f ignore i
  | Stdout    -> f (fun s -> output_string stdout s; flush stdout) i
  | Stderr    -> f (fun s -> output_string stderr s; flush stderr) i
  | LogFile p ->
    Path.write_with_channel
      (fun out_channel -> f (fun s -> output_string out_channel s) i)
      p
