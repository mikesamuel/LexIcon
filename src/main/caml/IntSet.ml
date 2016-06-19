include SetUtil.Make (struct
  type t = int
  let compare a b = Pervasives.compare a b
  let stringer = Stringer.int
end)
