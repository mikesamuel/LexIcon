include DisableGenericCompare

type t = Scope.F.Idx.t * int list

let compare_by_program_position (f, is) (g, js) =
  Cmp.chain (Scope.F.Idx.compare f g)
    (lazy (ListUtil.compare_rev compare is js))

let compare (f, is) (g, js) =
  Cmp.chain (Scope.F.Idx.compare f g) (lazy (ListUtil.compare compare is js))

let make_stringer fn_idx_stringer =
  Stringer.tup2 fn_idx_stringer (Stringer.list Stringer.int)

let stringer = make_stringer Scope.F.Idx.stringer

let child (f, ls) i = (f, i::ls)
let root f = (f, [])
let make f ls = (f, ls)
let fn_of = fst

type saddr = t
module SAddr = struct
  type t = saddr
  let compare = compare
  let stringer = stringer
end

module Map = MapUtil.Make (SAddr)
module Set = SetUtil.Make (SAddr)
