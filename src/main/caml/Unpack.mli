(** Operators for unpacking JSON configurations to domain values. *)

type 'a t
(** A configuration parser which can be stringified to provide help text or
    readable error messages. *)

exception Missing_config
  of string list * (Stringer.sink -> unit) * string list
(** [Missing_config (prop_names, stringify, missing)] is raised when
    following [prop_names] in reverse leads to a portion of the configuration
    which should have had properties named [missing] to match
    [Stringer.s (fun out _ -> stringify out) ()] but which does not. *)

exception Illegal_config
  of string list * (Stringer.sink -> unit) * Encodable.t
(** [Illegal_config (prop_names, stringify, e)] is raised when
    following [prop_names] in reverse leads to [e] which should have matched
    [... stringify ...] but which does not. *)

exception Unused_config
  of string list * (Stringer.sink -> unit) * string list
(** [Unused_config (prop_names, stringify, unused)] is raised when
    following [prop_names] in reverse leads to a portion of the configuration
    that matched but without using properties in [unused]. *)

type 'a prop

val obj     : 'a prop list -> (unit -> 'a) -> 'a t
(** [obj props ctor] is an unpacker that matches string relations, and
    delegates handling to property handlers.

    When unpacking, it tries to match property unpackers by name, and if
    all succeed and no properties are left over, then it uses [ctor ()] to
    construct an output value which it passes to the setter to incorporate
    information about the value.
*)

val prop     :
     key:string  -> doc:string -> ?default:(('a * ('a Stringer.t)) option)
  -> 'a t -> ('a -> 'o -> 'o) -> 'o prop
(** [prop key doc default value_unpacker setter] is an unpacker for a named
    property within an object.

    [key] is the name of the property to which it is appropriate to apply
    this unpacker.

    [doc] describes the semantics of the property.

    If [default] is [Some (x, x_stringer)] then [x] is an appropriate value
    when there is no property named [key] in the object.

    [value_unpacker] unpacks a value.

    [setter unpacked_value result] incorporates the unpacked value into the
    output often via [\{ result with x = unpacked_value \}].
 *)

val any      : 'a option t list -> 'a t -> 'a t
(** [any options fallback] tries unpacking using [options] in order, and
    stops with the first one that succeeds, trying fallback if none of the
    optional unpackers succeed. *)

val many     : 'a t -> 'a list t
(** [many unpacker] unpacks an array by applying [unpacker] to each value. *)

val manifold : 'a t -> ('b -> 'a -> 'b) -> ('b -> 'c) -> 'b -> 'c t
(** [manifold unpacker fold finish empty] is like [many unpacker] but uses
    [fold] to fold elements into a collection.

    [empty] is the initial value passed to fold, and [finish] is called with
    the result of the last fold to produce the output. *)

val relation : 'a t -> 'b t -> ('a -> 'b -> 'c -> 'c) -> 'c -> 'c t
(** [relation key_unpacker value_unpacker add empty] unpacks objects by
    matching keys and values using the corresponding unpackers, and then
    calling [add] to fold them into the result. *)

val force    : 'a option t -> 'a t

val option   : 'a t -> 'a option t

val string   : string option t
val float    : float  option t
val int      : int    option t
val null     : unit   option t
val bool     : bool   option t

val literal : Encodable.t -> 'a -> 'a option t
(** [literal x y] matches a specific value [x], and maps it to [y], rejecting
    all other inputs.  Useful with {!any} to handle symbolic names. *)

val unpack : 'a t -> Encodable.t -> 'a

val filter : ('a -> bool) -> 'a t -> 'a t
(** [filter up p] is an unpacker that matches as [up] does but raises
    {!Illegal_config} when [p unpacked_result] is false. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f up] is an unpacker that unpacks the same inputs as [up] but
    yields [f x] when [up] would yield [x]. *)

val with_stringify : (Stringer.sink -> unit) -> 'a t -> 'a t
(** The input unpacker but which stringifyies using the given stringifier. *)

val stringer : 'a t Stringer.t
