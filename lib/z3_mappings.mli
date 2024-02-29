module Fresh : sig
  module Make () : Mappings_intf.S
end

(** @inline *)
include Mappings_intf.S
