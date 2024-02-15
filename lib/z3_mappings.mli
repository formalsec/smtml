module Fresh : sig
  module Make () : Mappings_intf.S
end

include Mappings_intf.S (** @inline *)
