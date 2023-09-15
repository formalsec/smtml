module Fresh : sig
  module Make () : Mappings_intf.S with type optimize = Z3.Optimize.optimize
end

include Mappings_intf.S with type optimize = Z3.Optimize.optimize
