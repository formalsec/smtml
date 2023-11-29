type _ param =
  | Model : bool param
  | Unsat_core : bool param
  | Ematching : bool param

type t =
  { model : bool
  ; unsat_core : bool
  ; ematching : bool
  }

let default_model = true
let default_unsat_core = false
let default_ematching = true

let default_value (type a) (param : a param) : a =
  match param with
  | Model -> default_model
  | Unsat_core -> default_unsat_core
  | Ematching -> default_ematching

let default () =
  { model = default_model
  ; unsat_core = default_unsat_core
  ; ematching = default_ematching
  }

let set (type a) (params : t) (param : a param) (value : a) : t =
  match param with
  | Model -> { params with model = value }
  | Unsat_core -> { params with unsat_core = value }
  | Ematching -> { params with ematching = value }

let ( & ) (type a) (params : t) ((param, value) : a param * a) : t =
  set params param value

let get (type a) (params : t) (param : a param) : a =
  match param with
  | Model -> params.model
  | Unsat_core -> params.unsat_core
  | Ematching -> params.ematching
