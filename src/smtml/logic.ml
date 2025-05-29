type t =
  | ALL
  | AUFLIA
  | AUFLIRA
  | AUFNIRA
  | LIA
  | LRA
  | QF_ABV
  | QF_AUFBV
  | QF_AUFLIA
  | QF_AX
  | QF_BV
  | QF_BVFP
  | QF_FP
  | QF_IDL
  | QF_LIA
  | QF_LRA
  | QF_NIA
  | QF_NRA
  | QF_RDL
  | QF_S
  | QF_UF
  | QF_UFBV
  | QF_UFIDL
  | QF_UFLIA
  | QF_UFLRA
  | QF_UFNRA
  | UFLRA
  | UFNIA

let pp fmt = function
  | ALL -> Fmt.string fmt "ALL"
  | AUFLIA -> Fmt.string fmt "AUFLIA"
  | AUFLIRA -> Fmt.string fmt "AUFLIRA"
  | AUFNIRA -> Fmt.string fmt "AUFNIRA"
  | LIA -> Fmt.string fmt "LIA"
  | LRA -> Fmt.string fmt "LRA"
  | QF_ABV -> Fmt.string fmt "QF_ABV"
  | QF_AUFBV -> Fmt.string fmt "QF_AUFBV"
  | QF_AUFLIA -> Fmt.string fmt "QF_AUFLIA"
  | QF_AX -> Fmt.string fmt "QF_AX"
  | QF_BV -> Fmt.string fmt "QF_BV"
  | QF_BVFP -> Fmt.string fmt "QF_BVFP"
  | QF_FP -> Fmt.string fmt "QF_FP"
  | QF_IDL -> Fmt.string fmt "QF_IDL"
  | QF_LIA -> Fmt.string fmt "QF_LIA"
  | QF_LRA -> Fmt.string fmt "QF_LRA"
  | QF_NIA -> Fmt.string fmt "QF_NIA"
  | QF_NRA -> Fmt.string fmt "QF_NRA"
  | QF_RDL -> Fmt.string fmt "QF_RDL"
  | QF_S -> Fmt.string fmt "QF_S"
  | QF_UF -> Fmt.string fmt "QF_UF"
  | QF_UFBV -> Fmt.string fmt "QF_UFBV"
  | QF_UFIDL -> Fmt.string fmt "QF_UFIDL"
  | QF_UFLIA -> Fmt.string fmt "QF_UFLIA"
  | QF_UFLRA -> Fmt.string fmt "QF_UFLRA"
  | QF_UFNRA -> Fmt.string fmt "QF_UFNRA"
  | UFLRA -> Fmt.string fmt "UFLRA"
  | UFNIA -> Fmt.string fmt "UFNIA"

let of_string logic =
  match logic with
  | "ALL" -> Ok ALL
  | "AUFLIA" -> Ok AUFLIA
  | "AUFLIRA" -> Ok AUFLIRA
  | "AUFNIRA" -> Ok AUFNIRA
  | "LIA" -> Ok LIA
  | "LRA" -> Ok LRA
  | "QF_ABV" -> Ok QF_ABV
  | "QF_AUFBV" -> Ok QF_AUFBV
  | "QF_AUFLIA" -> Ok QF_AUFLIA
  | "QF_AX" -> Ok QF_AX
  | "QF_BV" -> Ok QF_BV
  | "QF_BVFP" -> Ok QF_BVFP
  | "QF_FP" -> Ok QF_FP
  | "QF_IDL" -> Ok QF_IDL
  | "QF_LIA" -> Ok QF_LIA
  | "QF_LRA" -> Ok QF_LRA
  | "QF_NIA" -> Ok QF_NIA
  | "QF_NRA" -> Ok QF_NRA
  | "QF_RDL" -> Ok QF_RDL
  | "QF_S" -> Ok QF_S
  | "QF_UF" -> Ok QF_UF
  | "QF_UFBV" -> Ok QF_UFBV
  | "QF_UFIDL" -> Ok QF_UFIDL
  | "QF_UFLIA" -> Ok QF_UFLIA
  | "QF_UFLRA" -> Ok QF_UFLRA
  | "QF_UFNRA" -> Ok QF_UFNRA
  | "UFLRA" -> Ok UFLRA
  | "UFNIA" -> Ok UFNIA
  | _ -> Error (`Msg (Fmt.str "unknown logic %s" logic))
