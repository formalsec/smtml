%{
open Smtlib
%}
%token LPAR
%token RPAR
(* %token BINARY *)
%token DECIMAL
(* %token HEXADECIMAL *)
%token NUMERAL
%token STRING
%token HOLE
%token ANNOT
%token AS
%token LET
%token EXISTS
%token FORALL
%token MATCH
%token PAR
%token NOT
%token THEORY
%token SORTS
%token FUNS
%token SORTS_DESCRIPTION
%token FUNS_DESCRIPTION
%token DEFINITION
%token VALUES
%token NOTES
%token LOGIC
%token THEORIES
%token LANGUAGE
%token EXTENSIONS
%token ASSERT
%token CHECK_SAT
%token CHECK_SAT_ASSUMING
%token DECLARE_CONST
%token DECLARE_DATATYPE
%token DECLARE_DATATYPES
%token DECLARE_FUN
%token DECLARE_SORT
%token DEFINE_FUN
%token DEFINE_FUN_REC
%token DEFINE_FUNS_REC
%token DEFINE_SORT
%token ECHO
%token EXIT
%token GET_ASSERTIONS
%token GET_ASSIGNMENT
%token GET_INFO
%token GET_MODEL
%token GET_OPTION
%token GET_PROOF
%token GET_UNSAT_ASSUMPTIONS
%token GET_UNSAT_CORE
%token GET_VALUE
%token POP
%token PUSH
%token RESET
%token RESET_ASSERTIONS
%token SET_INFO
%token SET_LOGIC
%token SET_OPTION
%token EOF

%token <string> NUM
%token <string> DEC
%token <string> HEX
%token <string> BIN
%token <string> STR
%token <string> SYMBOL
%token <string> KEYWORD

%start <Smtlib.script> script
%start <unit> theory_decl
%start <unit> logic
%%

let script := commands = command*; EOF; { commands }

let symbol := x = SYMBOL; { x }

(* S-expressions *)
let s_expr :=
  | _ = spec_constant; { }
  | _ = symbol; { }
  | _ = KEYWORD; { }
  | LPAR; _ = s_expr*; RPAR; { }

let spec_constant :=
  | x = NUM; { Num (int_of_string x) }
  | x = DEC; { Dec (float_of_string x) }
  | x = HEX; { Hex x }
  | x = BIN; { Bin x }
  | x = STR; { Str x }

(* Identifiers *)
let index :=
  | x = NUM;    { Num_idx (int_of_string x) }
  | x = symbol; { Sym_idx x }

let identifier :=
  | ~ = symbol;
    { Sym_id symbol }
  | LPAR; HOLE; ~ = symbol; indices = index+; RPAR;
    { Hole_id (symbol, indices) }

(* Sorts *)
let sort :=
  | ~ = identifier; { Id_sort identifier }
  | LPAR; ~ = identifier; sorts = sort+; RPAR; { Comp_sort (identifier, sorts) }

(* Attributes *)
let attribute_value :=
  | _ = spec_constant; { }
  | _ = symbol; { }
  | LPAR; _ = s_expr*; RPAR; { }

let attribute :=
  | _ = KEYWORD; { }
  | _ = KEYWORD; _ = attribute_value; { }

(* Terms *)
let qual_identifier :=
  | ~ = identifier; { Id_qual identifier }
  | LPAR; AS; ~ = identifier; ~ = sort; RPAR; { As_qual (identifier, sort) }

let var_binding :=
  | LPAR; ~ = symbol; ~ = term; RPAR; { (symbol, term) }

let sorted_var :=
  | LPAR; ~ = symbol; ~ = sort; RPAR; { (symbol, sort) }

let pattern :=
  | _ = symbol; { }
  | LPAR; _ = symbol; _ = symbol+; RPAR; { }

let match_case :=
  | LPAR; _ = pattern; _ = term; RPAR; { }

let term :=
  | ~ = spec_constant;
    { Const spec_constant }
  | ~ = qual_identifier;
    { Id qual_identifier }
  | LPAR; ~ = qual_identifier; term_list = term+; RPAR;
    { App (qual_identifier, term_list) }
  | LPAR; LET; LPAR; binds = var_binding+; RPAR; ~ = term; RPAR;
    { Let (binds, term) }
  | LPAR; FORALL; LPAR; vars = sorted_var+; RPAR; ~ = term; RPAR;
    { Forall (vars, term) }
  | LPAR; EXISTS; LPAR; vars = sorted_var+; RPAR; ~ = term; RPAR;
    { Exists (vars, term) }
  | LPAR; MATCH; term; LPAR; match_case+; RPAR; RPAR;
    { assert false }
  | LPAR; ANNOT; term; attribute+; RPAR;
    { assert false }

(* Theories *)
let sort_symbol_decl :=
  | LPAR; identifier; NUM; attribute*; RPAR; { }

let meta_spec_constant :=
  | NUMERAL; { }
  | DECIMAL; { }
  | STRING; { }

let fun_symbol_decl :=
  | LPAR; spec_constant; sort; attribute*; RPAR; { }
  | LPAR; meta_spec_constant; sort; attribute*; RPAR; { }
  | LPAR; identifier; sort+; attribute*; RPAR; { }

let par_fun_symbol_decl :=
  | fun_symbol_decl; { }
  | LPAR; PAR; LPAR; symbol+; RPAR;
    LPAR; identifier; sort+; attribute; RPAR; RPAR; { }

let theory_attribute :=
  | SORTS; LPAR; sort_symbol_decl+; RPAR; { }
  | FUNS; LPAR; par_fun_symbol_decl+; RPAR; { }
  | SORTS_DESCRIPTION; STR; { }
  | FUNS_DESCRIPTION; STR; { }
  | DEFINITION; STR; { }
  | VALUES; STR; { }
  | NOTES; STR; { }
  | attribute; { }

let theory_decl :=
  | LPAR; THEORY; symbol; theory_attribute+; RPAR; { }

(* Logics *)
let logic_attribute :=
  | THEORIES; LPAR; symbol+; RPAR; { }
  | LANGUAGE; STR; { }
  | EXTENSIONS; STR; { }
  | VALUES; STR; { }
  | NOTES; STR; { }
  | attribute; { }

let logic :=
  | LPAR; LOGIC; symbol; logic_attribute+; RPAR; { }

(* Commands *)
let sort_dec :=
  | LPAR; symbol; NUM; RPAR; { }

let selector_dec :=
  | LPAR; symbol; sort; RPAR; { }

let constructor_dec :=
  | LPAR; symbol; selector_dec*; RPAR; { }

let datatype_dec :=
  | LPAR; constructor_dec+; RPAR; { }
  | LPAR; PAR; LPAR; symbol+; RPAR; LPAR; constructor_dec+; RPAR; RPAR; { }

let function_dec :=
  | LPAR; symbol; LPAR; sorted_var*; sort; RPAR; { }

let function_def :=
  | symbol; LPAR; sorted_var*; RPAR; sort; term; { }

let prop_literal :=
  | symbol; { }
  | LPAR; NOT; symbol; RPAR; { }

let command :=
  | LPAR; ASSERT; ~ = term; RPAR;
    { Assert term }
  | LPAR; CHECK_SAT; RPAR;
    { Check_sat }
  | LPAR; CHECK_SAT_ASSUMING; LPAR; prop_literal*; RPAR; RPAR;
    { Check_sat_assuming }
  | LPAR; DECLARE_CONST; symbol; sort; RPAR;
    { Declare_const }
  | LPAR; DECLARE_DATATYPE; symbol; datatype_dec; RPAR;
    { Declare_datatype }
  | LPAR; DECLARE_DATATYPES; LPAR; sort_dec+; RPAR;
    LPAR; datatype_dec+; RPAR; RPAR;
    { Declare_datatypes }
  | LPAR; DECLARE_FUN; ~ = symbol; LPAR; args = sort*; RPAR; ~ = sort; RPAR;
    { Declare_fun (symbol, args, sort) }
  | LPAR; DECLARE_SORT; ~ = symbol; n = NUM; RPAR;
    { Declare_sort (symbol, int_of_string n) }
  | LPAR; DEFINE_FUN; function_def; RPAR;
    { Define_fun }
  | LPAR; DEFINE_FUN_REC; function_def; RPAR;
    { Define_fun_rec }
  | LPAR; DEFINE_FUNS_REC; LPAR; function_dec+; RPAR; LPAR; term+; RPAR; RPAR;
    { Define_funs_rec }
  | LPAR; DEFINE_SORT; symbol; LPAR; symbol*; RPAR; sort; RPAR;
    { Define_sort }
  | LPAR; ECHO; str = STR; RPAR;
    { Echo str }
  | LPAR; EXIT; RPAR;
    { Exit }
  | LPAR; GET_ASSERTIONS; RPAR;
    { Get_assertions }
  | LPAR; GET_ASSIGNMENT; RPAR;
    { Get_assignment }
  | LPAR; GET_INFO; info_flag; RPAR;
    { Get_info }
  | LPAR; GET_MODEL; RPAR;
    { Get_model }
  | LPAR; GET_OPTION; opt = KEYWORD; RPAR;
    { Get_option opt }
  | LPAR; GET_PROOF; RPAR;
    { Get_proof }
  | LPAR; GET_UNSAT_ASSUMPTIONS; RPAR;
    { Get_unsat_assumptions }
  | LPAR; GET_UNSAT_CORE; RPAR;
    { Get_unsat_core }
  | LPAR; GET_VALUE; LPAR; terms = term+; RPAR; RPAR;
    { Get_value terms }
  | LPAR; POP; n = NUM; RPAR;
    { Pop (int_of_string n) }
  | LPAR; PUSH; n = NUM; RPAR;
    { Push (int_of_string n) }
  | LPAR; RESET; RPAR;
    { Reset }
  | LPAR; RESET_ASSERTIONS; RPAR;
    { Reset_assertions }
  | LPAR; SET_INFO; attribute; RPAR;
    { Set_info }
  | LPAR; SET_LOGIC; ~ = symbol; RPAR;
    { Set_logic symbol }
  | LPAR; SET_OPTION; option_; RPAR;
    { Set_option }

(* Info flags *)
let info_flag := { }

(* Command options *)
let option_ := { }
