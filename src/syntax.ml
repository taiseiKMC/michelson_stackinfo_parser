(* ML interpreter / type reconstruction *)
type str = string
type prim = string

type position = (str * int) list
type location = position * position (* start, end *)
type ty =
    Type of str
  | App of ty * ty
type stack_ty = ty list
type info = location * stack_ty * stack_ty
type value =
    STRV of string
  | INTV of int
  | PairV of value * value
  (* めんどい *)

type arg =
    ArgCode of exp
  | ArgTy of ty
  | Arg of value
and exp =
    Seq of exp list * info
  | Prim of prim * arg list * info
  | NISeq of exp list (* Failしたなどでinfoが無い時 *)
  | NIPrim of prim * arg list

type code =
    Exp of exp

let rec print_ty ppf = function
    Type str -> Format.fprintf ppf "%s" str
  | App (ty1, ty2) -> Format.fprintf ppf "(%a %a)" print_ty ty1 print_ty ty2
let rec print_value ppf = function
    STRV str -> Format.fprintf ppf "(STRV %s)" str
  | INTV i -> Format.fprintf ppf "(INTV %d)" i
  | PairV (v1, v2) -> Format.fprintf ppf "(PairV %a %a)" print_value v1 print_value v2

let print_stackty ppf st_ty =
  Format.fprintf ppf "%a" (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf " : ") print_ty) st_ty
let print_position ppf poss =
  Format.fprintf ppf "%a" (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ") (fun ppf (st, i) -> Format.fprintf ppf "%s:%d" st i)) poss
let print_location ppf = fun (s,t) -> Format.fprintf ppf "(s:%a,t:%a)" print_position s print_position t

let print_info ppf = fun (loc, st1, st2) ->
  Format.fprintf ppf "(%a, %a, %a)" print_location loc print_stackty st1 print_stackty st2

let rec print_arg ppf = function
    ArgCode exp -> Format.fprintf ppf "ArgCode (%a)" print_exp exp
  | ArgTy ty -> Format.fprintf ppf "ArgTy (%a)" print_ty ty
  | Arg value -> Format.fprintf ppf "Arg (%a)" print_value value
and print_args ppf args = Format.fprintf ppf "[%a]" (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ") print_arg) args
and print_exp ppf = function
    Seq (exps, info) -> Format.fprintf ppf "Seq (%a, %a)"
                          (Format.pp_print_list ~pp_sep:Format.pp_print_space print_exp) exps
                          print_info info
  | NISeq exps -> Format.fprintf ppf "NISeq (%a)"
                    (Format.pp_print_list ~pp_sep:Format.pp_print_space print_exp) exps
  | Prim (prim, args, info) -> Format.fprintf ppf "Prim (%s, %a, %a)" prim print_args args print_info info
  | NIPrim (prim, args) -> Format.fprintf ppf "NIPrim (%s, %a)" prim print_args args

let print_code ppf (Exp exp) =
  print_exp ppf exp