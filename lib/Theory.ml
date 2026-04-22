module I = Type_inference.Make ()
open Simple_type.Infix

let t_bool = Simple_type.TPrim `Bool
let t_int = Simple_type.TPrim `Int

let t_dup =
  let rho = I.fresh_stack_var ~level:0 () in
  let a = I.fresh_ty_var ~level:0 () in
  rho &> a => (rho &> a &> a)

let t_drop =
  let rho = I.fresh_stack_var ~level:0 () in
  let a = I.fresh_ty_var ~level:0 () in
  rho &> a => rho

let t_swap =
  let rho = I.fresh_stack_var ~level:0 () in
  let a = I.fresh_ty_var ~level:0 () in
  let b = I.fresh_ty_var ~level:0 () in
  rho &> a &> b => (rho &> b &> a)

let t_math_binop =
  let rho = I.fresh_stack_var ~level:0 () in
  rho &> t_int &> t_int => (rho &> t_int)

let t_math_cmp =
  let rho = I.fresh_stack_var ~level:0 () in
  rho &> t_int &> t_int => (rho &> t_bool)

let t_bool_binop =
  let rho = I.fresh_stack_var ~level:0 () in
  rho &> t_bool &> t_bool => (rho &> t_bool)

let t_bool_unop =
  let rho = I.fresh_stack_var ~level:0 () in
  rho &> t_bool => (rho &> t_bool)

let t_choose =
  let rho = I.fresh_stack_var ~level:0 () in
  let a = I.fresh_ty_var ~level:0 () in
  rho &> t_bool &> a &> a => (rho &> a)

let t_call =
  let rho = I.fresh_stack_var ~level:0 () in
  let sigma = I.fresh_stack_var ~level:0 () in
  rho &> (rho => sigma) => sigma

let t_dip =
  let rho = I.fresh_stack_var ~level:0 () in
  let sigma = I.fresh_stack_var ~level:0 () in
  let a = I.fresh_ty_var ~level:0 () in
  rho &> a &> (rho => sigma) => (sigma &> a)

let load_prelude db =
  let store name ty =
    Db.store db (Query.WordType name) (Diagnosed.return ty) []
  in
  store "dup" t_dup;
  store "drop" t_drop;
  store "swap" t_swap;
  store "+" t_math_binop;
  store "-" t_math_binop;
  store "*" t_math_binop;
  store "/" t_math_binop;
  store "=" t_math_cmp;
  store "!=" t_math_cmp;
  store "<" t_math_cmp;
  store ">" t_math_cmp;
  store "<=" t_math_cmp;
  store ">=" t_math_cmp;
  store "and" t_bool_binop;
  store "not" t_bool_unop;
  store "choose" t_choose;
  store "call" t_call;
  store "dip" t_dip;
  ()
