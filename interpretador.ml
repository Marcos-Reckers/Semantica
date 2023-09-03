(*
Interpretador para L1 polimórfica, implicitamente tipada e com extensões.

Instruções:
      O trabalho consiste em implementar um interpretador para a linguagem L1 polimórfica implicitamente tipada com extensões conforme a gramática abstrata abaixo.
      O interpretador é constituído de uma função que realiza inferência de tipos, e de uma função que realiza a avaliação no estilo big-step (além de outras funções auxiliares).

Entregáveis: implementação e regras da semântica operacional big step e regras de coleta para as construções das linhas marcadas com asterisco na gramática abaixo.*)

(*
e ∈ Expr
e ::= n | b | x | e1 op e2
| ⟨e1, e2⟩ | fst e | snd e
| if e1 then e2 else e3
| fn x ⇒ e | e1 e2
| let x = e1 in e2
| let rec = fn x ⇒ e1 in e2
(∗) | e1 |> e2
(∗) | nil | e1::e2
(∗) | match e1 with nil ⇒ e2 | x::xs ⇒ e3
(∗) | nothing | just e
(∗) | match e1 with nothing ⇒ e2 | just x ⇒ e3
(∗) | left e | right e
(∗) | match e1 with left x ⇒ e2 | right y ⇒ e3
op ∈ {+, −, ∗, <, ≤, >, ≥, =, and, or}
v ::= n | b | fn x ⇒ e | ⟨v1, v2⟩ | nil | just v | nothing | left v | right v
T ::= int | bool | T1 → T2 | T1 ∗ T2 | either T1 T2 | T list | maybe T | X
σ ::= ∀X.σ | T |

*)

type tipo = 
    TyInt
  | TyBool
  | TyFn     of tipo * tipo
  | TyPair   of tipo * tipo 
  | TyVar    of int   (* variáveis de tipo -- números *)

                                               
type expr  =
    Num    of int 
  | Bool   of bool
  | Var    of ident
  | Binop  of bop * expr * expr
  | Pair   of expr * expr
  | Fst    of expr
  | Snd    of expr
  | If     of expr * expr * expr
  | Fn     of ident * expr                   
  | App    of expr * expr
  | Let    of ident * expr * expr           
  | LetRec of ident * ident * expr * expr 
  | Nil
  | Match of expr * pattern * expr * expr  (* Expressão, Padrão vazio, Expressão do padrão vazio, Expressão do padrão não vazio *)

and pattern =
  | PNil
  | PCons of string * string  (* Variáveis para o head e tail *)
  
type valor =
    VNum   of int
  | VBool  of bool
  | VPair  of valor * valor 
  | VClos  of ident * expr * renv
  | VRclos of ident * ident * expr * renv
and
  renv = (ident * valor) list
  | Vnil

(* ========== Tipos =======================================================*)


(* ========== Implementações do trabalho ==================================*)

(* e1 |> e2 *)(*todos*)

(* ============== Luiz ====================================================*)
(* nil | e1::e2 *) 

(* match e1 with nil ⇒ e2 | x::xs ⇒ e3 *)
(* 
REGRAS DA SEMÂNTICA OPERACIONAL BIG-STEP
         
         Γ ⊢ e1 ⇒ nil    Γ ⊢ e2 ⇒ v
----------------------------------------------
     Γ ⊢ match e1 with
              | nil -> e2
              | x::xs -> e3 ⇒ v
         

   Γ ⊢ e1 ⇒ x::xs    Γ ⊢ e3 ⇒ v
----------------------------------------------
     Γ ⊢ match e1 with
              | nil -> e2
              | x::xs -> e3 ⇒ v
*)

(* Incluir match with Match em eval*)
let rec eval (renv:renv) (e:expr) : valor =
  match exp with
  | Match(e1, pat, e2, e3) ->
      let v1 = eval ctx e1 in
      match_pattern ctx pat v1 e2 e3

and match_pattern ctx pat value e2 e3 =
  match pat with
  | PNil ->
      (match value with
      | Nil -> eval ctx e2
      | _ -> failwith "Pattern mismatch in match expression.")
  | PCons(x, xs) ->
      (match value with
      | Cons(h, t) ->
          let ctx' = (x, h) :: (xs, t) :: ctx in
          eval ctx' e3
      | _ -> failwith "Pattern mismatch in match expression.")

(*
REGRA DE TIPO
Γ ⊢ e1 : list[A]    Γ ⊢ e2 : T    x : A, xs : list[A], Γ ⊢ e3 : T
--------------------------------------------------------------
              Γ ⊢ match e1 with
                       | [] -> e2
                       | x::xs -> e3 : T
*)

let rec type_check ctx exp =
  match exp with
  | Match(e1, pat, e2, e3) ->
      let t1 = type_check ctx e1 in
      let t2 = type_check_pattern ctx pat in
      if t1 = t2 then
        let ctx' = extend_context ctx pat t2 in
        let t_e2 = type_check ctx' e2 in
        let t_e3 = type_check ctx' e3 in
        if t_e2 = t_e3 then t_e2
        else failwith "Branches of the match expression have different types."
      else failwith "Type mismatch in match expression."

and type_check_pattern ctx pat =
  match pat with
  | PNil -> TList TInt
  | PCons(x, xs) ->
      let tx = (try List.assoc x ctx with Not_found -> failwith ("Variable " ^ x ^ " not found in the context."))
      and txs = (try List.assoc xs ctx with Not_found -> failwith ("Variable " ^ xs ^ " not found in the context."))
      in
      match txs with
      | TList typ -> if tx = typ then TList tx else failwith "Type mismatch in pattern."
      | _ -> failwith "Pattern is not a list."

and extend_context ctx pat typ =
  match pat with
  | PNil -> ctx
  | PCons(x, xs) -> (x, typ) :: (xs, TList typ) :: ctx


(*============== Lipe =====================================================*)
(* nothing | just e *)


(* match e1 with nothing ⇒ e2 | just x ⇒ e3 *)


(*============= Marcos ====================================================*)
(* left e | right e *)

(* match e1 with left x ⇒ e2 | right y ⇒ e3 *)

(*=========================================================================*)

(* ========= Exemplos de Teste ============================================*)



