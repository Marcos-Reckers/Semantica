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

(* ========== Tipos =======================================================*)


(* ========== Implementações do trabalho ==================================*)

(* e1 |> e2 *)(*todos*)

(* ============== Luiz ====================================================*)
(* nil | e1::e2 *) 


(* match e1 with nil ⇒ e2 | x::xs ⇒ e3 *)

(*============== Lipe =====================================================*)
(* nothing | just e *)


(* match e1 with nothing ⇒ e2 | just x ⇒ e3 *)


(*============= Marcos ====================================================*)
(* left e | right e *)

(* match e1 with left x ⇒ e2 | right y ⇒ e3 *)

(*=========================================================================*)

(* ========= Exemplos de Teste ============================================*)



