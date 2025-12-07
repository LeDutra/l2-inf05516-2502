(*****************************************************)
(*                   LINGUAGEM L2                    *)
(*  Trabalho de Semântica Formal - INF05516 (25/2)   *)
(*                  Leonardo Dutra                   *)
(*                   Luana Hahn                      *)
(*****************************************************)

(*****************************************************)
(*                    DEFINIÇÕES                     *)
(*****************************************************)

(* Tipos de Operadores Binários *)
type bop = 
  | Sum | Sub | Mul | Div               (* Aritméticos: +, -, *, / *)
  | Eq  | Neq | Lt  | Gt                (* Relacionais: =, <>, <, > *)
  | And | Or                            (* Lógicos: &&, || *)

(* Tipos da Linguagem *)
type tipo = 
  | TyInt                               (* Inteiro: int *)
  | TyBool                              (* Booleano: bool *)
  | TyRef of tipo                       (* Referência (Ponteiro): ref T *)
  | TyUnit                              (* Unitário (Comando): unit *) 
    
(* Árvore de Sintaxe Abstrata (AST) *)
type expr = 
  | Num of int                          (* Números inteiros: 1, 42 *)
  | Bool of bool                        (* Booleanos: true, false *)
  | Id of string                        (* Variáveis (Identifiers): x, y *)
  | Binop of bop * expr * expr          (* Operação Binária: e1 op e2 *)
  | If of expr * expr * expr            (* Condicional: if e1 then e2 else e3 *)
  | Let of string * tipo * expr * expr  (* Declaração: let x:T = e1 in e2 *)
  | Wh of expr * expr                   (* Loop: while e1 do e2 *)
  | Asg of expr * expr                  (* Atribuição (Assign): e1 := e2 *)
  | New of expr                         (* Alocação: new e *)
  | Deref of expr                       (* Desreferência: !e *)
  | Seq of expr * expr                  (* Sequência: e1; e2 *)
  | Unit                                (* Valor Unitário: () *)
  | Loc of int                          (* Alocações de memória (apenas em tempo de execução) *)

(*****************************************************)
(*              INFERÊNCIA DE TIPOS                  *)
(*****************************************************)

(* Exceção para erros de tipo encontrados *)
exception TypeError of string

(* Função auxiliar para converter tipos em string (para mensagens de erro) *)
let rec type_to_string t = 
  match t with
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyRef t -> "ref " ^ type_to_string t
  | TyUnit -> "unit"
  | _ -> raise (TypeError "Tipo desconhecido na conversão para string")

(* Ambiente de tipos (Γ) *)
type tyenv = (string * tipo) list 

(* Busca o tipo de uma variável no ambiente *)
let rec lookup (env : tyenv) (x : string) : tipo = 
  match env with 
  | [] -> raise (TypeError ("Variável não declarada: " ^ x))
  | (y, t) :: rest -> if x = y then t else lookup rest x

(* Função principal de inferência de tipos (Γ ⊢ e : T) *)
let rec typeinfer (env : tyenv) (e : expr) : tipo = 
  match e with

  (* T-INT *)
  | Num _ -> TyInt

  (* T-BOOL *)
  | Bool _ -> TyBool

  (* Operadores Binários *)
  | Binop (op, e1, e2) ->
      let t1 = typeinfer env e1 in
      let t2 = typeinfer env e2 in
      (match op with 

      (* T-OP+, T-OP-, T-OP*, T-OP/ *)
      | Sum | Sub | Mul | Div ->
          if t1 = TyInt && t2 = TyInt then TyInt
          else raise (TypeError "Operações aritiméticas exigem que e1 e e2 sejam inteiros")

      (* T-OP<, T-OP>, T-OP=, T-OP!= *)
      | Lt | Gt | Eq | Neq ->
          if t1 = t2 then TyBool
          else raise (TypeError "Comparações entre e1 e e2 exigem que ambos sejam do mesmo tipo")
          
      (* T-OPAND, T-OPOR *)
      | And | Or ->
          if t1 = TyBool && t2 = TyBool then TyBool
          else raise (TypeError "Operações lógicas esperam booleanos"))
           
  (* T-IF *)
  | If (cond, e_then, e_else) ->
      if typeinfer env cond = TyBool then
        let t2 = typeinfer env e_then in
        let t3 = typeinfer env e_else in
        if t2 = t3 then t2
        else raise (TypeError ("Tipos dos ramos 'then' e 'else' não são iguais"))
      else raise (TypeError "Condição do if deve ser booleana")

  (* T-VAR *)
  | Id x -> lookup env x (* Busca o tipo de x - se x está no contexto do ambiente de tipos *)

  (* T-LET *)
  | Let (x, t_decl, e1, e2) ->
      let t1 = typeinfer env e1 in
      if t1 = t_decl then 
        typeinfer ((x, t_decl) :: env) e2 (* Estende o contexto Γ com x:T e infere e2, x |→ T *)
      else raise (TypeError ("Tipo de e1 não corresponde ao tipo declarado para " ^ x))
  
  (* T-ATR *)
  | Asg (e1, e2) ->
      let t1 = typeinfer env e1 in
      let t2 = typeinfer env e2 in
      if t1 = TyRef t2 then TyUnit
      else raise (TypeError ("O tipo ref T de e1 precisa ter o ser do mesmo tipo T de e2"))

  (* T-DEREF *)
  | Deref e ->
      (match typeinfer env e with 
       | TyRef t -> t (* Se for um tipo de referência, retorna o tipo referenciado *)
       | _ -> raise (TypeError "Operação de desreferência exige um tipo ref T"))

  (* T-NEW *)
  | New e ->
      let t = typeinfer env e in
      TyRef t (* Retorna uma referência para o tipo de e *)

  (* T-UNIT *)
  | Unit -> TyUnit
       
  (* T-WHILE *)
  | Wh (cond, body) ->
      if typeinfer env cond = TyBool then
        if typeinfer env body = TyUnit then TyUnit
        else raise (TypeError "Corpo do while deve ser do tipo unit")
      else raise (TypeError "Condição do while deve ser booleana")
      
  (* T-SEQ *)
  | Seq (e1, e2) ->
      if typeinfer env e1 = TyUnit then typeinfer env e2
      else raise (TypeError "Primeiro de uma sequência deve ser unit")

  | Loc _ -> raise (TypeError "Alocações de memória não devem aparecer no código fonte")

(*****************************************************)
(*              AVALIADOR (SMALL-STEP)               *)
(*****************************************************)

(* Memória *)
(* Representação da memória como uma lista de pares (local, valor) *)
type tymem = (int * expr) list

(* Gerenciador de Endereços Únicos *)
let next_loc = ref 0
let new_loc () =
  let l = !next_loc in
  incr next_loc;
  l

(* Atualiza o valor em um endereço de memória *)
let update_tymem (s : tymem) (l : int) (v : expr) : tymem =
  (l, v) :: List.filter (fun (loc, _) -> loc <> l) s

(* Busca o valor em um endereço de memória *)
let lookup_tymem (s : tymem) (l : int) : expr =
  try List.assoc l s
  with Not_found -> failwith ("Falha de Segmentação: endereço " ^ string_of_int l ^ " não alocado")

(* Verifica se uma expressão já é um valor final (não pode ser mais reduzida) *)
let is_value e = match e with
  | Num _ | Bool _ | Unit | Loc _ -> true
  | _ -> false

(* Substituição: substitui ocorrências de x por v na expressão e ({v/x}e) *)
let rec subst x v e =
  match e with
  | Num _ | Bool _ | Unit | Loc _ -> e
  | Id y -> if x = y then v else e (* Se achou a variável, troca pelo valor *)
  | If (e1, e2, e3) -> If (subst x v e1, subst x v e2, subst x v e3)
  | Binop (op, e1, e2) -> Binop (op, subst x v e1, subst x v e2)
  | Wh (e1, e2) -> Wh (subst x v e1, subst x v e2)
  | Asg (e1, e2) -> Asg (subst x v e1, subst x v e2)
  | Let (y, t, e1, e2) ->
      let e1' = subst x v e1 in
      if x = y then Let (y, t, e1', e2) (* Se x foi redeclarado, não substitui no corpo *)
      else Let (y, t, e1', subst x v e2)
  | New e1 -> New (subst x v e1)
  | Deref e1 -> Deref (subst x v e1)
  | Seq (e1, e2) -> Seq (subst x v e1, subst x v e2)

(* Um Passo da Execução (Small-Step): Transforma (e, s) em (e', s') *)
let rec step (s : tymem) (e : expr) : (expr * tymem) option =
  match e with
  | Num _ | Bool _ | Unit | Loc _ -> None                       (* Valores já estão prontos, não precisa dar passo *)
  
  (* Regras OP1 e OP2: Reduz operandos binários *)
  | Binop (op, e1, e2) when is_value e1 && is_value e2 ->
      (match op, e1, e2 with
       | Sum, Num n1, Num n2 -> Some (Num (n1 + n2), s)  
       | Sub, Num n1, Num n2 -> Some (Num (n1 - n2), s)  
       | Mul, Num n1, Num n2 -> Some (Num (n1 * n2), s)  
       | Div, Num n1, Num n2 -> Some (Num (n1 / n2), s)  
       | Lt,  Num n1, Num n2 -> Some (Bool (n1 < n2), s) 
       | Gt,  Num n1, Num n2 -> Some (Bool (n1 > n2), s) 
       | Eq,  Num n1, Num n2 -> Some (Bool (n1 = n2), s) 
       | Neq, Num n1, Num n2 -> Some (Bool (n1 <> n2), s)
       | And, Bool b1, Bool b2 -> Some (Bool (b1 && b2), s)
       | Or,  Bool b1, Bool b2 -> Some (Bool (b1 || b2), s)
       | _ -> None) (* Tipo errado, por segurança checamos *)
       
  | Binop (op, e1, e2) when is_value e1 ->
      (match step s e2 with
       | Some (e2', s') -> Some (Binop (op, e1, e2'), s')
       | None -> None)
       
  | Binop (op, e1, e2) ->
      (match step s e1 with
       | Some (e1', s') -> Some (Binop (op, e1', e2), s')
       | None -> None)

  (* Regras IF1, IF2, IF3 *)
  | If (Bool true, e2, _) -> Some (e2, s)
  | If (Bool false, _, e3) -> Some (e3, s)
  | If (e1, e2, e3) ->
      (match step s e1 with
       | Some (e1', s') -> Some (If (e1', e2, e3), s')
       | None -> None)

  (* Regras LET1 e LET2 *)
  | Let (x, _, v, e2) when is_value v ->
      Some (subst x v e2, s) (* Substitui x por v em e2 *)
  | Let (x, t, e1, e2) ->
      (match step s e1 with
       | Some (e1', s') -> Some (Let (x, t, e1', e2), s')
       | None -> None)

  (* Regras NEW e NEW1 *)
  | New v when is_value v ->
      let l = new_loc () in
      Some (Loc l, update_tymem s l v) (* Cria Loc l e salva v na memória *)
  | New e1 ->
      (match step s e1 with
       | Some (e1', s') -> Some (New e1', s')
       | None -> None)

  (* Regras DEREF e DEREF1 *)
  | Deref (Loc l) ->
      Some (lookup_tymem s  l, s) (* Lê valor da memória *)
  | Deref e1 ->
      (match step s e1 with
       | Some (e1', s') -> Some (Deref e1', s')
       | None -> None)

  (* Regras ATR, ATR1, ATR2 *)
  | Asg (Loc l, v) when is_value v ->
      Some (Unit, update_tymem s l v) (* Atualiza memória e retorna Unit *)
  | Asg (Loc l, e2) ->
      (match step s e2 with
       | Some (e2', s') -> Some (Asg (Loc l, e2'), s')
       | None -> None)
  | Asg (e1, e2) ->
      (match step s e1 with
       | Some (e1', s') -> Some (Asg (e1', e2), s')
       | None -> None)

  (* Regras SEQ e SEQ1 *)
  | Seq (Unit, e2) -> Some (e2, s) (* Descarta o primeiro comando se já acabou *)
  | Seq (e1, e2) ->
      (match step s e1 with
       | Some (e1', s') -> Some (Seq (e1', e2), s')
       | None -> None)

  (* Regra WHILE: desenrola o loop uma vez *)
  | Wh (cond, body) ->
      Some (If (cond, Seq (body, Wh (cond, body)), Unit), s)

  | Id _ -> None (* Variável livre? Erro de execução ou variável não substituída *)


(* Loop de Avaliação: roda 'step' até terminar *)
let rec eval (s : tymem) (e : expr) : expr * tymem =
  match step s e with
  | Some (e', s') -> eval s' e'
  | None -> (e, s)

(*****************************************************)
(*                      TESTES                       *)
(*****************************************************)

(* Converte expressão para string para impressão *)
let rec expr_to_string e = 
  match e with
  | Num n -> string_of_int n
  | Bool b -> string_of_bool b
  | Unit -> "()"
  | Loc l -> "loc(" ^ string_of_int l ^ ")"
  | Id x -> x
  | Binop _ -> "operacao"
  | _ -> "expr_complexa"

(* TESTE - fatorial
  let  x:     int = 5     in 
  let  z: ref int = new x in 
  let  y: ref int = new 1 in 
  (while (!z > 0) (
         y :=  !y * !z;
         z :=  !z - 1);
  !y)     
*)

let teste_factorial = 
  let body_loop = Seq(                                              (* body_loop: *)
      Asg(Id "y", Binop(Mul, Deref(Id "y"), Deref(Id "z"))),        (* y := !y * !z *)
      Asg(Id "z", Binop(Sub, Deref(Id "z"), Num 1))                 (* z := !z - 1 *)
  ) in
  Let("x", TyInt, Num 5,                                            (* let x:     int = 5     in *)
      Let("z", TyRef TyInt, New (Id "x"),                           (* let z: ref int = new x in *)
          Let("y", TyRef TyInt, New (Num 1),                        (* let y: ref int = new 1 in *)
              Seq(Wh(Binop(Gt, Deref(Id "z"), Num 0), body_loop),   (* while (!z > 0) do body_loop; *)
                  Deref (Id "y")))))                                (* !y *)
                  
let () =
  print_endline "*** Interpretador L2 ***";
  try
    print_string "1. Verificação de tipos (Inferência)... ";
    let t = typeinfer [] teste_factorial in
    print_endline ("Sucesso! Tipo inferido: " ^ type_to_string t);
    
    print_string "2. Executando programa (Avaliação)... ";
    let (res, _) = eval [] teste_factorial in
    print_endline ("Resultado Final: " ^ expr_to_string res)
  with 
  | TypeError msg -> print_endline ("\nFalha! Erro de tipo: " ^ msg)
  | Failure msg -> print_endline ("\nErro na execução: " ^ msg)
