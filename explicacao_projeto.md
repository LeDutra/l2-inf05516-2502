# Documentação do Projeto L2 (OCaml)
## Introdução
Este documento detalha a implementação do interpretador para a linguagem L2, incluindo a Sintaxe Abstrata, a Inferência de Tipos e o Avaliador Small-Step. O objetivo é fornecer uma explicação clara para a apresentação do trabalho.

---

## 1. Sintaxe Abstrata (AST)
A "Abstract Syntax Tree" define a estrutura da linguagem. É como definimos a gramática que o OCaml entende.

### Tipos (`type tipo`)
Define os tipos de dados aceitos na linguagem:
- `TyInt`: Inteiros (ex: `1`, `5`).
- `TyBool`: Booleanos (`true`, `false`).
- `TyUnit`: Representa comandos que não retornam valor útil (como atribuições). É similar ao `void` em C/Java.
- `TyRef of tipo`: Referências (ponteiros) de memória. `TyRef TyInt` é um ponteiro para um inteiro.

### Expressões (`type expr`)
Representam o código em si.
- **Valores Básicos:** `Num` (números), `Bool` (booleanos), `Unit`.
- **Variáveis:** `Id "x"`.
- **Operações:** `Binop` (soma, subtração, comparações, etc) e `If`.
- **Imperativo:**
  - `Let`: Declaração de variáveis.
  - `New`: Aloca memória (cria um ponteiro).
  - `Deref (!)`: Lê o valor dentro de um ponteiro.
  - `Asg (:=)`: Atribui (escreve) um novo valor na memória.
  - `Seq (;)`: Sequência de comandos. Executa o primeiro, descarta o resultado, e executa o segundo.
  - `Wh`: Loop `while`.
- **Interno:**
  - `Loc of int`: **Importante!** Isso não existe no código fonte escrito pelo usuário. É criado durante a execução (`New`) para representar um endereço de memória real (ex: endereço 0, endereço 1).

---

## 2. Inferência de Tipos (`infer`)
Esta função verifica se o programa faz sentido *antes* de rodar (tempo de compilação). Ela segue as regras `Γ ⊢ e : T` do PDF.

- **Entrada:** Um contexto `ctx` (lista de variáveis e seus tipos) e uma expressão `e`.
- **Saída:** O tipo da expressão (`tipo`) ou um erro (`TypeError`).

### Lógica Chave:
- **Pattern Matching (`match e with`):** O OCaml verifica qual é o tipo da expressão atual.
- **Binop:** Verifica se os operandos batem. Soma (`Sum`) exige dois `TyInt`. Comparação (`Lt`) exige dois `TyInt` mas retorna `TyBool`.
- **Let:** Calcula o tipo do valor (`t1`). Verifica se ele bate com o tipo declarado (`t`). Se sim, adiciona a variável no contexto e continua analisando o corpo.
- **Ref/Deref/Assign:**
  - `New e`: Se `e` é `int`, `New e` é `ref int`.
  - `Deref e (!)`: Se `e` é `ref int`, `!e` retorna `int`.
  - `Asg (:=)`: Verifica se estamos atribuindo o tipo certo dentro da referência certa.

---

## 3. Avaliador Small-Step (`step` e `eval`)
Esta parte roda o código. O modelo "Small-Step" significa que resolvemos uma pequena parte da conta de cada vez (ex: `1+1+1` vira `2+1`, depois vira `3`).

### Memória (`store`)
Como a linguagem tem ponteiros (`ref`), precisamos de uma memória.
- `type store`: É uma lista de pares `(endereço, valor)`.
- `next_loc`: Um contador global para gerar novos endereços (0, 1, 2...).

### Substituição (`subst`)
L2 usa substituição para lidar com variáveis.
- Quando fazemos `let x = 5 in x + 1`, o avaliador substitui todos os `x` por `5` no corpo. Vira `5 + 1`.

### O Passo (`step`)
Recebe a memória atual (`s`) e a expressão (`e`). Retorna `Some (nova_expressão, nova_memória)` ou `None` (se terminou ou travou).

**Casos Principais:**
1.  **Binop (Matemática):** Se os dois lados são valores (números prontos), calcula. Se não, tenta resolver o lado esquerdo ou direito.
2.  **If:** Se a condição é `true`, retorna o bloco `then`. Se `false`, o `else`.
3.  **New (Alocação):**
    - Gera um novo endereço (`new_loc`).
    - Guarda o valor na memória (`update_store`).
    - Retorna um `Loc l` (ponteiro) substituindo o comando `new`.
4.  **Deref (!):** Olha o endereço na memória (`lookup_store`) e devolve o valor guardado.
5.  **Assign (:=):** Atualiza a memória naquele endereço e retorna `Unit`.
6.  **While:** Truque clássico de compiladores.
    - `while c do b` vira `if c then (b; while c do b) else ()`.
    - Ele se desenrola em um `If`.

### Avaliação Completa (`eval`)
Fica chamando `step` repetidamente (loop recursivo) até que `step` retorne `None` (quando chega num valor final como um número ou um booleano).

---

## 4. O Exemplo (Fatorial)
O código define manualmente a AST para calcular o fatorial de 5.

```ocaml
let fat = Let("x", TyInt, Num 5, 
            Let("z", TyRef TyInt, New (Id "x"),  (* z aponta para 5 *)
                Let("y", TyRef TyInt, New (Num 1), (* y aponta para 1 *)
                    seq)))                         (* Loop do fatorial *)
```
Isso é equivalente a:
```c
int x = 5;
int *z = new(x);
int *y = new(1);
while (*z > 0) {
    *y = *y * *z;
    *z = *z - 1;
}
return *y;
```
