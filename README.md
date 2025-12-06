# l2-inf05516-2502

Trabalho final da disciplina de Semântica Formal - INF05516 (25/2)

# Setup

Para instalar o ambiente necessário para rodar o interpretador L2 localmente:

1. Instale o [OPAM](https://opam.ocaml.org/doc/Install.html), o gerenciador de pacotes do OCaml.
2. Inicialize o OPAM (caso seja a primeira vez):
    ```sh
    opam init
    eval $(opam env)
    ```
3. Instale o compilador OCaml recomendado:
    ```sh
    opam switch create 4.14.0
    eval $(opam env)
    ```
4. Instale as dependências do projeto:
    ```sh
    opam install ounit2
    ```
5. Clone este repositório e navegue até o diretório do projeto:
    ```sh
    git clone <url-do-repositorio>
    cd l2-inf05516-2502
    ```

# Como carregar a linguagem dos arquivos `.ml`

Será necessário carregar a linguagem em um interpretador para testá-la.

## 1. Rodar com o interpretador interativo:

Você pode usar o interpretador `ocaml` para testar comandos interativamente:

```sh
ocaml
```

Dentro do interpretador, carregue seu arquivo:

```ocaml
#use "l2.ml";;
```

## 2. Executar pelo navegador usando o website TryOCaml:

Caso não tenha instalado o interpretador localmente, poderá ser utilizado o interpretador online [TryOCaml](https://try.ocamlpro.com/). Copie e cole o código à esquerda no website, e clique em `Eval code`. O interpretador fica à direita da tela.
