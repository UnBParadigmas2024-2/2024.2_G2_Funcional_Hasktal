<!-- Código referência
-- Autor: Mriganka Basu Roy Chowdhury
-- Título do projeto: A fractal generator in haskell
-- Fonte: https://gist.github.com/mbrc12/c3a40215022ea8efcddf7ad39993e4f3
-->

# Simple
## Executando com o Slack
1. Instale o`stack`,  o tutorial de instalação pode ser encontrado [aqui](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

2. No diretório onde o arquivo `stack.yaml` e o arquivo `.cabal` estão localizados, execute o seguinte comando para que o Stack configure o ambiente:

```bash
stack setup
stack build
stack exec generate-exe
```