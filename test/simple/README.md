<!-- Código referência
-- Autor: Mickaël Bergeron Néron
-- Título do projeto: "Simple fractal in Haskell in under 100 lines of code"
-- Fonte: https://www.dropbox.com/scl/fi/hxx73ate9jka88gbue8vd/Haskell-fractals.7z?rlkey=jih3wg8584b8f85oqa4mh4xkh&e=2&dl=0 

-->

# Simple
## Executando com o Slack
1. Instale o`stack`,  o tutorial de instalação pode ser encontrado [aqui](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

2. No diretório onde o arquivo `stack.yaml` e o arquivo `.cabal` estão localizados, execute o seguinte comando para que o Stack configure o ambiente:

```bash
stack setup
stack build
stack exec simple-exe
```

3. Usando o menu
Alterne entre fractal usando a tecla 's' ou 'k'.