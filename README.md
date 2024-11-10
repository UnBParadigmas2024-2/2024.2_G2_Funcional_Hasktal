# Hasktal


**Disciplina**: FGA0210 - PARADIGMAS DE PROGRAMAÇÃO - T01 <br>
**Nro do Grupo (de acordo com a Planilha de Divisão dos Grupos)**: 02<br>
**Paradigma**: Funcional<br>

## Alunos
|Matrícula | Aluno |
| -- | -- |
| 21/1039288  |  Bruno Campos Ribeiro |
| 21/1039297  |  Bruno Martins Valério Bomfim |
| 20/2016462  |  Heitor Marques|
| 21/1029352  |  Igor e Silva Penha |
| 19/0057858  |  José Luís Ramos Teixeira |
| 21/1029405  |  Leonardo Gonçalves Machado |
| 20/2016767  |  Lucas Felipe Soares |
| 20/0062379  |  Marcos Vinícius de Deus |
| 20/2045268  |  Raquel Eucaria Pereira da Costa |
| 21/2002907  |  Zenilda Pedrosa Vieira |


## Sobre 

O projeto visa representar visualmente uma variedade de fractais gerados a partir de padrões repetitivos. Utilizando Haskell, serão desenvolvidos processos iterativos para criar diferentes tipos de fractais, explorando suas propriedades geométricas e características únicas derivadas de funções matemáticas específicas. A tela inicial mostra os fractais que foram desenvolvidos, conforme a imagem 1:

<div align="center">
  <figure style="display: inline-block; margin-right: 20px;">
    <img src="./imgs/telaInicial.png" width="300" />
    <figcaption><strong>Imagem 1</strong>: Tela inicial com os fractais desenvolvidos</figcaption>
  </figure>
  <font size="3"><p style="text-align: center"><strong>Fonte:</strong> Autores</a>, 2024</p></font>
</div>

Mais informações sobre cada fractal podem ser encontradas no documento de [descrição dos fractais desenvolvidos](./docs/descricaoFractais.md).



## Screenshots
![rep](./imgs/exemplo-de-uso.gif)

<div style="text-align:center"> Vídeo 1: Exemplo de uso da interface</div>
<br>

## Instalação 
**Linguagens**: Haskell<br>
**Tecnologias**: Stack<br>
1. Instale o `stack`,  o tutorial de instalação pode ser encontrado [aqui](https://docs.haskellstack.org/en/stable/install_and_upgrade/).
2. Clone o repositório 
    ```bash
    git clone https://github.com/UnBParadigmas2024-2/2024.2_G2_Funcional_Hasktal.git
    ```
3. No terminal, abra o diretório onde o arquivo `hasktal.cabal` esta localizado e execute:
    ```bash
    stack build
    stack exec hasktal-exe
    ```

## Uso 
A partir dessa tela, é possível aumentar as iterações clicando no botão com o símbolo de mais (+), gerando progressivamente a imagem do fractal. O usuário também pode reduzir o número de iterações utilizando o botão com o símbolo de menos (-). Para retornar ao menu inicial e selecionar outro fractal, basta clicar no botão "Voltar".

## Vídeo
Adicione 1 ou mais vídeos com a execução do projeto.
Procure: 
(i) Introduzir o projeto;
(ii) Mostrar passo a passo o código, explicando-o, e deixando claro o que é de terceiros, e o que é contribuição real da equipe;
(iii) Apresentar particularidades do Paradigma, da Linguagem, e das Tecnologias, e
(iV) Apresentar lições aprendidas, contribuições, pendências, e ideias para trabalhos futuros.
OBS: TODOS DEVEM PARTICIPAR, CONFERINDO PONTOS DE VISTA.
TEMPO: +/- 15min

## Participações
Apresente, brevemente, como cada membro do grupo contribuiu para o projeto.
|Nome do Membro | Contribuição | Significância da Contribuição para o Projeto (Excelente/Boa/Regular/Ruim/Nula) | Comprobatórios (ex. links para commits)
| -- | -- | -- | -- |
| Fulano  |  Programação dos Fatos da Base de Conhecimento Lógica | Boa | Commit tal (com link)
|  Bruno Campos Ribeiro |
|  Bruno Martins Valério Bomfim |
|  Heitor Marques|
|  Igor e Silva Penha |
|  José Luís Ramos Teixeira |
|  Leonardo Gonçalves Machado |
|  Lucas Felipe Soares | Programação dos fractais de mandelbrot e Julia, revisão de código e documentação| Boa | Commits e histórico do git como: [exemplo](https://github.com/UnBParadigmas2024-2/2024.2_G2_Funcional_Hasktal/commit/9b03ed6c5577f58023c9d4945ca18f3b56881a87)|
|  Marcos Vinícius de Deus |
|  Raquel Eucaria Pereira da Costa |
|  Zenilda Pedrosa Vieira | Programação do fractal Árvore de Pitágoras de quadrados (Fractal Tree) <br> Programação do fractal Tapete de Siervinski (Siervinski Carpet) <br> Inclusão dos fractais na main <br> Formatação de layouts <br> Ajuda na escrita e formatação de tópicos do README e do documento de descricao dos fractais | Excelente | [Fractal Tree]() <br> [Siervinsk Carpet]() <br> [Fractais na main]() <br> [Layouts]() <br> [README.md]() <br>  [descricaoFractais.md]()|


## Outros 
Quaisquer outras informações sobre o projeto podem ser descritas aqui. Não esqueça, entretanto, de informar sobre:

(i) Lições Aprendidas;

(ii) Percepções;

(iii) Contribuições e Fragilidades, e

(iV) Trabalhos Futuros.


## Fontes
1. HASKELL. Documentação do Haskell. Disponível em: <https://www.haskell.org/documentation>. Acesso em: 02 de Novembro de 2024.
2. Wikipedia. Fractal. Disponível em: <https://pt.wikipedia.org/wiki/Fractal>. Acesso em: 02 de Novembro de 2024.
