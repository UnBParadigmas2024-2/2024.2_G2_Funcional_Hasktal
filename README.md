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
O projeto visa representar visualmente uma variedade de fractais gerados a partir de padrões repetitivos. Utilizando Haskell, serão desenvolvidos processos iterativos para criar diferentes tipos de fractais, explorando suas propriedades geométricas e características únicas derivadas de funções matemáticas específicas. Os fractais desenvolvidos foram:

#### Conjunto de Julia:

#### Curva de Koch:

#### Conjunto de Mandelbrot:

O fractal de Mandelbrot é um dos fractais mais famosos e fascinantes. O fractal é definido pelo conjunto de todos os pontos complexos 
para os quais a sequência gerada pela fórmula iterativa:

<div align="center">
  <figure style="display: inline-block; margin-right: 20px;">
    <img src="imgs/formulaMandelbrot.png" width="300" />
    <figcaption><strong>Imagem 1</strong>: Fórmula do conjunto de Mandelbrot</figcaption>
  </figure>
</div>

Algoritmicamente, o fractal de Mandelbrot é gerado ao iterar uma fórmula para cada ponto de uma grade de pixels, normalmente representada no plano complexo Cada ponto é tratado como um número complexo sendo sobre eles aplicado a fórmula anterior repetidamente para verificar se 
z diverge (escapa para o infinito) ou permanece limitado. Na **Imagem 2** vamos visualizar como fica a imagem deste fractal:

<div align="center">
  <figure style="display: inline-block; margin-right: 20px;">
    <img src="imgs/mandelbrot.png" width="300" />
    <figcaption><strong>Imagem 2</strong>: Fractal de Mandelbrot</figcaption>
  </figure>
</div>

#### Árvore de Pitágoras:
O fractal conhecido como "Árvore de Pitágoras" é um tipo de fractal geométrico que se assemelha a uma árvore, sendo composto por trapezios dispostos de forma recursiva. Esse fractal tem seu nome em homenagem ao Teorema de Pitágoras, pois a construção de cada "galho" segue a relação desse teorema.

O algoritimo desenvolvido da árvore de pitágoras começa com um trapezio que representa o tronco da árvore. A partir dele, cinco trapezios menores são adicionados no topo, formando um ângulo em relação ao trapezio inicial, que simboliza os primeiros "galhos" da árvore. Esse processo é então repetido para cada novo trapezio, com uma rotação e uma escala que mantêm o formato e a proporção dos galhos, criando assim uma estrutura ramificada e cada vez mais complexa.

A complexidade da árvore é controlada pelo número de iterações (ou profundidade recursiva) aplicadas. Em um número baixo de iterações, a árvore terá uma estrutura mais simples, como na **Imagem 3** (com duas iterações). Já em um número maior de iterações, como na **Imagem 4** (com cinco iterações), a árvore exibe uma quantidade maior de detalhes e uma estrutura que se assemelha mais a uma árvore real.

<div align="center">
  <figure style="display: inline-block; margin-right: 20px;">
    <img src="imgs/PythagorasTree2.png" width="300" />
    <figcaption><strong>Imagem 3</strong>: Árvore de Pitágoras com 2 interações</figcaption>
  </figure>
  <figure style="display: inline-block; margin-left: 20px;">
    <img src="imgs/PythagorasTree5.png" width="300" />
    <figcaption><strong>Imagem 4</strong>: Árvore de Pitágoras com 5 interações</figcaption>
  </figure>
</div>



#### Triângulo de Sierpinski:

#### Tapete de Sierpinsk:


## Screenshots
Adicione 2 ou mais screenshots do projeto em termos de interface e/ou funcionamento.

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

Ao iniciar o projeto, uma janela será exibida com uma lista dos fractais desenvolvidos pela equipe. O usuário pode selecionar o fractal desejado para visualização, e a estrutura inicial da forma (sem iteração) será apresentada. 

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
|  Zenilda Pedrosa Vieira |


## Outros 
Quaisquer outras informações sobre o projeto podem ser descritas aqui. Não esqueça, entretanto, de informar sobre:

(i) Lições Aprendidas;

(ii) Percepções;

(iii) Contribuições e Fragilidades, e

(iV) Trabalhos Futuros.


## Fontes
1. HASKELL. Documentação do Haskell. Disponível em: <https://www.haskell.org/documentation>. Acesso em: 02 de Novembro de 2024.
2. Wikipedia. Fractal. Disponível em: <https://pt.wikipedia.org/wiki/Fractal>. Acesso em: 02 de Novembro de 2024.
