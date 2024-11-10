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

A Curva de Koch é um fractal gerado por um algoritmo iterativo, que segue o seguinte processo geométrico:

1. Iniciar com um segmento de comprimento L. 
2. Retirar o terço médio do segmento inicial. 
3. Substituir o terço médio por um triângulo equilátero sem base de lado igual ao terço do segmento original.

Esse processo é repetido recursivamente para cada segmento gerado, aumentando a complexidade da figura a cada iteração. O algoritmo segue esse ciclo em cada iteração, produzindo uma curva que se torna progressivamente mais detalhada e auto-similar, caracterizando o comportamento fractal da Curva de Koch. 
Na **Imagem 1**, podemos observar a construção da curva conforme as iterações vão aumentando.

<div align="center">
  <figure style="display: inline-block; margin-right: 20px;">
    <img src="imgs/koch.png" width="550" />
    <figcaption><strong>Imagem 1</strong>: Fractal Curva de Koch</figcaption>
  </figure>
</div>

#### Conjunto de Mandelbrot:

O fractal de Mandelbrot é um dos fractais mais famosos e fascinantes. O fractal é definido pelo conjunto de todos os pontos complexos 
para os quais a sequência gerada pela fórmula iterativa:

<div align="center">
  <figure style="display: inline-block; margin-right: 20px;">
    <img src="imgs/formulaMandelbrot.png" width="300" />
    <figcaption><strong>Imagem 2</strong>: Fórmula do conjunto de Mandelbrot</figcaption>
  </figure>
</div>

Algoritmicamente, o fractal de Mandelbrot é gerado ao iterar uma fórmula para cada ponto de uma grade de pixels, normalmente representada no plano complexo Cada ponto é tratado como um número complexo sendo sobre eles aplicado a fórmula anterior repetidamente para verificar se 
z diverge (escapa para o infinito) ou permanece limitado. Na **Imagem 3** vamos visualizar como fica a imagem deste fractal em seu momento inicial, posteriormente, na **imagem 4**, vamos demonstrar após dadas iterações:

<div align="center">
  <figure style="display: inline-block; margin-right: 20px;">
    <img src="imgs/mandlbrotInicial.png" width="300" />
    <figcaption><strong>Imagem 3</strong>: Fractal de Mandelbrot, sem nenhuma iteração</figcaption>
  </figure>
</div>


<div align="center">
  <figure style="display: inline-block; margin-right: 20px;">
    <img src="imgs/mandelbrot.png" width="300" />
    <figcaption><strong>Imagem 4</strong>: Fractal de Mandelbrot, após algumas iterações</figcaption>
  </figure>
</div>

#### Árvore de Pitágoras:
O fractal conhecido como "Árvore de Pitágoras" é um tipo de fractal geométrico que se assemelha a uma árvore, sendo composto por trapezios dispostos de forma recursiva. Esse fractal tem seu nome em homenagem ao Teorema de Pitágoras, pois a construção de cada "galho" segue a relação desse teorema.

O algoritimo desenvolvido da árvore de pitágoras começa com um trapezio que representa o tronco da árvore. A partir dele, cinco trapezios menores são adicionados no topo, formando um ângulo em relação ao trapezio inicial, que simboliza os primeiros "galhos" da árvore. Esse processo é então repetido para cada novo trapezio, com uma rotação e uma escala que mantêm o formato e a proporção dos galhos, criando assim uma estrutura ramificada e cada vez mais complexa.

A complexidade da árvore é controlada pelo número de iterações (ou profundidade recursiva) aplicadas. Em um número baixo de iterações, a árvore terá uma estrutura mais simples, como na **Imagem 5** (com duas iterações). Já em um número maior de iterações, como na **Imagem 6** (com cinco iterações), a árvore exibe uma quantidade maior de detalhes e uma estrutura que se assemelha mais a uma árvore real.

<div align="center">
  <figure style="display: inline-block; margin-right: 20px;">
    <img src="imgs/PythagorasTree2.png" width="300" />
    <figcaption><strong>Imagem 5</strong>: Árvore de Pitágoras com 2 interações</figcaption>
  </figure>
  <figure style="display: inline-block; margin-left: 20px;">
    <img src="imgs/PythagorasTree5.png" width="300" />
    <figcaption><strong>Imagem 6</strong>: Árvore de Pitágoras com 5 interações</figcaption>
  </figure>
</div>



#### Triângulo de Sierpinski:
O Triângulo de Sierpinski é um fractal e um exemplo de um conjunto auto-similar, onde uma figura é composta por cópias de si mesma. Ele foi descrito pelo matemático polonês Wacław Sierpiński em 1915 e é obtido por um processo de divisão recursiva de triângulos. A construção começa com um triângulo equilátero e, em cada iteração, os triângulos menores são removidos para formar uma nova geração do fractal.

##### Propriedades e Curiosidades

* O Triângulo de Sierpinski possui auto-similaridade exata, o que significa que, independentemente do nível de ampliação, ele sempre parece o mesmo. Isso é característico de fractais, onde as partes menores replicam o todo.
* O fractal é criado por um processo recursivo, onde um triângulo é repetidamente subdividido em triângulos menores.
* Embora seja construído em um espaço bidimensional, o Triângulo de Sierpinski possui uma dimensão fractal entre 1 e 2. Sua dimensão é log(3)/log(2) ≈ 1,585, refletindo o fato de que ele ocupa mais espaço do que uma linha, mas menos do que uma área bidimensional completa.
* À medida que o processo de subdivisão continua indefinidamente, a área do Triângulo de Sierpinski converge para zero. Isso ocorre porque, em cada etapa, uma fração do triângulo original é removida.
* O perímetro, no entanto, aumenta a cada iteração, tornando-se infinitamente longo. Esse comportamento ilustra a diferença entre área e perímetro em fractais e em formas geométricas clássicas.

##### Lógica implementada do Triângulo de Sierpinski
* Passo 1: Comece com um triângulo equilátero.
* Passo 2: Divida o triângulo em quatro triângulos menores conectando os pontos médios de cada lado.
* Passo 3: Remova o triângulo central.
* Passo 4: Repita os passos 2 e 3 para cada um dos triângulos restantes.

##### Extra
Adição da mudança de cor dos triângulos restantes por meio da função makeColor, para que a mudança ocorrece a cada interação foi utilizada a função fromIntegral passando como argumento a profundidadde da interação. Dessa maneira, cada interação irá obter sua própria coloração de triângulos.

A seguir, na imagem 7, podemos observar a construção do fractal conforme as iterações vão aumentando.


<div align="center">
  <figure style="display: inline-block; margin-right: 20px;">
    <img src="imgs/interacoes-triangulo.png" width="550" />
    <figcaption><strong>Imagem 7</strong>: Fractal triângulo de Sierpinski</figcaption>
  </figure>
</div>

#### Tapete de Sierpinsk:


## Screenshots
![rep](imgs/exemplo-de-uso.gif)

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
