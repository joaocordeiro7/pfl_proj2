# PFL - 2º Trabalho Prático
# Implementação de um Jogo de Tabuleiro em Prolog

## Identificação do tópico e do grupo

Neste projeto foi desenvolvido o jogo **STAQS** em Prolog, no contexto da unidade curricular de Programção Funcional e em Lógica (PFL).    
Foi realizado pelos elementos do grupo *STAQS_7* da turma 10: 
* João Francisco Costa Cordeiro, up202205682
* Luana Filipa de Matos Lima, up202206845

Ambos os membros contribuiram de igual forma (50% / 50%) para este trabalho, sendo que o João realizou ... e a Luana ...


## Instalação e Execução (Rever)
Para instalar este jogo é necessário fazer o download da pasta ***PFL_TP2_T10_STAQS_7.zip*** e descompactar. Dentro do diretório ***scr*** deve-se consultar o ficeiro ***game.pl*** através da linha de comandos ou pela própria UI do Sicstus Prolog 4.9. O jogo está disponível para ambientes Windows e Linux.    
O jogo inicia-se com o predicado play/0:
```
?- play.
```


## Descrição do Jogo
**STAQS** é um jogo de tabuleiro estratégico para dois jogadores. É jogado num tabuleiro 5 por 5 em que cada jogador tem 4 peças empilháveis. O jogo começa com 25 peças neutras, uma por cada casa do tabuleiro. A princípio, à vez, cada jogador deve posicionar uma das suas peças em cima de uma das peças neutras até as 8 peças estarem colocadas. A partir daí, na sua vez, o jogador deve mover uma das suas pilhas (incluindo as peças neutras) uma casa, para cima de outra peça neutra. 

**Principais regras:**
* Uma pilha só se pode mover para uma das 8 casas que fazem extrita fronteira com a casa onde está inicialmente, ou seja, pode mover-se na vertical, horizontal ou diagonal;
* Uma pilha não pode saltar para cima de outra pilha ou mover-se para espaços vazios, ou seja, só se pode mover para uma casa com exatamente uma peça neutra;
* Se um jogador não puder mover nenhuma das suas pilhas, deve passar;
* Se ambos os jogadores passarem consecutivamente, o jogo termina.

**Critérios para determinar o vencedor:**
1. O jogador com a pilha mais alta vence;
2. Se ambos os jogadores tiverem pilhas com a maior altura, vence o que tiver mais pilhas com esse tamanho;
3. Se ainda assim houver empate, comparam-se as seguintes pilhas mais altas ou a maioria das pilhas de altura igual até que o vencedor seja determinado.


## Considerações para Extensões do Jogo
> Neste jogo é permitido escolher tabuleiros de diferentes tamanhos, sendo que o número de peças de cada jogador depende desse tamanho (o número de peças por jogador é igual ao tamanho do tabuleiro menos 1).    

> Além disso, o utilizador pode também escolher entre 4 modos de jogo diferentes:
* **Humano vs. Humano -** O jogo espera a interação de dois jogadores;
* **Humano vs. PC -** O jogo espera interação para o primeiro jogador enquanto o segundo jogador é um bot;
* **PC vs. Humano -** Semelhante à opção anterior mas a ordem de jogadores é trocada;
* **PC vs. PC -** O jogo é jogado por dois bots.    
> Para casos em que o PC é escolhido para jogar, o utilizador pode também escolher o seu nível de dificuldade entre duas opções: 

* **1 -** O bot faz jogadas aleatórias tendo em conta todas as opções disponíveis e válidas;   
* **2 -** O bot utiliza um algoritmo greedy para determinar qual a melhor jogada tendo em conta as opções disponíveis e válidas.    
> Já para casos em que um ou dois jogadores humanos, cada um destes pode inserir um nome para ser apresentado nas suas jogadas.


## Lógica do Jogo
**Representação da Configuração do Jogo**


**Representação Interna do Estado do Jogo**


**Representação das jogadas**


**Interação do utilizador**




## Conclusões


## Bibliografia

* Descrição do Jogo - https://boardgamegeek.com/boardgame/425529/staqs
* 