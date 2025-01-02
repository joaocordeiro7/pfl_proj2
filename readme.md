não se pode mover para espaços brancos
diagonais?


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
* Uma pilha só se pode mover para uma 8 casas que fazem extrita fronteira com a casa onde está inicialmente, ou seja, pode mover-se na vertical, horizontal ou diagonal;
* Uma pilha não pode saltar para cima de outra pilha ou mover-se para espaços vazios, ou seja, só se pode mover para uma casa com exatamente uma peça neutra;
* Se um jogador não puder mover nenhuma das suas pilhas, deve passar;
* Se ambos os jogadores passarem consecutivamente, o jogo termina.

**Critérios para determinar o vencedor:**
1. O jogador com a pilha mais alta vence;
2. Se ambos os jogadores tiverem pilhas com a maior altura, vence o que tiver mais pilhas com esse tamanho;
3. Se ainda assim houver empate, comparam-se as seguintes pilhas mais altas ou a maioria das pilhas de altura igual até que o vencedor seja determinado.


## Considerações para Extensões do Jogo
? Permitir tabuleiros maiores (ex.: 6x6 - 10 peças, 7x7 - 12 peças ...)
? ...


## Lógica do Jogo
**Representação da Configuração do Jogo**


**Representação Interna do Estado do Jogo**


**Representação das jogadas**


**Interação do utilizador**




## Conclusões


## Bibliografia

* Descrição do Jogo - https://boardgamegeek.com/boardgame/425529/staqs
* 