# **HASKELLPONG**
# Jogo PONG em linguagem funcional HASKELL

![alt text](https://github.com/GabrielMendoncaF/HaskellPong/blob/main/Imagem%20do%20jogo.png?raw=true)

Descrição do Projeto 📋
====================
O projeto busca criar uma versão do jogo PONG, similar a sua versão para o console ATARI. Implementado totalmente através linguagem funcional Haskell e o uso da biblioteca Gloss, que permite a implementação de figuras e controle de objetos na tela.
Jogo criado utilizando a linguagem funcional Haskell, como projeto final para a disciplina de Programação funcional UFABC Q2 2021

Comandos e funções 🔧
==================

|Função                      |Comando
|----------------------------|--------|
|Subir player da direita     |W
|Descer player da direita    |S
|Subir player da esquerda    |Up
|Subir player da esqurda     |Down
|Pausar o jogo               |P
|Despausa o jogo             |ENTER

Como executar 📋
====================
Para executar este jogo, é necessário que tenha em seu computador instaladas as ferramentas necessárias para controlar códigos em Haskell, GHC, Cabal e Stack, instaladas.
Mais informações em:
https://www.haskell.org/platform/

Com as ferramentas instaladas abra um terminal no local da pasta do jogoe insira os comandos:

    stack run


Dificuldades 
====================
No desenvolvimento deste projeto, o uso da bibioteca gloss se mostrou um desafio, seguir seus parâmetros a fim de obter os resultados esperados, para movimentação e colisão dos objetos em tela. A implementação dos contadores de pontos inspirados no funcionamento de um display digital de 7 seguimentos também se mostrou mais complexa que o esperado, para que o mesmo pudesse criar os números usando 7 retângulos e simular os algarismos e posteriormente, estes números formados pudessem ser apresentados em tela quando o jogador pontuasse.

Link para o vídeo de apresentação do projeto e código:
https://www.youtube.com/watch?v=x1QItp0SH2o
