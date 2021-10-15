{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where
import ImagensBase
import Graphics.Gloss
    ( text,
      scale,
      rectangleSolid,
      color,
      translate,
      white,
      pictures,
      black,
      play,
      Picture,
      Color,
      Display(InWindow) )
import Graphics.Gloss.Data.ViewPort ()
import Graphics.Gloss.Interface.Pure.Game
    ( SpecialKey(KeyUp, KeyDown, KeyEnter),
      KeyState(Down, Up),
      Key(SpecialKey, Char),
      Event(EventKey) )
import GHC.Exts (RuntimeRep(FloatRep))

data HaskellPong = Game
    {
      pausado      :: Bool           -- pausa do jogo
    , posiboll     :: (Float, Float) -- posição (x,y) da bolinha
    , veloboll     :: (Float, Float) -- velocidade (x,y) da bolinha
    , jogador1     :: Float          -- posicao do jogador 1
    , jogador2     :: Float          -- posição do jogador 2
    , raqTamanho   :: (Float, Float) -- tamanho da raquete (jogador) 
    , movJogador   :: (Float, Float) -- movimentação do jogador
    , pontuacaoD   :: Float            -- contador de pontos lado direito     
    , pontuacaoE   :: Float            -- contador de pontos lado esquerdo 
    , jogoEstado   :: JogoEstado       -- estados possíveis do jogo início, jogando, pausado, vitória da direita ou esquerda
    } deriving Show

type Raio                = Float             -- metade da largura da bolinha
type Position            = (Float, Float)    -- define posição como float, float, (x e y) 
type RaqTamanho          = (Float, Float)    -- tamanho da raquete
type Raquetepositela     = (Float, Float)    -- posição da raquete em relação a tela
type PontuacaoE          = Float               -- contador de pontos do lado esquerdo
type PontuacaoD          = Float              -- contador de pontos do lado esquerdo
type Posiboll            = (Float, Float)    -- posição da bolinha (x,y)
type Distancia           = Float

data JogoEstado = Inicio | Jogando | Pausa | VitoriaD | VitoriaE -- Os possíveis estados do jogo
    deriving (Show, Eq)

main :: IO ()
main = play janela fundo frames estadoInicial criaImagens entradas atualizacao -- configuração main com funções segundo as especificações do gloss gloss

frames :: Int -- A quantidade de verificações de estado por segundo
frames = 300

--tamanho da tela
larguratela, alturatela, posidisplay :: Int
larguratela  = 192*4 -- 192 é a largura da resolução de tela do atari
alturatela   = 160*4 -- 160 é a altura da resolução de tela do atari
posidisplay  = 300   -- posição que a tela carrega

estadoInicial :: HaskellPong
estadoInicial  = Game
    {
      pausado      = False          -- Estado de pausa, para parar os movimentos do jogo, true or false
    , posiboll     = (0, 0)         -- posição da bolinha (x,y)
    , veloboll     = (-150, -150)   -- velocidade da bolinha (x,y)
    , jogador1     = 20             -- posição jogador 1 (y)
    , jogador2     = -160           -- posição jogador 2 (y)
    , raqTamanho   = (22, 86)       -- Tamanho do player (tamanho do desenho da raquete)
    , movJogador   = (0, 0)         -- Movimentação do jogador
    , pontuacaoD   = 0              -- contadores de pontos da direita
    , pontuacaoE   = 0              -- contador de pontos da esquerda
    , jogoEstado   = Inicio
    }

janela :: Display
janela = InWindow "PONG" (larguratela, alturatela) (posidisplay, posidisplay) -- dimensões da tela

fundo :: Color
fundo = black  -- cor do fundo

fimDeJogoD :: PontuacaoD -> Bool -- direita ganhou o jogo
fimDeJogoD pontuacaoD  = pontuacao10
    where
        pontuacao10  = pontuacaoD  >=  10

jogoFinalizadoD :: HaskellPong -> HaskellPong
jogoFinalizadoD game = game { jogoEstado = p' } -- muda o estado para tela final vitória do jogador da direita
    where

        p  = jogoEstado game -- retorna a pontação atual

        p' = if fimDeJogoD (pontuacaoD game)
            then
                VitoriaD -- estado em que o jogador da direita ganha
            else
                p   -- se não, mantém

fimDeJogoE :: PontuacaoE -> Bool -- direita ganhou o jogo
fimDeJogoE pontuacaoE  = pontuacao10
    where
        pontuacao10  = pontuacaoE  >=  10

jogoFinalizadoE :: HaskellPong -> HaskellPong
jogoFinalizadoE game = game { jogoEstado = p' } -- muda o estado para tela final vitória do jogador da esquerda
    where
        p  = jogoEstado game -- retorna a pontação atual

        p' = if fimDeJogoE (pontuacaoE game)
            then
                VitoriaE -- estado em que o jogador da esquerda ganha
            else
                p   -- se não, mantém

-- verificando se a bolinha saiu para a esquerda
pontuouD :: Position -> Bool -- direita ganha ponto
pontuouD (x, _)  = saiuDireita
    where
        saiuDireita  = x  <=  -400

contadorDePontosD :: HaskellPong -> HaskellPong
contadorDePontosD game = game { pontuacaoD = p' } -- incrementando a pontuação
    where

        p  = pontuacaoD game -- retorna a pontação atual

        p' = if pontuouD (posiboll game) -- caso a saída da bolinha tenha sido verificada
            then
                p+1 -- incrementa 1 na pontuação
            else
                p   -- se não, mantém

-- verificando se a bolinha saiu para a direita
pontuouE :: Position -> Bool -- direita ganha ponto
pontuouE (x, _)  = saiuEsquerda
    where
        saiuEsquerda  = x  >=  400

contadorDePontosE :: HaskellPong -> HaskellPong
contadorDePontosE game = game { pontuacaoE = p' } -- incrementando a pontuação
    where

        p  = pontuacaoE game -- retorna a pontação atual

        p' = if pontuouE (posiboll game)
            then
                p+1 -- incrementa 1 na pontuação
            else
                p   -- se não, mantém

-- as duas incrementações de pontuaçao acima, ocorrem exatamente no mesmo instante que a bolinha volta para o centro
-- por esta razão, é incrementado apenas 1 ponto, pois a condição >= 400 ou <= -400 muda imediatamente

moveBall :: Float -> HaskellPong -> HaskellPong -- movimento da bolinha, é uma função que será usada mais
moveBall atualizacao game = game { posiboll = (xf, yf) }
    where
        (x, y)   = posiboll game -- posição atual
        (vx, vy) = veloboll game -- velocidade atual

        x' = x + vx * atualizacao -- calcula a nova posição no eixo x
        y' = y + vy * atualizacao -- calcula a nova posição no eixo y

        estado = jogoEstado game

        -- caso a bolinha saia da tela, volta para o centro, caso contrário, atualiza a posição pela velocidade
        xf
          | x' > 400 && estado == Jogando || x' < -400 && estado == Jogando = 0
          | x' < 400 && estado == Jogando || x' > -400 && estado == Jogando = x'
          | otherwise = 0

        yf
          | x' > 400 && estado == Jogando || x' < -400 && estado == Jogando = 0
          | x' < 400 && estado == Jogando || x' > -400 && estado == Jogando = y'
          | otherwise = 0

moveRaquetes :: Float -> HaskellPong -> HaskellPong                     -- movimentação das raquetes (jogadores)
moveRaquetes atualizacao game = game { jogador1 = j1', jogador2 = j2' } -- atualiza as posições em Y dos jogadores
    where
        (mov1, mov2) = movJogador game                             -- Retorna os valores inseridos no início do programa
        (raquetelarguratela, raquetealturatela1) = raqTamanho game -- retorna o tamanho das raquetes
        j1 = jogador1 game -- retorna a posição dos jogador1
        j2 = jogador2 game -- retorna a posição dos jogador2

        telaMeialarguratela    = fromIntegral alturatela / 2 - 31 -- 31 é a distancia para que não sobreponha a parede superior
        j1sobe  = j1 + raquetealturatela1 / 2 -- subida  = posição atual + metade da altura da raquete
        j1desce = j1 - raquetealturatela1 / 2 -- descida = posição atual - metade da altura da raquete

        j1Subida   = j1sobe  <  telaMeialarguratela && mov1 > 0 -- condição de verifica subida
        j1Descida  = j1desce > -telaMeialarguratela && mov1 < 0 -- condição que verifica descida

        j2sobe  = j2 + raquetealturatela1 / 2 -- subida  = posição atual + metade da altura da raquete
        j2desce = j2 - raquetealturatela1 / 2 -- descida = posição atual - metade da altura da raquete

        j2Subida   = j2sobe  <  telaMeialarguratela && mov2 > 0 -- condição de verifica subida
        j2Descida  = j2desce > -telaMeialarguratela && mov2 < 0 -- condição que verifica descida

        estado = jogoEstado game

        -- se subida ou descida forem verdadeiras
        -- então modifica a posição dos jogadores, caso contrário, mantem a posição original
        -- só movimenta se o estado for de jogo em andamento -> '0'

        j1' = if j1Subida && estado == Jogando || j1Descida && estado == Jogando
              then j1 + mov1 * atualizacao
              else j1

        j2' = if j2Subida  && estado == Jogando|| j2Descida && estado == Jogando
              then j2 + mov2 * atualizacao
              else j2

limiteTelaiColisao :: Position -> Raio -> Bool -- verifica a colisão da bolinha com o topo e a parte de baixo da tela
limiteTelaiColisao (_, y) distancia = cimaColisao || baixoColisao
    where
        cimaColisao  = y - distancia  <= -(fromIntegral alturatela / 2) -- a altura máxima que a bolinha pode chegar
        baixoColisao = y + distancia  >=  fromIntegral alturatela / 2 -- a altura minima que a bolinha pode chegar

limiteTelaiRebate :: HaskellPong -> HaskellPong -- ação de mudar a velocidade caso as condições de colisão sejam cumpridas
limiteTelaiRebate game = game { veloboll = (vx, vy') }
    where
        distancia = 42            -- a distância minima da bolinha com o canto da tela
        (vx, vy)  = veloboll game -- retorna a velocidade inicial do código
        vy'       = if limiteTelaiColisao (posiboll game) distancia -- se a função de colisão retornar verdadeiro
            then
                -vy -- inverte a velocidade em y
            else
                vy  -- caso contrário mantém velocidade igual

--colisao da bolinha com a raquete
raqueteColisao :: Position -> Raio -> Raquetepositela -> RaqTamanho -> Bool
raqueteColisao (ballX, ballY) raio (raquete1posidisplay, raquete2posidisplay) (raquetelarguratela, raquetealturatela1)  = bateuesquerda || bateuDireita
    where
        diametro     = raio * 2                 -- metade da largura da bolinha * 2
        raqueteCorpo = 314 - raquetelarguratela -- a posição x do player - a largura

        -- intervalo da distância X onde ocorre a colisão na raquete esquerda
        xEsquerdaColisao  = ballX - raio <= (-314)        && ballX - raio >= (-320) -- distâncias ajustadas manualmente
        -- intervalo da distância X onde ocorre a colisão na raquete edireita
        xDireitaColisao   = ballX - raio >= raqueteCorpo  && ballX - raio <= raqueteCorpo + 5 -- distâncias ajustadas manualmente         

        --verificando igualdade na posição Y da bolinha e da raquete esquerda
        yEsquerdaColisao  = raquete2posidisplay + raquetealturatela1 / 2 >= ballY && raquete2posidisplay - raquetealturatela1 / 2 <= ballY
        --verificando igualdade na posição Y da bolinha e da raquete direita
        yDireitaColisao   = raquete1posidisplay + raquetealturatela1 / 2 >= ballY && raquete1posidisplay - raquetealturatela1 / 2 <= ballY

        --Caso as colisões em X e Y sejam confirmadas verdadeiras, a batida é verificada no lado esquerdo
        bateuesquerda     = xEsquerdaColisao && yEsquerdaColisao
        --Caso as colisões em X e Y sejam confirmadas verdadeiras, a batida é verificada no lado direito
        bateuDireita      = xDireitaColisao  && yDireitaColisao


--condição que muda a direção da bolinha caso colida com a raquete
raqueteRebate :: HaskellPong -> HaskellPong
raqueteRebate game = game { veloboll = ( vx', vy ) }
    where
        distancia = 11            -- metade da largura da bolinha
        (vx, vy)  = veloboll game -- Retorna a velocidade atual da boinha

        -- se a condição de colisão for verdadeira
        vx'       = if raqueteColisao (posiboll game) distancia (jogador1 game, jogador2 game) (raqTamanho game)
            then
                -vx -- inverte a velocidade em x
            else
                vx  -- caso contrário, mantém a velocidade 

criaImagens :: HaskellPong -> Picture -- função que cria os desenhos de acordo com a biblioteca gloss
criaImagens game =
    pictures
            [ ball (jogoEstado game)              -- bolinha (que na verdade é um quadrado, para ser similar ao original)
            , limiteTela                            -- parede superior 
            , raquete white 320 $ jogador1 game     -- raquete do jogador 1 
            , raquete white (-320) $ jogador2 game  -- raquete do jogador 2 
            , redes                                 -- quadradinhos centrais que dividem a tela
            , pontoPdisplayE (pontuacaoE game)      -- função  que desenha o display Esquerdo
            , pontoPdisplayD (pontuacaoD game)      -- função que desenha o display direito
            , paredeDireita                         -- parede do lado direito da tela   
            , paredeEsquerda                        -- parede do lado esquerdo da tela
            , escondeRaquetesD (jogoEstado game)  -- bloco preto sobre as raquetes
            , escondeRaquetesE (jogoEstado game)  -- bloco preto sobre as raquetes
            , escondeRede   (jogoEstado game)     -- bloco preto sobre as raquetes e escrita de PAUSA
            , escondePlacar (jogoEstado game)     -- bloco preto sobre o placar
            , inicioDoJogo  (jogoEstado game)     -- escreve na tela inicial do jogo
            , fimDoJogoE    (jogoEstado game)     -- tela final quando o jogador da esquerda vence
            , fimDoJogoD    (jogoEstado game)     -- tela final quando o jogador da direita vence
            ]
    where

        --igualando as os valores numéricos aos desenhados no display, através da variável 'pontuacaoE'
        pontoPdisplayE :: PontuacaoE -> Picture
        pontoPdisplayE pontuacaoE
            | pontuacaoE == 0 = numero0e
            | pontuacaoE == 1 = numero1e
            | pontuacaoE == 2 = numero2e
            | pontuacaoE == 3 = numero3e
            | pontuacaoE == 4 = numero4e
            | pontuacaoE == 5 = numero5e
            | pontuacaoE == 6 = numero6e
            | pontuacaoE == 7 = numero7e
            | pontuacaoE == 8 = numero8e
            | pontuacaoE == 9 = numero9e
            | pontuacaoE == 10 = numero9e -- aqui a partida acaba, mas essa condição impede o travamento do código em 10

        --igualando as os valores numéricos aos desenhados no display, através da variável 'pontuacaoD'          
        pontoPdisplayD :: PontuacaoD -> Picture
        pontoPdisplayD pontuacaoD
            | pontuacaoD == 0 = numero0d
            | pontuacaoD == 1 = numero1d
            | pontuacaoD == 2 = numero2d
            | pontuacaoD == 3 = numero3d
            | pontuacaoD == 4 = numero4d
            | pontuacaoD == 5 = numero5d
            | pontuacaoD == 6 = numero6d
            | pontuacaoD == 7 = numero7d
            | pontuacaoD == 8 = numero8d
            | pontuacaoD == 9 = numero9d
            | pontuacaoD == 10 = numero9d -- aqui a partida acaba, mas essa condição impede o travamento do código em 10

        -- desenha a bolinha na posição 'posiboll' (na verdade é um quadrado, para ser similar ao original do ATARI)
        ball :: JogoEstado -> Picture
        ball   jogoEstado =
            if jogoEstado == Jogando  then uncurry translate (posiboll game) $ color white $ rectangleSolid 22 22
            else
                translate 1000 1000 $ color white $ rectangleSolid 22 22

        -- escreve na tela de início do jogo
        inicioDoJogo :: JogoEstado -> Picture
        inicioDoJogo estado
            |estado == Inicio = pictures [translate (-106) (-88) $ scale 0.2 0.2 $ color white $ text "Pressione ENTER",
                palavraPONG]
            |otherwise = translate (-106) (-88) $ scale 0.2 0.2 $ color white $ text ""

        fimDoJogoD :: JogoEstado -> Picture
        fimDoJogoD estado
            |estado == VitoriaD = pictures [translate (-180) (-88) $ scale 0.2 0.2 $ color white $ text "Vitoria da raquete da direita",
                translate (-120) (-120) $ scale 0.2 0.2 $ color white $ text "Obrigado por jogar!"]
            |otherwise = translate (-106) (-88) $ scale 0.2 0.2 $ color white $ text ""

        fimDoJogoE :: JogoEstado -> Picture
        fimDoJogoE estado
            |estado == VitoriaE = pictures [translate (-200) (-88) $ scale 0.2 0.2 $ color white $ text "Vitoria da raquete da esquerda",
                translate (-120) (-120) $ scale 0.2 0.2 $ color white $ text "Obrigado por jogar!"]
            |otherwise = translate (-106) (-88) $ scale 0.2 0.2 $ color white $ text ""

        -- cria um bloco escuro sobre as raquetes quando elas não devem estar visíveis
        escondeRaquetesD :: JogoEstado -> Picture
        escondeRaquetesD jogoEstado
            |jogoEstado == Pausa   = translate 320 0 $ color black $ rectangleSolid 40 578
            |jogoEstado == Jogando = translate 500 0 $ color black $ rectangleSolid 40 700
            |otherwise             = translate 320 0 $ color black $ rectangleSolid 40 578

        escondeRaquetesE :: JogoEstado -> Picture
        escondeRaquetesE jogoEstado
            |jogoEstado == Pausa   = translate (-320) 0 $ color black $ rectangleSolid 40 578
            |jogoEstado == Jogando = translate (-500) 0 $ color black $ rectangleSolid 40 700
            | otherwise            = translate (-320) 0 $ color black $ rectangleSolid 40 578

        -- cria um bloco preto sobre a rede quando ela não deve ser vista e escreve PAUSA, quando o jogo estiver pausado
        escondeRede :: JogoEstado -> Picture
        escondeRede jogoEstado
            | jogoEstado == Jogando = translate (-500) 0 $ color black $ rectangleSolid 40 700
            | jogoEstado == Pausa   = pictures [translate 0 0 $ color black $ rectangleSolid 22 578 , palavraPAUSA]
            | otherwise             = pictures [translate 0 0 $ color black $ rectangleSolid 22 578]

        escondePlacar :: JogoEstado -> Picture
        escondePlacar jogoEstado =
            if jogoEstado == Jogando
                then translate (-700) 0 $ color black $ rectangleSolid 264 132
            else
                translate 0 210 $ color black $ rectangleSolid 578 132

        (rAltura, rLargura) = raqTamanho game     -- iguala os tamanhos das raquetes a variável criada no início do código

        raquete :: Color -> Float -> Float -> Picture -- desenha a raquete
        raquete cor x y = pictures
            [
            translate x y $ color white $ rectangleSolid rAltura rLargura
            ]

-- função de atualização de estados a cada instante seguindo os requisitos da biblioteca gloss
atualizacao :: Float -> HaskellPong -> HaskellPong
atualizacao atualizacao game
    --verificação e movimento da raquete e da bolinha
    | movimentoRaquete = raqueteRebate . limiteTelaiRebate . contadorDePontosD . contadorDePontosE. jogoFinalizadoD . jogoFinalizadoE . moveRaquetes atualizacao . moveBall atualizacao $ game
    -- estado de pausa '1', retorna uma variável vazia, parando o movimento da raquete e da bolinha
    | jogoEstado game == Pausa  = game
    -- caso o movimento das raquetes esteja parado, continua atualizando o movimento da bolinha e as colisões 
    | otherwise        = raqueteRebate . limiteTelaiRebate . contadorDePontosD . contadorDePontosE . jogoFinalizadoD . jogoFinalizadoE . moveBall atualizacao $ game
    where
        -- movimento das raquetes é atualizado, caso a movimentação seja verificada verdadeira
        movimentoRaquete = fst (movJogador game) /= 0 || snd (movJogador game) /= 0

-- Inputs do teclado
-- 'Q' e 'W'    subida e descida do jogador da esquerda
-- 'Up'e 'Down' subida e descida do player da direita
-- 'P'          pausa
-- 'Enter'      Volta a partida depois de pausada 
entradas :: Event -> HaskellPong -> HaskellPong
entradas (EventKey (Char 'p') Up _ _) game             = game { jogoEstado = Pausa } -- coloca o jogo em pausa
entradas (EventKey (SpecialKey KeyEnter) Up _ _) game  = game { jogoEstado = Jogando} -- despausa o jogo
entradas (EventKey (Char 'w') Up _ _) game             = game { movJogador = (fst $ movJogador game, 0) }
entradas (EventKey (Char 'w') Down _ _) game           = game { movJogador = (fst $ movJogador game, 300) }
entradas (EventKey (Char 's') Up _ _) game             = game { movJogador = (fst $ movJogador game, 0) }
entradas (EventKey (Char 's') Down _ _) game           = game { movJogador = (fst $ movJogador game, -300) }
entradas (EventKey (SpecialKey KeyDown) Up _ _) game   = game { movJogador = (0, snd $ movJogador game) }
entradas (EventKey (SpecialKey KeyDown) Down _ _) game = game { movJogador = (-300, snd $ movJogador game) }
entradas (EventKey (SpecialKey KeyUp) Up _ _) game     = game { movJogador = (0, snd $ movJogador game) }
entradas (EventKey (SpecialKey KeyUp) Down _ _) game   = game { movJogador = (300, snd $ movJogador game) }
entradas _ game  = game
