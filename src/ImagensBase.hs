module ImagensBase where

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

--Criando um display 7 seguimentos a direita (que simula os números como em um display eletrônico)
dd1 :: Picture --linha equivalente a 'd' em um display digital de 7 seguimentos
dd1 = pictures
    [ translate 123 158 $ color white $ rectangleSolid 66 22]
dg1 :: Picture --linha equivalente a 'g' em um display digital de 7 seguimentos 
dg1 = pictures
    [ translate 123 202 $ color white $ rectangleSolid 66 22]
da1 :: Picture --linha equivalente a 'a' em um display digital de 7 seguimentos
da1 = pictures
    [ translate 123 246 $ color white $ rectangleSolid 66 22]
df1 :: Picture --linha equivalente a 'f' em um display digital de 7 seguimentos
df1 = pictures
    [ translate 101 224 $ color white $ rectangleSolid 22 66]
db1 :: Picture --linha equivalente a 'b' em um display digital de 7 seguimentos
db1 = pictures
    [ translate 145 224 $ color white $ rectangleSolid 22 66]
dc1 :: Picture --linha equivalente a 'c' em um display digital de 7 seguimentos
dc1 = pictures
    [ translate 145 180 $ color white $ rectangleSolid 22 66]
de1 :: Picture --linha equivalente a 'e' em um display digital de 7 seguimentos
de1 = pictures
    [ translate 101 180 $ color white $ rectangleSolid 22 66]

--Criando um display 7 seguimentos a esquerda (que simula os números como em um display eletrônico)
dd2 :: Picture --linha equivalente a 'd' em um display digital de 7 seguimentos
dd2 = pictures
    [ translate (-123) 158 $ color white $ rectangleSolid 66 22]
dg2 :: Picture --linha equivalente a 'g' em um display digital de 7 seguimentos
dg2 = pictures
    [ translate (-123) 202 $ color white $ rectangleSolid 66 22]
da2 :: Picture --linha equivalente a 'a' em um display digital de 7 seguimentos
da2 = pictures
    [ translate (-123) 246 $ color white $ rectangleSolid 66 22]
db2 :: Picture --linha equivalente a 'b' em um display digital de 7 seguimentos
db2 = pictures
    [ translate (-101) 224 $ color white $ rectangleSolid 22 66]
df2 :: Picture --linha equivalente a 'f' em um display digital de 7 seguimentos
df2 = pictures
    [ translate (-145) 224 $ color white $ rectangleSolid 22 66]
de2 :: Picture --linha equivalente a 'e' em um display digital de 7 seguimentos
de2 = pictures
    [ translate (-145) 180 $ color white $ rectangleSolid 22 66]
dc2 :: Picture --linha equivalente a 'c' em um display digital de 7 seguimentos
dc2 = pictures
    [ translate (-101) 180 $ color white $ rectangleSolid 22 66]

--numeros formados a direita (respectivamente "acendendo" as linhas no display para cada número correspondente)
numero0d = pictures [da1,dd1,df1,db1,dc1,de1]
numero1d = pictures [df1,de1]
numero2d = pictures [da1,dg1,dd1,db1,de1]
numero3d = pictures [da1,dg1,dd1,db1,dc1]
numero4d = pictures [dg1,df1,db1,dc1]
numero5d = pictures [da1,dg1,dd1,df1,dc1]
numero6d = pictures [da1,dg1,dd1,df1,dc1,de1]
numero7d = pictures [da1,db1,dc1]
numero8d = pictures [da1,dg1,dd1,df1,db1,dc1,de1]
numero9d = pictures [da1,dg1,dd1,df1,db1,dc1]

--numeros formados a esquerda (respectivamente "acendendo" as linhas no display para cada número correspondente)
numero0e = pictures [da2,dd2,df2,db2,dc2,de2]
numero1e = pictures [db2,dc2]
numero2e = pictures [da2,dg2,dd2,db2,de2]
numero3e = pictures [da2,dg2,dd2,db2,dc2]
numero4e = pictures [dg2,df2,db2,dc2]
numero5e = pictures [da2,dg2,dd2,df2,dc2]
numero6e = pictures [da2,dg2,dd2,df2,dc2,de2]
numero7e = pictures [da2,db2,dc2]
numero8e = pictures [da2,dg2,dd2,df2,db2,dc2,de2]
numero9e = pictures [da2,dg2,dd2,df2,db2,dc2]

--escrevendo PAUSA
--U -> começando pelo meio
letraU1 :: Picture
letraU1 = pictures
    [ translate 0 (-44) $ color white $ rectangleSolid 66 22]

letraU2 :: Picture
letraU2 = pictures
    [ translate 22 0 $ color white $ rectangleSolid 22 110]

letraU3 :: Picture
letraU3 = pictures
    [ translate (-22) 0 $ color white $ rectangleSolid 22 110]

letraA1 :: Picture
letraA1 = pictures
    [ translate (-66) 0 $ color white $ rectangleSolid 22 110]

letraA2 :: Picture
letraA2 = pictures
    [ translate (-110) 0 $ color white $ rectangleSolid 22 110]

letraA3 :: Picture
letraA3 = pictures
    [ translate (-88) 44 $ color white $ rectangleSolid 66 22]

letraA4 :: Picture
letraA4 = pictures
    [ translate (-88) 0 $ color white $ rectangleSolid 66 22]

letraP1 :: Picture
letraP1 = pictures
    [ translate (-177) 44 $ color white $ rectangleSolid 66 22]

letraP2 :: Picture
letraP2 = pictures
    [ translate (-177) 0 $ color white $ rectangleSolid 66 22]

letraP3 :: Picture
letraP3 = pictures
    [ translate (-155) 22 $ color white $ rectangleSolid 22 66]

letraP4 :: Picture
letraP4 = pictures
    [ translate (-199) 0 $ color white $ rectangleSolid 22 110]

letraS1 :: Picture
letraS1 = pictures
    [ translate 66 22 $ color white $ rectangleSolid 22 66]

letraS2 :: Picture
letraS2 = pictures
    [ translate 110 (-22) $ color white $ rectangleSolid 22 66]

letraS3 :: Picture
letraS3 = pictures
    [ translate 88 44 $ color white $ rectangleSolid 66 22]

letraS4 :: Picture
letraS4 = pictures
    [ translate 88 0 $ color white $ rectangleSolid 66 22]

letraS5 :: Picture
letraS5 = pictures
    [ translate 88 (-44) $ color white $ rectangleSolid 66 22]

letra2A1 :: Picture
letra2A1 = pictures
    [ translate 154 0 $ color white $ rectangleSolid 22 110]

letra2A2 :: Picture
letra2A2 = pictures
    [ translate 198 0 $ color white $ rectangleSolid 22 110]

letra2A3 :: Picture
letra2A3 = pictures
    [ translate 176 44 $ color white $ rectangleSolid 66 22]

letra2A4 :: Picture
letra2A4 = pictures
    [ translate 176 0 $ color white $ rectangleSolid 66 22]

--o texto que será inserido, avisando o botão para voltar a partida
textoAperteEnter :: Picture
textoAperteEnter = translate (-106) (-88) $ scale 0.2 0.2 $ color white $ text "Pressione ENTER"

-- o que apresentar quando "palavraPAUSA" for chamado
palavraPAUSA = pictures [letraU1, letraU2, letraU3, letraA1, letraA2, letraA3 , letraA4, letraP1,
    letraP2, letraP3, letraP4, letraS1, letraS2, letraS3, letraS4, letraS5, letra2A1, letra2A2,
    letra2A3 , letra2A4, textoAperteEnter ]

-- escrevendo o título inicial "PONG"
letra2P1 :: Picture
letra2P1 = pictures
    [ translate (-132) 44 $ color white $ rectangleSolid 66 22]

letra2P2 :: Picture
letra2P2 = pictures
    [ translate (-132) 0 $ color white $ rectangleSolid 66 22]

letra2P3 :: Picture
letra2P3 = pictures
    [ translate (-110) 22 $ color white $ rectangleSolid 22 66]

letra2P4 :: Picture
letra2P4 = pictures
    [ translate (-154) 0 $ color white $ rectangleSolid 22 110]

letraO1 :: Picture
letraO1 = pictures
    [ translate (-21) 0 $ color white $ rectangleSolid 22 110]

letraO2 :: Picture
letraO2 = pictures
    [ translate (-65) 0 $ color white $ rectangleSolid 22 110]

letraO3 :: Picture
letraO3 = pictures
    [ translate (-43) (-44) $ color white $ rectangleSolid 66 22]

letraO4 :: Picture
letraO4 = pictures
    [ translate (-43) 44  $ color white $ rectangleSolid 66 22]

letraN1 :: Picture
letraN1 = pictures
    [ translate 45 44 $ color white $ rectangleSolid 66 22]

letraN2 :: Picture
letraN2 = pictures
    [ translate 68 0 $ color white $ rectangleSolid 22 110]

letraN3 :: Picture
letraN3 = pictures
    [ translate 23 0 $ color white $ rectangleSolid 22 110]

letraG1 :: Picture
letraG1 = pictures
    [ translate 112 0 $ color white $ rectangleSolid 22 110]

letraG2 :: Picture
letraG2 = pictures
    [ translate 156 (-33) $ color white $ rectangleSolid 22 44]

letraG3 :: Picture
letraG3 = pictures
    [ translate 134 (-44) $ color white $ rectangleSolid 66 22]

letraG4 :: Picture
letraG4 = pictures
    [ translate 134 44  $ color white $ rectangleSolid 66 22]

palavraPONG = pictures [letra2P1, letra2P2, letra2P3, letra2P4, letraO1, letraO2,
    letraO3, letraO4, letraN1, letraN2, letraN3, letraG1, letraG2, letraG3, letraG4]

--desenha parede a direita
paredeDireita :: Picture
paredeDireita = pictures
    [ translate 375 0 $ color white $ rectangleSolid 22 600]

--desenha parede a esquerda
paredeEsquerda :: Picture
paredeEsquerda  = pictures
    [ translate (-375) 0 $ color white $ rectangleSolid 22 600]

--criação da rede no centro (quadrados centrais que dividem os campos dos jogadores)
rede :: Float -> Picture
rede posidisplay =
    translate 0 posidisplay $ color redeColor $ rectangleSolid lado lado

lado = 22
redeColor = white
distancia = -243

distanciaInicial    = -287 -- posição do primeiro quadrado da rede
distanciaEntreredes = lado*2   -- distância de cada um = dobro da própria altura

redes     = pictures
    [
    -- a distância entre cada quadrado = distância do quadrado inferior + a distancia entre eles * posição de cada um
    rede $ distanciaInicial + distanciaEntreredes * x | x <- [0 ..13]
    ]

limiteTelai :: Float -> Picture -- desenha a linha superior e inferior que delimitam a área do jogo
limiteTelai posidisplay =
    translate 0 posidisplay $ color limiteTelaiColor $ rectangleSolid larguraLimite alturaFloat

larguraLimite = 768 -- largura da linha superior da tela

alturaFloat :: Float
alturaFloat = fromInteger $ toInteger $ alturaLimite $ mapFuncao crialista -- transforma o length de Int pata Float
    
alturaLimite s = length s  -- mede o comprimento da lista

mapFuncao :: a -> [Float] -- um exemplo de uso da função map aplicada na lista que foi criada
mapFuncao xs = map quadrado xs -- usando função de alta ordem (função que chama função)
    where
        quadrado x = x * x
        xs = crialista

crialista ::[Float] -- altura da linha superior da tela, medido em 22, por uma lista de 22 elementos
crialista = [x | x <- [1 .. 22]]

limiteTelaiColor = white
limiteTela       = pictures [limiteTelai 300, limiteTelai (-300)] -- desenha as duas paredes em y = 300 e y = -300
