module Tarefa3TestSpec (testesTarefa3) where

import LI12324
import Tarefa3
import Test.HUnit


blocos1 :: [[Bloco]]
blocos1 = [ [ Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio]
          , [ Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio]
          , [ Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio]
          , [ Vazio, Alcapao, Plataforma, Plataforma, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma, Vazio]
          , [ Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
          , [ Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
          , [ Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]]

gameMap1 :: Mapa
gameMap1 = Mapa ((8.5, 6.5), Este) (5, 1.5) blocos1

pl1 = Personagem (0.0,0.0) Jogador (8.5,7) Oeste (0.8,0.8) False False 10 0 (True, 10.0)

en1 = Personagem (0.0,0.0) Fantasma (8,7) Este (0.8,0.8) False True 10 0 (False, 0.0)
en2 = Personagem (0.0,0.0) Fantasma (8.7,7) Este (0.8,0.8) False True 10 0 (False, 0.0)

c1 = (Martelo, (5,1))

j1 = Jogo gameMap1 [en1,en2] [c1] pl1

j2 = Jogo gameMap1 [en1 {vida = 9}, en2] [c1] (pl1 {vida = 9, aplicaDano = (True, 9.0)})

teste1 = "T1: Inimigo e jogador perdem vidas." ~: j2 ~=? movimenta 100 1.0 j1

pl2 = Personagem (0.0,0.0) Jogador (5.2,1) Oeste (0.8,0.8) False False 10 0 (False, 0.0)

j3 = Jogo gameMap1 [] [c1] pl2

j4 = Jogo gameMap1 [] [] (pl2 {aplicaDano = (True, 10.0)})

teste2A = "T2A: Jogador apanha martelo e a flag fica True." ~: True ~=? (fst . aplicaDano . jogador $ movimenta 100 1.0 j3) 

teste2B = "T2B: Jogador apanha martelo e o tempo restante é maior que zero." ~: True ~=? (snd . aplicaDano . jogador $ movimenta 100 1.0 j3) > 0

pl3 = Personagem (0.0,0.0) Jogador (3.5,4) Oeste (0.8,0.8) True False 10 0 (False, 0.0)

j5 = Jogo gameMap1 [] [] pl3

teste3 = "T3: Jogador não cai quando esta na escada." ~: j5 ~=? movimenta 100 1.0 j5

pl4 = Personagem (-1.0,0.0) Jogador (0.5,10.5) Oeste (1,1) False False 10 0 (False, 0.0)

j6 = Jogo gameMap1 [] [] pl4

teste4 = "T4: Jogador não atravessa o limite do mapa." ~: j6 ~=? movimenta 100 1.0 j6

pl5 = Personagem (0.0,0.0) Jogador (5,7.6) Oeste (1,1) False False 10 0 (False, 0.0)
en3 = Personagem (0.0,0.0) Fantasma (2.5,7.6) Este (1,1) False True 10 0 (False, 0.0)

j7 = Jogo gameMap1 [en3] [] pl5

blocos2 :: [[Bloco]]
blocos2 = [ [ Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio]
          , [ Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio]
          , [ Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio]
          , [ Vazio, Alcapao, Plataforma, Plataforma, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Vazio]
          , [ Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
          , [ Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
          , [ Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]]

gameMap2 :: Mapa
gameMap2 = Mapa ((8.5, 6.5), Este) (5, 1.5) blocos2

j8 = Jogo gameMap2 [en3] [] pl5

teste5 = "T5: Alcapao e removido por jogador mas nao pelo inimigo." ~: j8 ~=? movimenta 100 1.0 j7

pl6 = Personagem (0.0,0.0) Jogador (5,1) Oeste (1,1) False False 10 0 (False, 0.0)
c2 = (Moeda, (5,1))

j9 = Jogo gameMap1 [] [c2] pl6

teste6 = "T6: Jogador apanha uma moeda" ~: True ~=? (pontos . jogador $ movimenta 100 1.0 j9) > (pontos . jogador $ j9)


testFantasmahit :: Test
testFantasmahit = TestList
    [ "Test 1: vida diminui" ~:
        fantasmahit [Personagem {tipo = Fantasma, vida = 3}] (Personagem {tipo = Jogador, posicao = (0, 0)}) ~?= [Personagem {tipo = Fantasma, vida = 2}]
    , "Test 2: sem colisão, sem perda de hp" ~:
        fantasmahit [Personagem {tipo = Fantasma, vida = 3}] (Personagem {tipo = Jogador, posicao = (10, 10)}) ~?= [Personagem {tipo = Fantasma, vida = 3}]
    ]


testHitboxMartelo :: Test
testHitboxMartelo = TestList
    [ "Test 1" ~:
        hitboxMartelo (Personagem {posicao = (1, 1), direcao = Este, tamanho = (1, 1), emEscada = False, aplicaDano = (True, 10.0)}) ~?= Just ((1, 0), (2, 2))
    , "Test 2" ~:
        hitboxMartelo (Personagem {posicao = (1, 1), direcao = Oeste, tamanho = (1, 1), emEscada = False, aplicaDano = (True, 10.0)}) ~?= Just ((0, 0), (1, 2))
    , "Test 3" ~:
        hitboxMartelo (Personagem {posicao = (1, 1), direcao = Este, tamanho = (1, 1), emEscada = False, aplicaDano = (False, 0.0)}) ~?= Nothing
    ]

testFantasmamortopontos :: Test
testFantasmamortopontos = TestList
    [ "Test 1: pontos aumentam" ~:
        fantasmamortopontos [Personagem {tipo = Fantasma, vida = 3}] (Personagem {tipo = Jogador, posicao = (0, 0), pontos = 100}) ~?= (Personagem {tipo = Jogador, posicao = (0, 0), pontos = 600})
    , "Test 2: pontos não aumentam" ~:
        fantasmamortopontos [Personagem {tipo = Fantasma, vida = 3}] (Personagem {tipo = Jogador, posicao = (10, 10), pontos = 100}) ~?= (Personagem {tipo = Jogador, posicao = (10, 10), pontos = 100})
    ]


testJogadorhit :: Test
testJogadorhit = TestList
    [ "Test 1: hp dimunui" ~:
        jogadorhit [Personagem {vida = 3, posicao = (0, 0)}] (1,2) (Personagem {vida = 2, posicao=(5,4)}) ~?= (Personagem {vida = 1, posicao=(1,2)})
    , "Test 2: hp não muda" ~:
        jogadorhit [Personagem {vida = 3, posicao = (0, 0)}] (1,2) (Personagem {vida = 3, posicao = (5,4)}) ~?= (Personagem {vida = 3, posicao = (5,4)})
    ]


testTiracole :: Test
testTiracole = TestList
    [ "Test 1:moeda desaparece" ~:
        tiracole [(Moeda, (1, 1))] (Personagem {posicao = (1, 1)}) ~?= []
    , "Test 2: moeda não desaparece" ~:
        tiracole [(Moeda, (1, 1))] (Personagem {posicao = (10, 10)}) ~?= [(Moeda, (1, 1))]
    ]

testApanhacole :: Test
testApanhacole = TestList
    [ "Test 1: pontos aumentam" ~:
        apanhacole [(Moeda, (1, 1))] (Personagem {posicao = (1, 1), velocidade = (0, 0),pontos = 100}) ~?= (Personagem {posicao = (1, 1), velocidade = (0, 0), pontos = 600})
    , "Test 2: pontos ficam iguais" ~:
        apanhacole [(Moeda, (1, 1))] (Personagem {posicao = (10, 10), velocidade = (0, 0), pontos = 100}) ~?= (Personagem {posicao = (10, 10), velocidade = (0, 0), pontos = 100})
    ]


testRemoveMartelo :: Test
testRemoveMartelo = TestList
    [ "Test 1: tempo diminui" ~:
        removeMartelo (Personagem {aplicaDano = (True, 5.0), velocidade = (0, 0)}) ~?= (Personagem {aplicaDano = (True, 4.75), velocidade = (0, 0)}) -- Assuming a frame rate of 60fps
    , "Test 2: tempo chega a zero" ~:
        removeMartelo (Personagem {aplicaDano = (True, 0.1) ,velocidade = (0, 0)}) ~?= (Personagem {aplicaDano = (False, 0.0), velocidade = (0, 0)})
    , "Test 3: tempo não muda" ~:
        removeMartelo (Personagem {aplicaDano = (False, 0.0), velocidade = (0, 0)}) ~?= (Personagem {aplicaDano = (False, 0.0), velocidade = (0, 0)})
    ]


testQueda :: Test
testQueda = TestList
    [ "Test 1: o personagem cai" ~:
        queda gameMap1 (0, -10) (Personagem {tipo = Jogador} {posicao = (5, 5), tamanho = (1, 1), velocidade = (0, 0), direcao = Norte, emEscada = False}) ~?=
        (Personagem {tipo = Jogador} {posicao = (5, 5), tamanho = (1, 1), velocidade = (0, -10), direcao = Sul, emEscada = False})
    , "Test 2: personagem fica igual" ~:
        queda gameMap1 (0, -10) (Personagem {tipo = Jogador} {posicao = (5, 5), tamanho = (1, 1), velocidade = (0, 0), direcao = Norte, emEscada = False}) ~?=
        (Personagem {tipo = Jogador} {posicao = (5, 5), tamanho = (1, 1), velocidade = (0, 0), direcao = Norte, emEscada = False})
    ]


testRemoveAlcapao :: Test
testRemoveAlcapao = TestList
    [ "Test 1" ~:
        removeAlcapao (Personagem {posicao = (5, 5), velocidade = (0, 0)}) gameMap1 ~?= gameMap1
    ]


testColisao :: Test
testColisao = TestList
    [ "Test 1" ~:
        colisao (Personagem {posicao = (5, 5), direcao = Este}) gameMap1 ~?= (Personagem {posicao = (5, 5), direcao = Este})
    , "Test 2" ~:
        colisao (Personagem {posicao = (5, 5), direcao = Oeste}) gameMap1 ~?= (Personagem {posicao = (5, 5), direcao = Oeste})
    , "Test 3" ~:
        colisao (Personagem {posicao = (5, 5), direcao = Norte, velocidade = (0, -1)}) gameMap1 ~?= (Personagem {posicao = (5, 5), direcao = Norte, velocidade = (0, -1)})
    ]


testVelocidades :: Test
testVelocidades = TestList
    [ "Test 1" ~:
        velocidades (Personagem {posicao = (5, 5), velocidade = (0, 0)}) ~?= (Personagem {posicao = (5, 5), velocidade = (0, 0)})
    , "Test 2" ~:
        velocidades (Personagem {posicao = (5, 5), velocidade = (2, 0), direcao = Este}) ~?= (Personagem {posicao = (5.01, 5), velocidade = (2, 0), direcao = Este})
    , "Test 3" ~:
        velocidades (Personagem {posicao = (5, 5), velocidade = (2, 2), direcao = Este}) ~?= (Personagem {posicao = (5.01, 5.01), velocidade = (2, 2), direcao = Este})
    ]

testMarioEscCheck :: Test
testMarioEscCheck = TestList
    [ "Test 1" ~:
        marioEscCheck gameMap1 (Personagem {posicao = (5, 5), velocidade = (0, 5), emEscada = True}) ~?= (Personagem {posicao = (5, 5), velocidade = (0, 0), emEscada = True})
    , "Test 2" ~:
        marioEscCheck gameMap1 (Personagem {posicao = (5, 5), velocidade = (0, 5), emEscada = False}) ~?= (Personagem {posicao = (5, 5), velocidade = (0, 5), emEscada = False})
    ]


testOverlap' :: Test
testOverlap' = TestList
    [ "Test 1" ~:
        overlap' (Just ((1, 1), (3, 3))) ((2, 2), (4, 4)) ~?= True
    , "Test 2" ~:
        overlap' (Just ((1, 1), (3, 3))) ((4, 4), (6, 6)) ~?= False
    , "Test 3" ~:
        overlap' Nothing ((2, 2), (4, 4)) ~?= False
    ]


testColisoesHitB :: Test
testColisoesHitB = TestList
    [ "Test 1" ~:
        colisoesHitB (1, 1) (2, 2) ~?= True
    , "Test 2" ~:
        colisoesHitB (1, 1) (4, 4) ~?= False
    ]



testesTarefa3 = test [teste1, teste2A, teste2B, teste3, teste4, teste5, teste6,testFantasmahit, testHitboxMartelo, testFantasmamortopontos,
 testJogadorhit, testTiracole, testApanhacole , testRemoveMartelo, testQueda, testRemoveAlcapao , testColisao, testVelocidades, testMarioEscCheck ,testOverlap',testColisoesHitB ]
