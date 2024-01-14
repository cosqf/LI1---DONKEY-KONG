module Tarefa2TestSpec (testesTarefa2) where

import LI12324
import Tarefa2
import Test.HUnit

test_chao = test [
    "Teste 1" ~: True ~=? chao (Mapa ((0.5, 5.5), Oeste) (0.5, 2.5) [[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],[Plataforma, Plataforma, Vazio, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]])
    ,"Teste 2" ~: False ~=? chao (Mapa ((1.0, 2.0), Oeste) (3.0, 2.5) [[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Plataforma]])
    ,"Teste 3" ~: True ~=? chao (Mapa ((1.1,3.0), Este) (5.0,5.0) [[Plataforma, Plataforma,Plataforma, Plataforma, Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio],[Plataforma, Plataforma,Plataforma, Plataforma, Plataforma]])
                ]

test_validaJogador = test
  [ "Teste 1" ~: True ~=? validaJogador (Personagem (0.5, 0) Jogador (0.5, 2.5) Este (1, 1) False False 3 300 (False, 0))
  , "Teste 2" ~: False ~=? validaJogador (Personagem (-0.5, 0) Jogador (0.5, 5.5) Oeste (1, 1) False True 3 500 (True, 10))
  , "Teste 3" ~: True ~=? validaJogador (Personagem (1.1,0) Jogador (1.1,3.0) Este (1, 1) False False 3 0 (True, 5))
  ]

test_validaInimigo = test
  [ "Teste 1" ~: False ~=? validaInimigo [Personagem (1.1,0) Fantasma (1, 1) Este (1, 1) False False 3 0 (False, 0), Personagem (-5.0, 0) Fantasma (1, 1) Oeste (1, 1) False True 3 0 (False, 0)]
  , "Teste 2" ~: True ~=? validaInimigo [Personagem (0.5, 0) Fantasma (0.5, 2.5) Este (1, 1) False True 3 0 (False, 0), Personagem (0, 0) Fantasma (1, 1) Este (1, 1) False True 3 0 (False, 0)]
  , "Teste 3" ~: True ~=? validaInimigo []
  ]

test_posicaoI = test
  [ "Teste 1" ~: True ~=? posicaoI [Personagem (-5.0, 0) Fantasma (1, 1) Oeste (1, 1) False True 3 0 (False, 0)] ( Personagem  { velocidade = (0.0, 0.0),tipo = Jogador,posicao = (5,5),direcao = Oeste, tamanho = (0.8, 0.8),emEscada = False, ressalta = False, vida = 10, pontos = 0, aplicaDano = (False, 0)}) (Mapa ((5,5), Norte) (0,0) [[Plataforma, Plataforma,Plataforma, Plataforma, Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio],[Plataforma, Plataforma,Plataforma, Plataforma, Plataforma]])
  , "Teste 2" ~: False ~=? posicaoI [Personagem (0.5, 0) Fantasma (0.5, 2.5) Este (1, 1) False True 3 0 (False, 0), Personagem (0, 0) Fantasma (1,1) Este (1, 1) False True 3 0 (False, 0)] ( Personagem  { velocidade = (0.0, 0.0),tipo = Jogador,posicao = (1,1),direcao = Oeste, tamanho = (0.8, 0.8),emEscada = False, ressalta = False, vida = 10, pontos = 0, aplicaDano = (False, 0)}) (Mapa ((1,1), Norte) (1,1) [[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Plataforma]])
  , "Teste 3" ~: True ~=? posicaoI [] ( Personagem  { velocidade = (0.0, 0.0),tipo = Jogador,posicao = (1,1),direcao = Oeste, tamanho = (0.8, 0.8),emEscada = False, ressalta = False, vida = 10, pontos = 0, aplicaDano = (False, 0)}) (Mapa ((1,1), Norte) (0,0) [[Vazio]])
  ]


mapa01 :: Mapa
mapa01 =
  Mapa
    ((8.5, 6.5), Este)
    (5, 1.5)
    [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio],
      [Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio],
      [Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio],
      [Vazio, Plataforma, Plataforma, Plataforma, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma, Vazio],
      [Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],
      [Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],
      [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ]

inimigoModelo =
  Personagem
    { velocidade = (0.0, 0.0),
      tipo = Fantasma,
      posicao = (2.5, 7.6),
      direcao = Este,
      tamanho = (1, 1),
      emEscada = False,
      ressalta = True,
      vida = 1,
      pontos = 0,
      aplicaDano = (False, 0)
    }

jogadorParado =
  Personagem
    { velocidade = (0.0, 0.0),
      tipo = Jogador,
      posicao = (8.5, 6.5),
      direcao = Oeste,
      tamanho = (0.8, 0.8),
      emEscada = False,
      ressalta = False,
      vida = 10,
      pontos = 0,
      aplicaDano = (False, 0)
    }

jogo01 :: Jogo
jogo01 =
  Jogo
    { mapa = mapa01,
      inimigos = [inimigoModelo, inimigoModelo],
      colecionaveis = [],
      jogador = jogadorParado
    }

teste01 :: Test
teste01 = "T01: Jogo que respeita todas as regras é válido" ~: True ~=? valida jogo01

mapa02 :: Mapa
mapa02 =
  Mapa
    ((8.5, 6.5), Este)
    (5, 1.5)
    [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio],
      [Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio],
      [Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio],
      [Vazio, Plataforma, Plataforma, Plataforma, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma, Vazio],
      [Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],
      [Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],
      [Plataforma, Plataforma, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ]

teste02 :: Test
teste02 = "T02: Jogo é inválido porque não tem chão completo" ~: False ~=? valida jogo01 {mapa = mapa02}

teste03 :: Test
teste03 = TestLabel "T03" $ test [testeA, testeB]
  where
    testeA = "A: Personagens do tipo Fantasma que não ressaltam tornam o jogo inválido" ~: False ~=? valida jogo01 {inimigos = [inimigoModelo {ressalta = False}, inimigoModelo]}
    testeB = "B: Jogador não pode ressaltar" ~: False ~=? valida jogo01 {jogador = jogadorParado {ressalta = True}}

teste04 :: Test
teste04 = "T04: Se na posição inicial um inimigo colidir com a posição inicial do jogador, o jogo é inválido" ~: False ~=? valida jogo01 {inimigos = [inimigoModelo {posicao = (8.0, 6.0)}, inimigoModelo]}

teste05 :: Test
teste05 = "T05: Para o jogo ser válido precisa de ter pelo menos dois inimigos" ~: False ~=? valida jogo01 {inimigos = [inimigoModelo]}

teste06 :: Test
teste06 = "T06: Os fantasmas começam com 1 vida" ~: False ~=? valida jogo01 {inimigos = [inimigoModelo {vida = 2}, inimigoModelo]}

mapa03 :: Mapa
mapa03 =
  Mapa
    ((8.5, 6.5), Este)
    (5, 1.5)
    [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Alcapao, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio],
      [Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio],
      [Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio],
      [Vazio, Plataforma, Plataforma, Plataforma, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma, Vazio],
      [Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],
      [Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],
      [Plataforma, Plataforma, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ]

teste07 :: Test
teste07 = "T07: As escadas não podem terminar em Alcapao" ~: False ~=? valida jogo01 {mapa = mapa03}


test_numI :: Test
test_numI = "Teste numI" ~:
  test
    [ "Teste 1" ~: True ~=? numI [Personagem { vida = 1 }, Personagem { vida = 1 }]
    , "Teste 2" ~: False ~=? numI [Personagem { vida = 1 }]
    , "Teste 3" ~: True ~=? numI [Personagem { vida = 1 }, Personagem { vida = 1 }, Personagem { vida = 1 }]
    ]

test_iniVida :: Test
test_iniVida = "Teste iniVida" ~:
  test
    [ "Teste 1" ~: True ~=? iniVida [Personagem { tipo = MacacoMalvado, vida = 1 }]
    , "Teste 2" ~: False ~=? iniVida [Personagem { tipo = Fantasma, vida = 2 }]
    , "Teste 3" ~: True ~=? iniVida [Personagem { tipo = Fantasma, vida = 1 }]
    , "Teste 4" ~: False ~=? iniVida [Personagem { tipo = MacacoMalvado, vida = 2 }]
    , "Teste 5" ~: True ~=? iniVida [Personagem { tipo = Jogador, vida = 1 }]
    , "Teste 6" ~: False ~=? iniVida [Personagem { tipo = Jogador, vida = 2 }]
    ]


test_ehPlataforma :: Test
test_ehPlataforma = "Teste ehPlataforma" ~:
  test
    [ "Teste 1" ~: True ~=? ehPlataforma Plataforma
    , "Teste 2" ~: False ~=? ehPlataforma Vazio
    , "Teste 3" ~: False ~=? ehPlataforma Alcapao
    ]

test_ehAlcapao :: Test
test_ehAlcapao = "Teste ehAlcapao" ~:
  test
    [ "Teste 1" ~: True ~=? ehAlcapao Alcapao
    , "Teste 2" ~: False ~=? ehAlcapao Vazio
    , "Teste 3" ~: False ~=? ehAlcapao Plataforma
    ]



test_escadaValida :: Test    --podia ter posto os mapas já definidos , mas estava cansado para confirmar 
test_escadaValida = "Teste escadaValida" ~:
  test
    [ "Teste 1" ~: True ~=? escadaValida (Mapa ((0, 0), Norte) (0, 0) [[Escada, Escada, Escada], [Plataforma, Plataforma, Plataforma]])
    , "Teste 2" ~: True ~=? escadaValida (Mapa ((0, 0), Norte) (0, 0) [[Escada, Escada], [Plataforma, Plataforma]])
    , "Teste 3" ~: False ~=? escadaValida (Mapa ((0, 0), Norte) (0, 0) [[Escada, Plataforma, Escada], [Plataforma, Plataforma, Plataforma]])
    ]


test_checkmario :: Test
test_checkmario = "Teste checkmario" ~:
  test
    [ "Teste 1" ~: True ~=? checkmario (Personagem { posicao = (0, 0) }) (Mapa ((0, 0), Norte) (0, 0) [[Vazio]])
    , "Teste 2" ~: True ~=? checkmario (Personagem { posicao = (0, 0) }) (Mapa ((0, 0), Norte) (0, 0) [[Escada]])
    , "Teste 3" ~: False ~=? checkmario (Personagem { posicao = (0, 0) }) (Mapa ((0, 0), Norte) (0, 0) [[Plataforma]])
    ]






testesTarefa2 :: Test
testesTarefa2 = TestLabel "Tarefa2 (valida)" $ test [teste01, teste02, teste03, teste04, teste05, teste06, teste07, test_numI, test_iniVida ,test_ehPlataforma,
 test_ehAlcapao, test_escadaValida, test_checkmario]
