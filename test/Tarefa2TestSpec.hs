module Tarefa2TestSpec where

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

testesTarefa2 :: Test
testesTarefa2 = TestLabel "Tarefa2 (valida)" $ test [teste01, teste02, teste03, teste04, teste05, teste06, teste07]