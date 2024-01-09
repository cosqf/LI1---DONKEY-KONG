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
c2 = (Martelo, (5,1))

j9 = Jogo gameMap1 [] [c2] pl6

teste6 = "T6: Jogador apanha uma moeda" ~: True ~=? (pontos . jogador $ movimenta 100 1.0 j9) > (pontos . jogador $ j9)

testesTarefa3 = test [teste1, teste2A, teste2B, teste3, teste4, teste5, teste6]
