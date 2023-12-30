{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Ivo Filipe Mendes Vieira <a103999@alunos.uminho.pt>
              Filipa Cosquete Santos <a106837@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 where

import LI12324



--  Calcula a hitbox de um 'Personagem'.
hitboxPersonagem :: Personagem -> Hitbox
hitboxPersonagem p = let 
                      (x, y) = posicao p
                      (w, h) = tamanho p
                    in ((x - w/2, y - h/2), (x + w/2, y + h/2))


-- Testa se um 'Personagem' colide com as paredes ou plataformas do 'Mapa'.
colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede (Mapa _ _ blocos) personagem =
  let hitboxPers = hitboxPersonagem personagem
      (posX, posY) = posicao personagem
      (width, height) = tamanho personagem
      (blocoW, blocoH) = (1.0, 1.0)  -- Assume que cada bloco tem tamanho 1x1
      (mapWidth, mapHeight) = (fromIntegral $ length (head blocos), fromIntegral $ length blocos)
      (blocoX, blocoY) = (floor posX, floor posY)
  in
    -- Verifica colisão com paredes laterais
    posX - width/2 < 0 || posX + width/2 > mapWidth ||
    -- Verifica colisão com o topo do mapa
    posY + height/2 > mapHeight ||
    -- Verifica colisão com plataformas
    case safeGet blocoX blocoY blocos of
      Just Plataforma -> posY - height/2 < fromIntegral blocoY + blocoH
      _               -> False

-- Função auxiliar para obter um elemento de uma matriz com verificação de limites
safeGet :: Int -> Int -> [[a]] -> Maybe a
safeGet x y m =
  if x >= 0 && y >= 0 && x < length (head m) && y < length m
    then Just ((m !! y) !! x)
    else Nothing



-- Testa se dois 'Personagens' colidem.
colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens p1 p2 =
  let hitboxP1 = hitboxPersonagem p1
      hitboxP2 = hitboxPersonagem p2
  in
    overlap hitboxP1 hitboxP2

-- Função auxiliar que verifica se duas hitboxes se sobrepõem
overlap :: Hitbox -> Hitbox -> Bool
overlap ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) =
  x1 < x4 && x2 > x3 && y1 < y4 && y2 > y3



{- era só a definição de um teste mas podes ignorar

let mapaTeste = Mapa ((0, 0), Norte) (5, 5)
                 [ [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
                 , [Plataforma, Vazio, Vazio, Escada, Plataforma]
                 , [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
                 ]

  -- Criar personagens para testar
  let personagem1 = Personagem { velocidade = (0, 0), tipo = Jogador, posicao = (1.5, 1.5), direcao = Norte, tamanho = (1, 1), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False, 0) }
  let personagem2 = Personagem { velocidade = (0, 0), tipo = Jogador, posicao = (4.5, 3.5), direcao = Norte, tamanho = (1, 1), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False, 0) }

-}
 
