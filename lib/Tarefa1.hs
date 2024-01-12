{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Ivo Filipe Mendes Vieira <a103999@alunos.uminho.pt>
              Filipa Cosquete Santos <a106837@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 where

import LI12324



--|  Calcula a hitbox de um personagem.
hitboxPersonagem :: Personagem -> Hitbox
hitboxPersonagem p = let (x, y) = posicao p
                         (w, h) = tamanho p
                     in  ((x - w/2, y - h/2), (x + w/2, y + h/2))


{-|Testa se um personagem colide com as paredes ou plataformas do mapa.
O valor True indica que o personagem colidiu com alguma parede ou plataforma, e o valor False indica que não houve colisão-}
colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede (Mapa _ _ blocos) personagem =
  let hitboxPers = hitboxPersonagem personagem
      (posX, posY) = posicao personagem
      (width, height) = tamanho personagem
      (blocoW, blocoH) = (1.0, 1.0)  -- Assume que cada bloco tem tamanho 1x1
      (mapWidth, mapHeight) = (fromIntegral $ length (head blocos), fromIntegral $ length blocos)
      (blocoX, blocoY) = (floor posX, floor posY)
  in
    --| Verifica colisão com paredes laterais
    posX - width/2 < 0 || posX + width/2 > mapWidth ||
    --| Verifica colisão com o topo do mapa
    posY + height/2 > mapHeight ||
    --| Verifica colisão com plataformas
    case safeGet blocoX blocoY blocos of
      Just Plataforma -> posY - height/2 < fromIntegral blocoY + blocoH
      _               -> False

{-| Função auxiliar para obter um elemento de uma matriz com verificação de limites
Ela retorna o elemento da matriz na linha e coluna especificadas, ou Nothing se a linha ou coluna estiver fora dos limites da matriz-}
safeGet :: Int -> Int -> [[a]] -> Maybe a
safeGet x y m =
  if x >= 0 && y >= 0 && x < length (head m) && y < length m
    then Just ((m !! y) !! x)
    else Nothing



{-|Testa se dois 'Personagens' colidem, True indica que os personagens colidiram, e o valor False indica que não houve colisão-}
colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens p1 p2 =
  let hitboxP1 = hitboxPersonagem p1
      hitboxP2 = hitboxPersonagem p2
  in
    overlap hitboxP1 hitboxP2

{-|Função auxiliar que verifica se duas hitboxes se sobrepõem, o valor True indica que as hitboxes se sobrepõem, e o valor False indica que não se sobrepõem-}
overlap :: Hitbox -> Hitbox -> Bool
overlap ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) =
  x1 < x4 && x2 > x3 && y1 < y4 && y2 > y3



