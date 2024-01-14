module Funcoes where 
import LI12324 
import Data.List (elemIndices)
import Tarefa1 (overlap)

blocodirecao :: Personagem -> Direcao -> Mapa -> Bloco     --indica o bloco posicionado no lado norte/sul/etc do personagem
blocodirecao (Personagem {posicao= (x,y)}) Norte mapa = blocopos (x,y-1) mapa
blocodirecao (Personagem {posicao= (x,y)}) Sul mapa = blocopos (x,y+1) mapa
blocodirecao (Personagem {posicao= (x,y)}) Oeste mapa = blocopos (x-1,y) mapa
blocodirecao (Personagem {posicao= (x,y)}) Este mapa = blocopos (x+1,y) mapa

-- indica o bloco na coordenada (x,y)
blocopos :: Posicao -> Mapa -> Bloco    
blocopos (x, y) (Mapa _ _ mapa)
    |y<0 || y>fromIntegral maxy=Vazio      -- se estiver fora do mapa dá vazio
    |x<0 || x>fromIntegral maxx=Vazio
    | otherwise = (mapa !! floor y !! floor x)
        where
            maxy = length mapa
            maxx = length (head mapa)

        
--calcula todas as posicoes de um bloco no mapa
posb :: Mapa -> Bloco -> [Posicao] 
posb (Mapa _ _ l) b = aux (map (elemIndices b) l) 1.0
  where
    aux :: [[Int]] -> Double -> [Posicao]
    aux [] _ = []
    aux ([]:c) x = aux c (x + 1)
    aux ((a:b):c) x = (x, fromIntegral a) : aux (b:c) x

-- | Tamanho dos blocos por pixel
blockSize :: Float
blockSize = 80

-- | Tamanho e comprimento do mapa em pixeis
tamanhoCompMapa :: Mapa -> (Float,Float)
tamanhoCompMapa (Mapa _ _  mapa) = (fromIntegral (length (head mapa)) * blockSize, fromIntegral (length mapa) * blockSize)


-- | Testa se duas posições colidem.
colisoesposicoes :: Posicao -> (Double, Double) -> Posicao -> (Double, Double) -> Bool
colisoesposicoes (x,y) (w,h) (x2,y2) (w2,h2)=
  let 
    hitbox1 = ((x - w/2, y - h/2), (x + w/2, y + h/2)) 
    hitbox2 = ((x2 - w2/2, y2 - h2/2), (x2 + w2/2, y2 + h2/2)) 
 in
    overlap hitbox1 hitbox2
