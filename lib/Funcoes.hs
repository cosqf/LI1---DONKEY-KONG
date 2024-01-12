module Funcoes where 
import LI12324 
import Data.List (elemIndices)

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
    |x<1 && y<1 = fun x y
    |x<1 = fun x (y-1)
    |y<1 = fun (x-1) y
    | otherwise = (mapa !! ceiling (y-1)) !! ceiling (x-1)
        where
            maxy = length mapa
            maxx = length (head mapa)
            fun x y = (mapa !! floor y) !! floor x
        
--calcula todas as posicoes de um bloco no mapa
posb :: Mapa -> Bloco -> [Posicao] 
posb (Mapa _ _ l) b = aux (map (elemIndices b) l) 1.0
  where
    aux :: [[Int]] -> Double -> [Posicao]
    aux [] _ = []
    aux ([]:c) x = aux c (x + 1)
    aux ((a:b):c) x = (x, fromIntegral a) : aux (b:c) x
