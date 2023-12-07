{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : Ivo Filipe Mendes Vieira <a103999@alunos.uminho.pt>
              Filipa Cosquete Santos <a106837@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where

import LI12324
import Data.List (elemIndices, group)

valida :: Jogo -> Bool
valida = undefined

{-
1. O mapa tem “chão”, i.e. uma plataforma que impede que o jogador            chao
ou outro personagem caia fora do mapa.

2. Todos os inimigos têm a propriedade ressalta a True, enquanto que       validaJogador, validaInimigo
o jogador a tem a False.

3. A posição inicial de um jogador não pode colidir com a posição inicial
de um outro personagem. Note que as posições iniciais de inimigos              posicaoI
podem colidir entre estes.

4. Número mı́nimo de inimigos: 2 (dois.)                                                  numI

5. Inimigos Fantasma têm exactamente 1 (uma) vida                            iniVida

6. Escadas não podem começar/terminar em alçapões, e pelo menos uma
das suas extremidades tem que ser do tipo Plataforma.

7. Alçapões não podem ser menos largos que o jogador.

8. Não podem existir personagens nem coleccionáveis “dentro” de plata-
formas ou alçapões, i.e. o bloco (na matriz do mapa) correspendente
à posição de um personagem ou objecto tem que ser Vazio.
-}

--exemplo
--mario = Personagem (5,5) Jogador (0,0) Este (1,1) False False 3 500 (False,0)

chao :: Mapa -> Bool
chao (Mapa _ _ m) = all (==Plataforma) (last m)


validaJogador :: Personagem -> Bool
validaJogador (Personagem {ressalta=r}) = not r

validaInimigo :: [Personagem] -> Bool
validaInimigo [] = True
validaInimigo (Personagem {ressalta=r}:xs) = r && validaInimigo xs


posicaoI :: [Personagem] -> Mapa -> Bool
posicaoI [] m = True
posicaoI (Personagem {posicao = pf}:xs) m@(Mapa (pm,_) _ _) = pf==pm && posicaoI xs m

numI :: [Personagem] -> Bool    --numero de inimigos
numI l = length l >=2 

iniVida :: [Personagem] -> Bool
iniVida [] = True
iniVida (Personagem {tipo = MacacoMalvado}:xs) = iniVida xs
iniVida (Personagem {tipo = fantasma, vida=v}:xs) = v==1 && iniVida xs

--eu vou acabar essa
--alcapaoL ::Mapa -> Personagem -> Bool
--alcapaoL (Mapa _ _ l) Personagem {tamanho=(x,y)} = filter Alcapao map group l 


-- 6.Função auxiliar para verificar se uma escada está válida
escadaValida :: [[Bloco]] -> Posicao -> Bool
escadaValida matrizEscada (col, row) =
  let blocoInicial = matrizEscada !! round row !! round col
      blocoFinal = matrizEscada !! (round row + 1) !! round col
  in blocoInicial == Plataforma || blocoFinal == Plataforma


-- 8.Cria um Personagem
criarPersonagem :: Velocidade -> Entidade -> Posicao -> Direcao -> (Double, Double) -> Int -> Int -> (Bool, Double) -> Maybe Personagem
criarPersonagem vel ent pos dir tam vida pontos dano =
  let tamanhoX = fst tam
      tamanhoY = snd tam
      -- Verifica se o bloco na posição inicial do personagem é Vazio
      blocoInicial = matrizBlocos !! round (snd pos) !! round (fst pos)
  in if blocoInicial == Vazio
       then Just (Personagem vel ent pos dir tam False False vida pontos dano)
       else Nothing

-- 8.Cria um Colecionavel
criarColecionavel :: Colecionavel -> Posicao -> Maybe (Colecionavel, Posicao)
criarColecionavel col pos =
  let -- Verifica se o bloco na posição inicial do colecionável é Vazio
      blocoInicial = matrizBlocos !! round (snd pos) !! round (fst pos)
  in if blocoInicial == Vazio
       then Just (col, pos)
       else Nothing


{-
posb :: Mapa -> Bloco -> [[Int]] --calcula posicoes de um bloco, usa se achares util
posb (Mapa _ _ l) b = map (elemIndices b) l

fantasma = Personagem {tipo= Fantasma, tamanho= (1,1), ressalta = True, vida= 1, aplicaDano = (False,0)} -- tamanho dos fantasmas = tamanho mario = (1,1)?
dk = Personagem {tipo= MacacoMalvado,tamanho= (2,2), ressalta = True, vida = 8, aplicaDano = (False,0)} --vida 8 pq há 8 alçapões, tamanho (2,2)?
mario = Personagem {tipo = Jogador,tamanho= (1,1), ressalta = False, vida = 3}
-}

{-
chao :: Mapa -> Mapa
chao (Mapa x y m) = Mapa x y (init m ++ [replicate (length (last m)) Plataforma]) --oq isto faz é subsituir a ultima linha da matriz do mapa por uma q tem apenas plataformas
-}
{-exemplo de mapa
Mapa ((0.5, 5.5), Oeste) (0.5, 2.5) [[P, P, P, P, P, P, P, P, P, P],[V, V, V, V, V, V, V, V, V, V],[V, V, V, V, V, V, V, V, V, V],[P, P, V, V, V, V, P, P, P, P],[V, V, V, V, V, V, V, V, E, V],[V, V, V, V, V, V, V, V, E, V],[P, P, P, P, P, P, P, P, P, P]]
-}
