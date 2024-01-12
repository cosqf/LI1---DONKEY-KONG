{-|
Module      : Tarefa4
Description : Atualiza as velocidades das personagens no jogo
Copyright   : Ivo Filipe Mendes Vieira <a103999@alunos.uminho.pt>
              Filipa Cosquete Santos <a106837@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 de LI1 em 2023/24.
-}
module Tarefa4 where

import Data.Maybe
import LI12324
import Funcoes



atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza listamov jogadormov Jogo {mapa= m, inimigos= i, colecionaveis= c, jogador= j} = 
    Jogo
    { mapa = m,
    inimigos = movimentos (allFantMov i m) i,
    colecionaveis = c,
    jogador = movimentosM jogadormov j
    }

-- | combina todas as funções de movimento relacionadas com os fantasmas
allFantMov :: [Personagem] -> Mapa -> [Maybe Acao]   
allFantMov [] _ = []
allFantMov p  m=
  case (head (fantescadas p m), head (fantMov p)) of -- | fantescada toma prioridade
    (Just a, _)        -> Just a : allFantMov (tail p) m
    (Nothing , Just a) -> Just a : allFantMov (tail p) m
    _                  -> Nothing : allFantMov (tail p) m

-- |gera uma lista de ações aleatórias para os fantasmas
fantMov :: [Personagem] -> [Maybe Acao] -- movimento aleatorio, fzr mais inteligente dps
fantMov [] = []
fantMov f = func (zip f (geraAleatorios 10 (length f)))
    where
        func :: [(Personagem, Int)] -> [Maybe Acao]
        func [] = []
        func ((p,x):fs)
            |even x = Just AndarDireita : func fs
            |otherwise = Just AndarEsquerda : func fs

-- | decide se os fantasmas sobem as escadas ou não
fantEscada :: [Personagem] -> Mapa -> [Personagem]
fantEscada [] _ = []
fantEscada f mapa= func (zip f x)           -- fazer mais inteligente dps
    where
        x= geraAleatorios 10 (length f)
        func :: [(Personagem, Int)] -> [Personagem]
        func [] = []
        func ((p,x):fs)
            |emEscada p && ( velocidade p == (0,10) || velocidade p == (0,-10)) = p :func fs
            |Escada == blocodirecao p Sul mapa && even x = p {velocidade = (0,10), emEscada = True} : func fs
            |Escada == blocopos (posicao p) mapa && even x = p {velocidade = (0,-10), emEscada = True} : func fs
            |Escada == blocodirecao p Sul mapa = p {velocidade = (0,0), emEscada = False} :func fs
            |Escada == blocopos (posicao p) mapa = p {velocidade = (0,0), emEscada = False} :func fs
            |otherwise= p: func fs

-- |atribui movimentos aos fantasmas
movfantescadas :: [Personagem] -> [Maybe Acao] 
movfantescadas (p:ps)
    |velocidade p == (0,10) && emEscada p = Just Descer : movfantescadas ps
    |velocidade p == (0,-10) && emEscada p = Just Subir : movfantescadas ps
    |velocidade p == (0,0) && not (emEscada p) = Nothing : movfantescadas ps


fantescadas :: [Personagem] -> Mapa -> [Maybe Acao]
fantescadas p mapa = movfantescadas (fantEscada p mapa)

-- adicionar função para decidir oq os fantasmas fazem dps de descer/subir a escada?


{-|implementa o movimento dos personagens, ver se ponho mais alguma coisa-}
movimentosM :: Maybe Acao -> Personagem -> Personagem 
movimentosM (Just AndarDireita) p@(Personagem {velocidade= (vx,vy)}) = 
    p {velocidade = (10,vy), direcao= Este, emEscada= False}
movimentosM (Just AndarEsquerda) p@(Personagem {velocidade= (vx,vy)}) = 
    p {velocidade = (-10,vy), direcao= Oeste, emEscada= False}
movimentosM (Just Subir) (p) = p {velocidade = (0,-10), direcao= Norte,emEscada=True}
movimentosM (Just Descer) (p) = p {velocidade = (0,10), direcao= Sul, emEscada= True}
movimentosM (Just Parar) (p) = p {velocidade = (0,0)} 
movimentosM (Just Saltar) p@(Personagem {velocidade= (vx,vy)}) = 
    p {velocidade = (vx, -10)} 
movimentosM Nothing p = p

movimentos :: [Maybe Acao] -> [Personagem] -> [Personagem]
movimentos ms ps = map (\(m, p) -> movimentosM m p) $ zip ms ps



{-
movimentos :: [Maybe Acao] -> [Personagem] -> [Personagem] -- definição dos movimentos andar, subir etc
movimentos [] [] = []
movimentos (Just AndarDireita: ms) (p@(Personagem {velocidade= (vx,vy)}):ps) = 
    p {velocidade = (10,vy), direcao= Este}: movimentos ms ps
movimentos (Just AndarEsquerda: ms) (p@(Personagem {velocidade= (vx,vy)}):ps) = 
    p {velocidade = (-10,vy), direcao= Oeste}: movimentos ms ps
movimentos (Just Subir: ms) (p:ps) = p {velocidade = (0,-10), direcao= Norte} :movimentos ms ps
movimentos (Just Descer: ms) (p:ps) = p {velocidade = (0,10), direcao= Sul} :movimentos ms ps
movimentos (Just Parar: ms) (p:ps) = p {velocidade = (0,0)} : movimentos ms ps
movimentos (Just Saltar: ms) (p@(Personagem {velocidade= (vx,vy)}):ps) = 
    p {velocidade = (vx, -10)} : movimentos ms ps
movimentos (Nothing:ms) (p:ps) = p : movimentos ms ps 
-}



--fantMov :: [Personagem] -> [Maybe Acao]
--fantMov f = map (\(_, x) -> if even x then Just AndarDireita else Just AndarEsquerda) (zip f (geraAleatorios 10 4))


{-
1. Ao pressionar uma tecla deverá convertê-la (se possı́vel) numa Acao,
e.g. KeySpace fará o jogador saltar, e invocar a função atualiza com
o argumento Just Saltar para a acção do jogador.

2. A cada novo frame deverá atribuir uma acção a todos os inimigos.
A acção atribuı́da poderá ser tão sofisticada quanto quiser. Poderá
ser (inclusivamente) Nothing, o que faria com que os inimigos não
reagissem à passagem do tempo, ou seja, manteriam os seus percursos.-}
