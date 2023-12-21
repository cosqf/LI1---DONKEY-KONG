{-|
Module      : Tarefa4
Description : Atualiza as velocidades das personagens no jogo
Copyright   : Ivo Filipe Mendes Vieira <a103999@alunos.uminho.pt>
              Filipa Cosquete Santos <a106837@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 de LI1 em 2023/24.
-}
module Tarefa4 where

import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import LI12324
import Tarefa3

atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza = undefined


{-
1. Ao pressionar uma tecla deverá convertê-la (se possı́vel) numa Acao,
e.g. KeySpace fará o jogador saltar, e invocar a função atualiza com
o argumento Just Saltar para a acção do jogador.

2. A cada novo frame deverá atribuir uma acção a todos os inimigos.
A acção atribuı́da poderá ser tão sofisticada quanto quiser. Poderá
ser (inclusivamente) Nothing, o que faria com que os inimigos não
reagissem à passagem do tempo, ou seja, manteriam os seus percursos.-}



marioMov :: Event -> Personagem -> Maybe Acao
marioMov (EventKey (SpecialKey KeyUp) Down _ _) mario@(Personagem {emEscada= b})
    |b = Just Subir
    |otherwise = Nothing
marioMov (EventKey (SpecialKey KeyDown) Down _ _) mario@(Personagem {emEscada= b})
    |b = Just Descer
    |otherwise = Nothing
marioMov (EventKey (SpecialKey KeyLeft) Down _ _) mario@(Personagem {emEscada= False}) = Just AndarEsquerda
marioMov (EventKey (SpecialKey KeyRight) Down _ _) mario@(Personagem {emEscada= False})= Just AndarDireita
marioMov (EventKey (SpecialKey KeySpace) Up _ _) mario@(Personagem {aplicaDano = (b,_)})
    |b= Nothing
    |otherwise= Just Saltar
marioMov _ e = Just Parar

fantMov :: [Personagem] -> [Maybe Acao] --movimento aleatorio, fzr mais inteligente dps
fantMov [] = []
fantMov f = func (zip f (geraAleatorios 10 4))
    where
        func :: [(Personagem, Int)] -> [Maybe Acao]
        func [] = []
        func ((p,x):fs)
            |even x = Just AndarDireita : func fs
            |otherwise = Just AndarEsquerda : func fs

fantEscada :: [Personagem] -> [Int] -> Mapa -> [Personagem]   -- decidir se os fantasmas sobem a escada ou n
fantEscada [] _ _ = []
fantEscada f a mapa= func (zip f x)                     -- fazer mais inteligente dps
    where
        x= geraAleatorios 10 (length f)
        func :: [(Personagem, Int)] -> [Personagem]
        func [] = []
        func ((p,x):fs)
            |Escada == blocodirecao p Sul mapa && even x = p {velocidade = (0,10), emEscada = True} : func fs
            |Escada == blocopos (posicao (head f)) mapa && even x = p {velocidade = (0,-10), emEscada = True} : func fs
            |Escada == blocodirecao p Sul mapa = p {velocidade = (0,0), emEscada = False} :func fs
            |Escada == blocopos (posicao (head f)) mapa = p {velocidade = (0,0), emEscada = False} :func fs

movfantescadas :: [Personagem] -> [Maybe Acao]
movfantescadas (p:ps)
    |velocidade p == (0,10) && emEscada p = Just Descer : movfantescadas ps
    |velocidade p == (0,-10) && emEscada p = Just Subir : movfantescadas ps
    |velocidade p == (0,0) && not (emEscada p) = Nothing : movfantescadas ps


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


velocidades :: [Personagem] -> [Personagem] -- relacionar a velocidade com a posicao
velocidades [] = [] 
velocidades (p@Personagem{velocidade=(vx,vy), posicao=(x,y)}:ps) = 
    p { posicao = (x + (vx / 10), y + (vy / 10)) } : velocidades ps



--fantMov :: [Personagem] -> [Maybe Acao]
--fantMov f = map (\(_, x) -> if even x then Just AndarDireita else Just AndarEsquerda) (zip f (geraAleatorios 10 4))
