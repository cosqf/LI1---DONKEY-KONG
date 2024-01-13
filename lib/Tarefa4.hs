{-|
Module      : Tarefa4
Description : Atualiza as velocidades das personagens no jogo
Copyright   : Ivo Filipe Mendes Vieira <a103999@alunos.uminho.pt>
              Filipa Cosquete Santos <a106837@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 de LI1 em 2023/24.
-}
module Tarefa4 where

import LI12324
import Funcoes



atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza listamov jogadormov Jogo {mapa= m, inimigos= i, colecionaveis= c, jogador= j} = 
    Jogo
    { mapa = m,
    inimigos = movimentos listamov i,
    colecionaveis = c,
    jogador = movimentosM jogadormov j
    }

-- | combina todas as funções de movimento relacionadas com os fantasmas
allFantMov :: Mapa -> Personagem -> Maybe Acao 
allFantMov m p=
  case (ressaltacheck m p, escFantFix m p, fantEscada m p, fantMov p) of -- | prioridade : escFantFixA > fantEscadas > fantMov
    (Just a,_,_,_)                       -> Just a
    (Nothing, Just a, _, _)              -> Just a
    (Nothing, Nothing, Just a, _)        -> Just a 
    (Nothing, Nothing, Nothing , Just a) -> Just a 
    _                                    -> Nothing 


-- |gera uma lista de ações aleatórias para os fantasmas
fantMov :: Personagem -> Maybe Acao -- movimento aleatorio, fzr mais inteligente dps
fantMov f = func (f,n)
    where
        [n] =geraAleatorios 10 1
        func :: (Personagem, Int) -> Maybe Acao
        func (p,x)
            |even x = Just AndarDireita 
            |otherwise = Just AndarEsquerda 

-- | decide se os fantasmas sobem as escadas ou não
fantEscada :: Mapa -> Personagem -> Maybe Acao
fantEscada mapa f= func (f,n)           -- fazer mais inteligente dps
    where
        [n]= geraAleatorios 100 1 
        func :: (Personagem, Int) -> Maybe Acao
        func (p@Personagem {posicao= (x,y)},n)
          |even n && blocopos (x,y+2) mapa == Escada = Just Descer
          |even n && blocopos (posicao p) mapa == Escada = Just Subir
          |otherwise = Nothing

-- | Para o fantasma depois de subirem uma escada
escFantFix :: Mapa -> Personagem -> Maybe Acao
escFantFix mapa (Personagem {posicao= (x,y), emEscada= esc , velocidade= (vx,vy)})
    |esc && blocopos (x,y) mapa == Vazio && vy<0 = Just Parar 
    |otherwise = Nothing

-- | Altera a direção dos personagens com "Ressalta" definida como True quando colidem com algo
ressaltacheck ::  Mapa -> Personagem -> Maybe Acao
ressaltacheck mapa p@Personagem {ressalta= True, posicao= (x,y), velocidade= (vx,vy), direcao = d} =
    case d of 
        Este -> if blocopos (x+1,y+1) mapa == Vazio ||blocopos (x+1,y) mapa == Plataforma then Just AndarEsquerda else Nothing
        Oeste ->  if blocopos (x-1,y+1) mapa == Vazio ||blocopos (x-1,y) mapa == Plataforma then Just AndarDireita else Nothing
        _ -> Nothing


-- adicionar função para decidir oq os fantasmas fazem dps de descer/subir a escada?


{-|implementa o movimento dos personagens-}
movimentosM :: Maybe Acao -> Personagem -> Personagem 
movimentosM (Just AndarDireita) p@(Personagem {velocidade= (vx,vy)}) = 
    p {velocidade = (10,vy), direcao= Este, emEscada= False}
movimentosM (Just AndarEsquerda) p@(Personagem {velocidade= (vx,vy)}) = 
    p {velocidade = (-10,vy), direcao= Oeste, emEscada= False}
movimentosM (Just Subir) (p) = p {velocidade = (0,-10), direcao= Norte,emEscada=True}
movimentosM (Just Descer) (p) = p {velocidade = (0,10), direcao= Sul, emEscada= True}
movimentosM (Just Parar) (p) = p {velocidade = (0,0)} 
movimentosM (Just Saltar) p@(Personagem {velocidade= (vx,vy), emEscada = False,direcao= d })
    |d== Este = p {velocidade = (50, -50)} 
    |otherwise = p {velocidade = (-50, -50)} 
movimentosM Nothing p = p


movimentos :: [Maybe Acao] -> [Personagem] -> [Personagem]
movimentos ms ps = map (\(m, p) -> movimentosM m p) $ zip ms ps



{-
1. Ao pressionar uma tecla deverá convertê-la (se possı́vel) numa Acao,
e.g. KeySpace fará o jogador saltar, e invocar a função atualiza com
o argumento Just Saltar para a acção do jogador.

2. A cada novo frame deverá atribuir uma acção a todos os inimigos.
A acção atribuı́da poderá ser tão sofisticada quanto quiser. Poderá
ser (inclusivamente) Nothing, o que faria com que os inimigos não
reagissem à passagem do tempo, ou seja, manteriam os seus percursos.-}
