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


{-|Função principal da tarefa 4, recebe as ações que os personagens receberão e implementa-os.

@
atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza listamov jogadormov Jogo {mapa= m, inimigos= i, colecionaveis= c, jogador= j} = 
    Jogo
    { mapa = m,
    inimigos = movimentos listamov i,
    colecionaveis = c,
    jogador = movimentosM jogadormov j
    }
@
-}
atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza listamov jogadormov Jogo {mapa= m, inimigos= i, colecionaveis= c, jogador= j} = 
    Jogo
    { mapa = m,
    inimigos = movimentos listamov i,
    colecionaveis = c,
    jogador = movimentosM jogadormov j
    }

-- | Combina todas as funções de movimento relacionadas com os fantasmas
allFantMov :: [Int] -> Mapa -> Personagem -> Maybe Acao 
allFantMov x m p=
        case (escFantFix m p, ressaltacheck m p, fantEscada m p (head x), fantMov p (last x)) of -- | prioridade : escFantFixA > fantEscadas > fantMov
            (Just a,_,_,_)                       -> Just a
            (Nothing, Just a, _, _)              -> Just a
            (Nothing, Nothing, Just a, _)        -> Just a 
            (Nothing, Nothing, Nothing , Just a) -> Just a 
            _                                    -> Nothing 


-- |Gera uma lista de ações aleatórias para os fantasmas
fantMov :: Personagem -> Int -> Maybe Acao 
fantMov f n = func (f,n)
    where
        func :: (Personagem, Int) -> Maybe Acao
        func (p,x)
            |even x = Just AndarDireita 
            |otherwise = Just AndarEsquerda 

-- | Decide se os fantasmas sobem/descem as escadas ou não
fantEscada :: Mapa -> Personagem -> Int -> Maybe Acao
fantEscada mapa f n= func (f,n)          
    where
        func :: (Personagem, Int) -> Maybe Acao
        func (p@Personagem {posicao= (x,y), tamanho = (tx,ty)},n)
          |even n && blocopos (x,y+2+ty/2) mapa == Escada = Just Descer
          |even n && blocopos (posicao p) mapa == Escada 
          || (blocopos (posicao p) mapa == Plataforma && blocopos (x,y+1+ty/2) mapa == Escada)
          ||(emEscada p && blocopos (posicao p) mapa == Vazio && blocopos (x,y+ty/2) mapa == Plataforma) = Just Subir
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


-- |Implementa o movimento dos personagens
movimentosM :: Maybe Acao -> Personagem -> Personagem 
movimentosM (Just AndarDireita) p@(Personagem {velocidade= (vx,vy)}) = 
    p {velocidade = (10,vy), direcao= Este, emEscada= False}
movimentosM (Just AndarEsquerda) p@(Personagem {velocidade= (vx,vy)}) = 
    p {velocidade = (-10,vy), direcao= Oeste, emEscada= False}
movimentosM (Just Subir) (p) = p {velocidade = (0,-10), direcao= Norte,emEscada=True}
movimentosM (Just Descer) (p) = p {velocidade = (0,10), direcao= Sul, emEscada= True}
movimentosM (Just Parar) (p) = p {velocidade = (0,0), emEscada= False} 
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
      [Vazio, Alcapao, Plataforma, Plataforma, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma, Vazio],
      [Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],
      [Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],
      [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ]

inimigoParado =
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
      posicao = (8.5, 7),
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
      inimigos = [inimigoParado],
      colecionaveis = [],
      jogador = jogadorParado
    }
