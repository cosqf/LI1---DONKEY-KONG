module Tarefa5 where
import LI12324
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy


marioMovTeclas :: Event -> Jogo -> Maybe Acao
marioMovTeclas (EventKey (SpecialKey KeyUp) Down _ _) jogo =
  case jogador jogo of
    Personagem {emEscada = True}  -> Just Subir
    _                             -> Nothing
marioMovTeclas (EventKey (SpecialKey KeyDown) Down _ _) jogo =
  case jogador jogo of
    Personagem {emEscada = True}  -> Just Descer
    _                             -> Nothing
marioMovTeclas (EventKey (SpecialKey KeyLeft) Down _ _) jogo =
  case jogador jogo of
    Personagem {emEscada = False} -> Just AndarEsquerda
    _                             -> Nothing
marioMovTeclas (EventKey (SpecialKey KeyRight) Down _ _) jogo =
  case jogador jogo of
    Personagem {emEscada = False} -> Just AndarDireita
    _                             -> Nothing
marioMovTeclas (EventKey (SpecialKey KeySpace) Down _ _) jogo =
  case jogador jogo of
    Personagem {emEscada = False, aplicaDano = (False, _)} -> Just Saltar
    _                                                      -> Nothing
marioMovTeclas _ _ = Just Parar


