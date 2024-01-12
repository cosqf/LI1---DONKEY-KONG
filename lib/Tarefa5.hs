module Tarefa5 where
import LI12324
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

type Imagem = [(String, Picture)]

data Estado = Estado 
  {
    modo :: Modo,
    jogo :: Jogo,
    tempo :: Tempo,
    imagens :: Imagem
  }

data Modo = EmJogo | MenuInicial MenuInicialOp | Pausa PausaOp | Mensagem MensagemOp | OpcoesOp

data MenuInicialOp = Menu | Jogar | Sair | Opcoes

data OpcoesOp = Skins 

data PausaOp = VoltaMenu | RetomaJogo deriving (Show,Eq)

data MensagemOp = Vitoria | Derrota
  deriving (Show, Eq)




