{-|
Module      : Tarefa5
Description : Semelhante à LI12324 mas com gloss.
Copyright   : Ivo Filipe Mendes Vieira <a103999@alunos.uminho.pt>
              Filipa Cosquete Santos <a106837@alunos.uminho.pt>

Módulo para a realização da Tarefa 5 de LI1 em 2023/24.
-}

module Tarefa5 where
import LI12324
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Imagens

-- | Estado do jogo
data Estado = Estado 
  {
    modo :: Modo, -- ^ Indica se está a decorrer o jogo, menu, etc.
    jogo :: Jogo, -- ^ Contém toda a informação para correr o jogo
    tempo :: Tempo, -- ^ Quanto tempo decorreu desde o início do jogo
    imagens :: Imagem -- ^ Todas as imagens do jogo
  }

-- | Os vários modos de jogo
data Modo = EmJogo | MenuInicial MenuInicialOp | Pausa PausaOp | Mensagem MensagemOp | Opcoes OpcoesOp

-- | Opções do menu inicial
data MenuInicialOp = Menu | Jogar | Sair | Opcao

-- | Opcções das opções
data OpcoesOp = Creditos1 | Creditos2 | Creditos3 | Creditos4 | Creditos5

-- | Opcções do menu de pausa
data PausaOp = VoltaMenu | RetomaJogo deriving (Show,Eq)

-- | As duas mensagens possíveis.
data MensagemOp = Vitoria | Derrota
  deriving (Show, Eq)




