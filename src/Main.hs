module Main where
import LI12324
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Directory.Internal.Prelude (exitFailure)


-- Defina os tipos de dados e exporte-os
data Estado = Estado { modo :: Modo }
  deriving (Show, Eq)

data Modo = MenuInicial | EmJogo Jogo1 | Mensagem MensagemOp
  deriving (Show, Eq)

data MensagemOp = Vitoria | Derrota
  deriving (Show, Eq)

data Jogo1 = Jogo1
  -- Adicione campos necessários para o estado do jogo
  deriving (Show, Eq)


-- Constantes para a janela e a taxa de atualização
janela :: Display
janela = InWindow "Jogo" (800, 600) (0, 0)

fr :: Int
fr = 60

-- Função principal
main :: IO ()
main = playIO janela corFundo fr estadoInicial desenha reageEvento atualiza

-- Estado inicial
estadoInicial :: Estado
estadoInicial = Estado MenuInicial

-- Cor de fundo
corFundo :: Color
corFundo = black

-- Função para desenhar o estado do jogo
desenha :: Estado -> IO Picture
desenha (Estado modo) = case modo of
  MenuInicial -> desenhaMenu
  EmJogo jogo -> desenhaJogo jogo
  Mensagem op -> desenhaMensagem op

-- Função para reagir aos eventos
reageEvento :: Event -> Estado -> IO Estado
reageEvento evento (Estado modo) = case modo of
  MenuInicial -> reageMenu evento (Estado modo) -- Pass the current estado to reageMenu
  EmJogo jogo -> reageJogo evento jogo
  Mensagem op -> reageMensagem evento op

-- Função para atualizar o estado do jogo
atualiza :: Float -> Estado -> IO Estado
atualiza _ estado = return estado

-- Funções específicas para cada estado

-- Menu Inicial
desenhaMenu :: IO Picture
desenhaMenu =
  return $
    Pictures
      [ Translate (-150) 100 $ Color blue $ Text "Jogar",
        Translate (-150) (-100)$ Color blue $ Text "Sair"
      ]

reageMenu :: Event -> Estado -> IO Estado
reageMenu (EventKey (SpecialKey KeyDown) Down _ _) estado =
  return (Estado MenuInicial)
reageMenu (EventKey (SpecialKey KeyUp) Down _ _) estado =
  return (Estado MenuInicial)
reageMenu (EventKey (SpecialKey KeyEnter) Down _ _) estado =
  return (Estado (EmJogo novoJogo))
  where
    novoJogo = Jogo1 -- Use o alias de tipo Jogo1
reageMenu _ estado = return estado


-- Jogo
desenhaJogo :: Jogo1 -> IO Picture
desenhaJogo jogo =
  -- Adicione lógica para desenhar o jogo
  return Blank

reageJogo :: Event -> Jogo1 -> IO Estado
reageJogo evento jogo =
  -- Adicione lógica para reagir a eventos durante o jogo
  return (Estado (EmJogo jogo))

-- Mensagem
desenhaMensagem :: MensagemOp -> IO Picture
desenhaMensagem op =
  return $
    Pictures
      [ Translate (-150) 100 $ Color blue $ mensagem,
        Translate (-150) (-100) $ Text "Pressione Enter para retornar ao menu"
      ]
  where
    mensagem = case op of
      Vitoria -> Text "Parabéns! Você venceu!"
      Derrota -> Text "Você perdeu. Tente novamente."

reageMensagem :: Event -> MensagemOp -> IO Estado
reageMensagem (EventKey (SpecialKey KeyEnter) Down _ _) _ =
  return (Estado MenuInicial) -- Retorne ao menu após pressionar Enter
reageMensagem _ estado = return (Estado (Mensagem estado)) -- Mantenha o estado atual se outros eventos ocorrerem

