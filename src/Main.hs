module Main where
import LI12324
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Directory.Internal.Prelude (exitFailure)

data Estado = Estado --adicionar o resto
  { modo :: Modo
  }

{-
data Estado = Estado --adicionar o resto
  { jogo :: Jogo,
    imagens :: Imagens,
    modo :: Modo
  }
-}
data Modo = EmJogo | MenuInicial MenuInicialOp | Pausa PausaOp
  deriving (Show, Eq)

data MenuInicialOp = Jogar | Sair 

data PausaOp = VoltaMenu | RetomaJogo deriving (Show,Eq)


{-
data CategoriaImagens = IBlocos | IPersonagem | IInimigos

type ImagensBlocos = [(Tema, [(Bloco, Picture)])]
type ImagensPersonagens = [(Tema, [(Entidade, Picture)])]


type Imagens = Imagens {
        blocos :: ImagensBlocos,
        personagens :: ImagensPersonagens
                       }
-}
-- menu texto

opcaoJogar = Translate (-150) (100) $ Text "Jogar"

opcaoSair = Translate (-150) (-100) $ Text "Sair"

opcaoMenu = Translate (-150) (100) $ Text "Menu"

opcaoRetomaJogo = Translate (-150) (100) $ Text "Jogar"

desenha :: Estado -> IO Picture
desenha e@Estado {modo = MenuInicial Jogar} =
  return $ Pictures [Color blue opcaoJogar, opcaoSair]
desenha e@Estado {modo = MenuInicial Sair} =
  return $ Pictures [opcaoJogar, Color blue opcaoSair]
desenha e@Estado {modo = Pausa RetomaJogo} =
  return $ Pictures [Color blue opcaoRetomaJogo, opcaoSair]
desenha e@Estado {modo = Pausa VoltaMenu} =
  return $ Pictures [opcaoRetomaJogo, Color blue opcaoSair]

-- menu
menureage :: Event -> Estado -> IO Estado
menureage (EventKey (SpecialKey KeyEsc) Down _ _) e@Estado {modo = EmJogo} =
  return e {modo = Pausa Retomajogo}
menureage (EventKey (SpecialKey KeyDown) Down _ _) e@Estado {modo = Pausa RetomaJogo} =
  return e {modo = Pausa VoltaMenu}
menureage (EventKey (SpecialKey KeyUp) Down _ _) e@Estado {modo = Pausa RVoltaMenu} =
  return e {modo = Pausa RetomaJogo}
menureage (EventKey (SpecialKey KeyEnter) Down _ _) e@Estado {modo = Pausa VoltaMenu} =
  return e {modo = MenuInicial}
menureage (EventKey (SpecialKey KeyEnter) Down _ _) e@Estado {modo = Pausa RetomaJogo} =
  return e {modo = EmJogo}

menureage (EventKey (SpecialKey KeyDown) Down _ _) e@Estado {modo = MenuInicial Jogar} =
  return e {modo = MenuInicial Sair}
menureage (EventKey (SpecialKey KeyUp) Down _ _) e@Estado {modo = MenuInicial Sair} =
  return e {modo = MenuInicial Jogar}
menureage (EventKey (SpecialKey KeyEnter) Down _ _) e@Estado {modo = MenuInicial Jogar} =
  return e {modo = EmJogo}
menureage (EventKey (SpecialKey KeyEnter) Down _ _) e@Estado {modo = MenuInicial Sair} = exitFailure
menureage _ e = return e


janela :: Display
janela = InWindow
       "DK" 
       (400, 400)   
       (0,0) 

corFundo = Black 

fr:: Int
fr = 60

tempo :: IO Estado

main :: IO ()
main = do
  playIO 
  janela corFundo fr (Estado {modo = MenuInicial Jogar}) desenha menureage --tempo

{-
main :: IO ()
main = do
  playIO 
  janela corFundo fr (Estado {jogo = jogoInicial, imagens = imgs, modo = MenuInicial Jogar}) desenha menureage --tempo
-}
