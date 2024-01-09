module Main where
  
import LI12324
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

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
data Modo = EmJogo Jogo1 | MenuInicial MenuInicialOp | Pausa PausaOp | Mensagem MensagemOp
data Jogo1 = Jogo1 deriving (Show, Eq)

data MenuInicialOp = Jogar | Sair 

data PausaOp = VoltaMenu | RetomaJogo deriving (Show,Eq)

data MensagemOp = Vitoria | Derrota
  deriving (Show, Eq)

{-
data CategoriaImagens = IBlocos | IPersonagem | IInimigos

type ImagensBlocos = [(Tema, [(Bloco, Picture)])]
type ImagensPersonagens = [(Tema, [(Entidade, Picture)])]


type Imagens = Imagens {
        blocos :: ImagensBlocos,
        personagens :: ImagensPersonagens
                       }
-}

-- funções desenha

desenha :: Estado -> IO Picture
desenha estado@Estado {modo= modo} = case modo of
  MenuInicial op -> desenhaMenu estado
  Pausa op -> desenhaMenu estado
  EmJogo op -> desenhaJogo estado
  Mensagem op -> desenhaMensagem op

desenhaMenu :: Estado -> IO Picture
desenhaMenu e@Estado {modo = MenuInicial Jogar} =
  return $ Pictures [Color blue opcaoJogar, Color white opcaoSair]
desenhaMenu e@Estado {modo = MenuInicial Sair} =
  return $ Pictures [Color white opcaoJogar, Color blue opcaoSair]
desenhaMenu e@Estado {modo = Pausa RetomaJogo} =
  return $ Pictures [Color blue opcaoRetomaJogo, Color white opcaoSair]
desenhaMenu e@Estado {modo = Pausa VoltaMenu} =
  return $ Pictures [Color white opcaoRetomaJogo, Color blue opcaoSair]

opcaoJogar = Translate (-150) (100) $ Text "Jogar"
opcaoSair = Translate (-150) (-100) $ Text "Sair"
opcaoMenu = Translate (-150) (100) $ Text "Menu"
opcaoRetomaJogo = Translate (-150) (100) $ Text "Jogar"


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


desenhaJogo :: Estado -> IO Picture    -- graficos 
desenhaJogo jogo =
  return Blank







-- funções reage

reageEvento :: Event -> Estado -> IO Estado
reageEvento evento estado@Estado {modo= modo} = case modo of
  MenuInicial jogar -> menureage evento (Estado modo) -- Passar o estado corrente para o reageMenu
  EmJogo op -> jogoreage evento estado
  Mensagem op -> reageMensagem evento op
  Pausa op -> pausareage evento $ Estado modo


pausareage :: Event -> Estado -> IO Estado
pausareage (EventKey (SpecialKey KeyDown) Down _ _) e@Estado {modo = Pausa RetomaJogo} =
  return e {modo = Pausa VoltaMenu}
pausareage (EventKey (SpecialKey KeyUp) Down _ _) e@Estado {modo = Pausa VoltaMenu} =
  return e {modo = Pausa RetomaJogo}
pausareage (EventKey (SpecialKey KeyEnter) Down _ _) e@Estado {modo = Pausa VoltaMenu} =
  return e {modo = MenuInicial Jogar}
pausareage (EventKey (SpecialKey KeyEnter) Down _ _) e@Estado {modo = Pausa RetomaJogo} =
  return e {modo = EmJogo Jogo1}


menureage :: Event -> Estado -> IO Estado
menureage (EventKey (SpecialKey KeyDown) Down _ _) e@Estado {modo = MenuInicial Jogar} =
  return e {modo = MenuInicial Sair}
menureage (EventKey (SpecialKey KeyUp) Down _ _) e@Estado {modo = MenuInicial Sair} =
  return e {modo = MenuInicial Jogar}
menureage (EventKey (SpecialKey KeyEnter) Down _ _) e@Estado {modo = MenuInicial Jogar} =
  return e {modo = EmJogo Jogo1}
menureage (EventKey (SpecialKey KeyEnter) Down _ _) e@Estado {modo = MenuInicial Sair} = error "Sair do jogo"
menureage _ e = return e


jogoreage :: Event -> Estado -> IO Estado         -- !!!
jogoreage (EventKey (SpecialKey KeyEsc) Down _ _) e@Estado {modo = EmJogo Jogo1} =
  return e {modo = Pausa RetomaJogo}




reageMensagem :: Event -> MensagemOp -> IO Estado
reageMensagem (EventKey (SpecialKey KeyEnter) Down _ _) _ =
  return (Estado (MenuInicial Jogar)) -- Retorne ao menu após pressionar Enter
reageMensagem _ estado = return (Estado (Mensagem estado)) -- Mantenha o estado atual se outros eventos ocorrerem


janela :: Display
janela = InWindow
       "DK" 
       (1024, 768)
       (0,0) 

corFundo = black 

fr:: Int
fr = 60

tempo :: Float -> Estado -> IO Estado
tempo _ estado = return estado

main :: IO ()
main = do
  playIO janela corFundo fr (Estado {modo = MenuInicial Jogar}) desenhaMenu menureage tempo




{-
marioMov (EventKey (SpecialKey KeyUp) Down _ _) e@Estado {modo = EmJogo Jogo1} (mario@(Personagem {emEscada= b})
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
-}