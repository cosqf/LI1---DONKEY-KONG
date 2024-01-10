module Main where
  
import LI12324
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

data Estado = Estado --adicionar o resto
  { 
    modo :: Modo,
    jogo :: Jogo
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


desenhaJogador:: Estado -> IO Picture 
desenhaJogador estado@Estado {modo= EmJogo Jogo1, jogo= Jogo {jogador= 
  Personagem {velocidade= (0,0), tipo= Jogador, posicao = (x,y), direcao=dir, tamanho= tam, emEscada= False, ressalta= False, vida= v, pontos= p, aplicaDano= (False, d)}}} = 
    return $ turnEste dir . tamanhoscale tam $ marioparado
desenhaJogador estado@Estado {modo= EmJogo Jogo1, jogo= Jogo {jogador= 
  Personagem {velocidade= (0,0), tipo= Jogador, posicao = (x,y), direcao=dir, tamanho= tam, emEscada= False, ressalta= False, vida= v, pontos= p, aplicaDano= (True, d)}}}
  |even d = return $ turnEste dir . tamanhoscale tam $ mariomarteloup
  |otherwise = return $ turnEste dir . tamanhoscale tam $ mariomartelodown
desenhaJogador estado@Estado {modo= EmJogo Jogo1, jogo= Jogo {jogador= 
  Personagem {velocidade= vel, tipo= Jogador, posicao = (x,y), direcao=dir, tamanho= tam, emEscada= False, ressalta= False, vida= v, pontos= p, aplicaDano= (False, d)}}} = 
    return $ turnEste dir . tamanhoscale tam $ marioanda1 -- adicionar o resto dos sprites
desenhaJogador estado@Estado {modo= EmJogo Jogo1, jogo= Jogo {jogador= 
  Personagem {velocidade= vel, tipo= Jogador, posicao = (x,y), direcao=dir, tamanho= tam, emEscada= False, ressalta= False, vida= v, pontos= p, aplicaDano= (True, d)}}}
  |even d = return $ turnEste dir . tamanhoscale tam $ mariomarteloandaup
  |otherwise = return $ turnEste dir . tamanhoscale tam $ mariomarteloandadown
desenhaJogador estado@Estado {modo= EmJogo Jogo1, jogo= Jogo {jogador= 
  Personagem {velocidade= vx, tipo= Jogador, posicao = (x,y), direcao=dir, tamanho= tam, emEscada= True, ressalta= False, vida= v, pontos= p, aplicaDano= (False, d)}}} = 
    return $ tamanhoscale tam $ mariosubir -- adicionar o resto dos sprites


turnEste :: Direcao -> Picture -> Picture
turnEste Este p= scale (-1) 1 p
turnEste _ p = p

tamanhoscale :: (Double,Double) -> Picture -> Picture
tamanhoscale (x,y)= scale (realToFrac x) (realToFrac y)



desenhaJogo :: Estado -> IO Picture    -- graficos 
desenhaJogo e = return Blank





-- funções reage

reageEvento :: Event -> Estado -> IO Estado
reageEvento evento estado@Estado {modo= modo} = case modo of
  MenuInicial jogar -> menureage evento estado -- Passar o estado corrente para o reageMenu
  EmJogo op -> jogoreage evento estado
  Mensagem op -> reageMensagem evento op estado
  Pausa op -> pausareage evento estado


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
jogoreage (EventKey (SpecialKey KeyEsc) Down _ _) e@Estado {modo = Pausa _} =
  return e {modo = EmJogo Jogo1}





reageMensagem :: Event -> MensagemOp -> Estado -> IO Estado
reageMensagem (EventKey (SpecialKey KeyEnter) Down _ _) _ e@Estado {modo = modo}=
  return (e {modo= MenuInicial Jogar}) -- Retorne ao menu após pressionar Enter
reageMensagem _ estado e@Estado {modo = modo} = return (e{modo=Mensagem estado}) -- Mantenha o estado atual se outros eventos ocorrerem


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

