module Main where

import LI12324
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Imagens

data Estado = Estado --adicionar o resto
  {
    modo :: Modo,
    jogo :: Jogo,
    tempo :: Tempo
  }



data Modo = EmJogo Jogo1 | MenuInicial MenuInicialOp | Pausa PausaOp | Mensagem MensagemOp

data Jogo1 = Jogo1 deriving (Show, Eq)

data MenuInicialOp = Jogar | Sair

data PausaOp = VoltaMenu | RetomaJogo deriving (Show,Eq)

data MensagemOp = Vitoria | Derrota
  deriving (Show, Eq)



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
desenhaMenu _ = return $ Pictures []

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
desenhaJogador Estado {modo= EmJogo Jogo1, tempo= t, jogo= Jogo {jogador=
  Personagem {velocidade= (0,0), tipo= Jogador, posicao = pos, direcao=dir, tamanho= tam, emEscada= False, ressalta= False, vida= v, pontos= p, aplicaDano= (False, d)}}} =
    transPos pos . turnEste dir . tamanhoscale tam $ marioparado
desenhaJogador Estado {modo= EmJogo Jogo1, tempo= t, jogo= Jogo {jogador=
  Personagem {velocidade= (0,0), tipo= Jogador, posicao = pos, direcao=dir, tamanho= tam, emEscada= False, ressalta= False, vida= v, pontos= p, aplicaDano= (True, d)}}}=
    transPos pos . turnEste dir . tamanhoscale tam $ if (mod (round (t*1000)) 200) < 100 then
                                                                                           mariomarteloup
                                                                                           else mariomartelodown
desenhaJogador Estado {modo= EmJogo Jogo1, tempo= t, jogo= Jogo {jogador=
  Personagem {velocidade= vel, tipo= Jogador, posicao = pos, direcao=dir, tamanho= tam, emEscada= False, ressalta= False, vida= v, pontos= p, aplicaDano= (False, d)}}} =
    transPos pos . turnEste dir . tamanhoscale tam $ if (mod (round (t*1000)) 200) < 100 then
                                                                                            marioanda1
                                                                                            else marioanda2
desenhaJogador Estado {modo= EmJogo Jogo1, tempo= t, jogo= Jogo {jogador=
  Personagem {velocidade= vel, tipo= Jogador, posicao = pos, direcao=dir, tamanho= tam, emEscada= False, ressalta= False, vida= v, pontos= p, aplicaDano= (True, d)}}} =
    transPos pos . turnEste dir . tamanhoscale tam $ if (mod (round (t*1000)) 200) < 100 then
                                                                                            mariomarteloupanda
                                                                                            else mariomarteloandadown
desenhaJogador Estado {modo= EmJogo Jogo1, tempo= t, jogo= Jogo {jogador=
  Personagem {velocidade= vx, tipo= Jogador, posicao = pos, direcao=dir, tamanho= tam, emEscada= True, ressalta= False, vida= v, pontos= p, aplicaDano= (False, d)}}} =
    transPos pos . tamanhoscale tam $ if (mod (round (t*1000)) 200) < 100 then
                                                                              mariosubir
                                                                              else turnEste Este marioanda2
-- falta morte

desenhaColec :: Estado -> IO [Picture] --mapM converte a função de [IO Picture] para IO [Picture]
desenhaColec Estado {modo = EmJogo Jogo1, jogo = Jogo {colecionaveis = l}} = mapM (\(c, pos) -> case c of 
                                                                                Martelo -> transPos pos martelo
                                                                                Moeda   -> transPos pos coin) l


desenhaFantasmas :: Estado -> IO [Picture]
desenhaFantasmas Estado {modo = EmJogo Jogo1, jogo = Jogo {inimigos = l}} =
  mapM (\Personagem
          { velocidade = vel
          , tipo = Fantasma
          , posicao = pos
          , direcao = dir
          , tamanho = tam
          , emEscada = esc
          , ressalta = True
          , vida = v
          , pontos = p
          , aplicaDano = dano
          } -> transPos pos . turnEste dir . tamanhoscale tam $ fantasma) l



desenhaJogo :: Estado -> IO Picture
desenhaJogo e = do
  fant <- desenhaFantasmas e
  colec <- desenhaColec e
  let fant2 = pictures fant
      colec2 = pictures colec
  desenhaJogador e <> return (fant2 <> colec2) -- <> junta as imagens




-- funcoes uteis 
turnEste :: Direcao -> IO Picture -> IO Picture
turnEste Este p= do
  scale (-1) 1 <$> p
turnEste _ p = p

tamanhoscale :: (Double,Double) -> IO Picture -> IO Picture
tamanhoscale (x,y) p= do
  scale (realToFrac x) (realToFrac y) <$> p

transPos :: Posicao -> IO Picture -> IO Picture
transPos (x,y) p = do
  translate (realToFrac x) (realToFrac y) <$> p


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

tempof :: Float -> Estado -> IO Estado
tempof _ e@(Estado {modo= Pausa _}) = return e
tempof _ e@(Estado {modo= MenuInicial _}) = return e {tempo= 0}
tempof t e = return e {tempo= (realToFrac t) + tempo e}



estadoInicial = Estado
  { modo = MenuInicial Jogar
  , jogo = j1
  , tempo = 0
  }

main :: IO ()
main = do
  playIO janela corFundo fr estadoInicial desenhaMenu menureage tempof

{-

:: Display	 Display mode.

-> Color	 Background color.

-> Int	 Number of simulation steps to take for each second of real time.

-> world	The initial world.

-> (world -> Picture)	 A function to convert the world a picture.

-> (Event -> world -> world)	 A function to handle input events.

-> (Float -> world -> world)	 A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced.

-> IO ()	 
-}


blocos1 :: [[Bloco]]
blocos1 = [ [ Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio]
          , [ Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio]
          , [ Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio]
          , [ Vazio, Alcapao, Plataforma, Plataforma, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma, Vazio]
          , [ Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
          , [ Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
          , [ Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]]

gameMap1 :: Mapa
gameMap1 = Mapa ((8.5, 6.5), Este) (5, 1.5) blocos1

pl1 = Personagem (0.0,0.0) Jogador (8.5,7) Oeste (0.8,0.8) False False 10 0 (True, 10.0)

en1 = Personagem (0.0,0.0) Fantasma (8,7) Este (0.8,0.8) False True 10 0 (False, 0.0)
en2 = Personagem (0.0,0.0) Fantasma (8.7,7) Este (0.8,0.8) False True 10 0 (False, 0.0)

c1 = (Martelo, (5,1))

j1 = Jogo gameMap1 [en1,en2] [c1] pl1
