module Main where

import LI12324
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Imagens
--import Tarefa2 (valida)
import Tarefa3 (movimenta)
--import Tarefa4 (atualiza)


data Estado = Estado --adicionar o resto
  {
    modo :: Modo,
    jogo :: Jogo,
    tempo :: Tempo
  }



data Modo = EmJogo | MenuInicial MenuInicialOp | Pausa PausaOp | Mensagem MensagemOp | OpcoesOp

data MenuInicialOp = Jogar | Sair | Opcoes

data OpcoesOp = Skins -- mais alguma coisa?

data PausaOp = VoltaMenu | RetomaJogo deriving (Show,Eq)

data MensagemOp = Vitoria | Derrota
  deriving (Show, Eq)



-- funções desenha

desenha :: Estado -> IO Picture
desenha estado@Estado {modo= modo} = case modo of
  MenuInicial op -> desenhaMenu estado
  Pausa op -> desenhaMenu estado
  EmJogo -> desenhaJogo estado
  Mensagem op -> desenhaMensagem op
  --OpcoesOp

desenhaMenu :: Estado -> IO Picture
desenhaMenu Estado {modo = MenuInicial Jogar} = menujogar
desenhaMenu Estado {modo = MenuInicial Sair} = menusair
desenhaMenu Estado {modo = MenuInicial Opcoes} = menuopcoes
desenhaMenu Estado {modo = Pausa RetomaJogo} =
  return $ Pictures [Color blue opcaoRetomaJogo, Color white opcaoSair]
desenhaMenu Estado {modo = Pausa VoltaMenu} =
  return $ Pictures [Color white opcaoRetomaJogo, Color blue opcaoSair]
desenhaMenu _ = return $ Pictures []

opcaoSair = Translate (-150) (-100) $ Text "Sair"
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
desenhaJogador Estado {modo= EmJogo, tempo= t, jogo= Jogo {jogador=
  Personagem {velocidade= (0,0), posicao = pos, direcao=dir, tamanho= tam, emEscada= False, aplicaDano= (False, d)},mapa = mapa}} =
    let tamcomp = tamanhoCompMapa mapa
    in translateParaPos pos tamcomp . turnEste dir . tamanhoscale tam $ marioparado
desenhaJogador Estado {modo= EmJogo, tempo= t, jogo= Jogo {jogador=
  Personagem {velocidade= (0,0), posicao = pos, direcao=dir, tamanho= tam, emEscada= False, aplicaDano= (True, d)}, mapa = mapa}} =
    let tamcomp = tamanhoCompMapa mapa
    in translateParaPos pos tamcomp . turnEste dir . tamanhoscale tam $ if (mod (round (t*1000)) 200) < 100 then
                                                                                           mariomarteloup
                                                                                           else mariomartelodown
desenhaJogador Estado {modo= EmJogo, tempo= t, jogo= Jogo {jogador=
  Personagem {posicao = pos, direcao=dir, tamanho= tam, emEscada= False, aplicaDano= (False, d)},mapa = mapa}} =
    let tamcomp = tamanhoCompMapa mapa
    in translateParaPos pos tamcomp . turnEste dir . tamanhoscale tam $ if (mod (round (t*1000)) 200) < 100 then
                                                                                            marioanda1
                                                                                            else marioanda2
desenhaJogador Estado {modo= EmJogo, tempo= t, jogo= Jogo {jogador=
  Personagem {posicao = pos, direcao=dir, tamanho= tam, emEscada= False, aplicaDano= (True, d)},mapa = mapa}} =
    let tamcomp = tamanhoCompMapa mapa
    in translateParaPos pos tamcomp . turnEste dir . tamanhoscale tam $ if (mod (round (t*1000)) 200) < 100 then
                                                                                            mariomarteloupanda
                                                                                            else mariomarteloandadown
desenhaJogador Estado {modo= EmJogo, tempo= t, jogo= Jogo {jogador=
  Personagem {posicao = pos, direcao=dir, tamanho= tam, emEscada= True, aplicaDano= (False, d)},mapa = mapa}} =
    let tamcomp = tamanhoCompMapa mapa
    in translateParaPos pos tamcomp . tamanhoscale tam $ if (mod (round (t*1000)) 200) < 100 then
                                                                              mariosubir
                                                                              else turnEste Este marioanda2

-- falta morte

desenhaColec :: Estado -> IO [Picture] --mapM converte a função de [IO Picture] para IO [Picture]
desenhaColec Estado {modo = EmJogo, jogo = Jogo {colecionaveis = l,mapa= mapa}} = 
  let 
    t = tamanhoCompMapa mapa
  in
    mapM (\(c, pos) -> case c of 
                        Martelo -> translateParaPos pos t martelo
                        Moeda   -> translateParaPos pos t coin) l


desenhaFantasmas :: Estado -> IO [Picture]
desenhaFantasmas Estado {modo = EmJogo, jogo = Jogo {inimigos = l, mapa= mapa}, tempo =temp} =
  let 
    t=tamanhoCompMapa mapa
  in
    mapM (\Personagem
            { posicao = pos
            , direcao = dir
            , tamanho = tam
            } -> translateParaPos pos t. turnEste dir . tamanhoscale tam $ (if (mod (round (temp*1000)) 200) < 100 then fantasma1 else fantasma2)) l


desenhaJogo :: Estado -> IO Picture
desenhaJogo e = do
  mapa <- mapapicture e
  fant <- desenhaFantasmas e
  colec <- desenhaColec e
  jogador <- desenhaJogador e
  let fant2 = pictures fant
      colec2 = pictures colec
  return $ mapa <> colec2 <> fant2 <> jogador -- <> junta as imagens

tamanhoCompMapa :: Mapa -> (Float,Float)
tamanhoCompMapa (Mapa _ _  mapa) = (fromIntegral (length (head mapa)) * blockSize, fromIntegral (length mapa) * blockSize)
  
blockSize :: Float
blockSize = 80

blockPicture :: Bloco -> IO Picture
blockPicture Vazio = return $ Color black $ rectangleSolid blockSize blockSize
blockPicture Plataforma = plataforma
blockPicture Escada = escada
blockPicture Alcapao = alcapao  

rowToPicture :: [Bloco] -> IO Picture
rowToPicture row = do
  blockPictures <- sequence $ zipWith (\block x -> Translate (fromIntegral x * blockSize) 0 <$> blockPicture block) row [0..]
  return $ Pictures blockPictures

mapToPicture :: Estado -> IO Picture
mapToPicture Estado {jogo = Jogo {mapa= Mapa _ _ mapa }} = do
    rows <- sequence $ zipWith (\row y -> Translate 0 (-fromIntegral y * blockSize) <$> rowToPicture row) mapa [0..]
    return $ Pictures rows

mapapicture :: Estado -> IO Picture
mapapicture e@(Estado {jogo = Jogo {mapa= mapa}}) = do
    mapaPic <- mapToPicture e
    let (mapWidth, mapHeight) = tamanhoCompMapa mapa
    return $ Translate (-mapWidth/2 + blockSize/2) (mapHeight/2 - blockSize/2) mapaPic



-- funcoes uteis 
turnEste :: Direcao -> IO Picture -> IO Picture
turnEste Este p= do
  scale (-1) 1 <$> p
turnEste _ p = p

tamanhoscale :: (Double,Double) -> IO Picture -> IO Picture
tamanhoscale (x,y) p= do
  scale (realToFrac x) (realToFrac y) <$> p

translateParaPos :: Posicao -> (Float,Float) -> IO Picture -> IO Picture
translateParaPos (x,y) (w,h) p = do
  translate ((realToFrac x)* blockSize -w/2) (h/2 - (realToFrac y)* blockSize) <$> p

-- funcao coordenada para pixeis: x * 80 - 400; y * 80 + 420

-- funções reage

reageEvento :: Event -> Estado -> IO Estado
reageEvento evento estado@Estado {modo= modo} = case modo of
  MenuInicial jogar -> menureage evento estado -- Passar o estado corrente para o reageMenu
  EmJogo -> jogoreage evento estado
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
  return e {modo = EmJogo}
pausareage _ e = return e


menureage :: Event -> Estado -> IO Estado
menureage (EventKey (SpecialKey KeyDown) Down _ _) e@Estado {modo = MenuInicial Jogar} =
  return e {modo = MenuInicial Sair}
menureage (EventKey (SpecialKey KeyUp) Down _ _) e@Estado {modo = MenuInicial Sair} =
  return e {modo = MenuInicial Jogar}
menureage (EventKey (SpecialKey KeyUp) Down _ _) e@Estado {modo = MenuInicial Jogar} =
  return e {modo = MenuInicial Opcoes}
menureage (EventKey (SpecialKey KeyDown) Down _ _) e@Estado {modo = MenuInicial Sair} =
  return e {modo = MenuInicial Opcoes}
menureage (EventKey (SpecialKey KeyDown) Down _ _) e@Estado {modo = MenuInicial Opcoes } =
  return e {modo = MenuInicial Jogar}
menureage (EventKey (SpecialKey KeyUp) Down _ _) e@Estado {modo = MenuInicial Opcoes} =
  return e {modo = MenuInicial Sair}
menureage (EventKey (SpecialKey KeyEnter) Down _ _) e@Estado {modo = MenuInicial Jogar} =
  return e {modo = EmJogo}
menureage (EventKey (SpecialKey KeyEnter) Down _ _) e@Estado {modo = MenuInicial Sair} = error "Sair do jogo"
menureage (EventKey (SpecialKey KeyEnter) Down _ _) e@Estado {modo = MenuInicial Opcoes} = 
    return e {modo = OpcoesOp}
menureage _ e = return e


jogoreage :: Event -> Estado -> IO Estado         -- !!!
jogoreage (EventKey (SpecialKey KeyEsc) Down _ _) e@Estado {modo = EmJogo} =
  return e {modo = Pausa RetomaJogo}
jogoreage (EventKey (SpecialKey KeyEsc) Down _ _) e@Estado {modo = Pausa _} =
  return e {modo = EmJogo}
jogoreage _ e = return e


reageMensagem :: Event -> MensagemOp -> Estado -> IO Estado
reageMensagem (EventKey (SpecialKey KeyEnter) Down _ _) _ e@Estado {modo = modo}=
  return (e {modo= MenuInicial Jogar}) -- Retorne ao menu após pressionar Enter
reageMensagem _ estado e@Estado {modo = modo} = return (e{modo=Mensagem estado}) -- Mantenha o estado atual se outros eventos ocorrerem


janela :: Display
janela = InWindow
       "DK"
       (1440, 1080)
       (0,0)

corFundo = black

fr:: Int
fr = 60

tempof :: Float -> Estado -> IO Estado
tempof t w =return w {tempo=realToFrac t + tempo w}

{-
tempof :: Float -> Estado -> IO Estado
tempof _ e@(Estado {modo = Pausa _, tempo = t}) = return e {tempo = t}
tempof _ e@(Estado {modo = MenuInicial _}) = return e {tempo = 0}
tempof t estado = do
  let
    temponovo = (realToFrac t) + tempo estado
    novojogo = movimenta 100 (realToFrac t) (jogo estado)
    desenho = desenhaJogo estado{jogo=novojogo}
  return estado {tempo = temponovo, jogo = novojogo}
-}

estadoInicial = Estado
  { modo = MenuInicial Jogar
  , jogo = jogo01
  , tempo = 0
  }

main :: IO ()
main = do
  --if valida $ jogo estadoInicial 
    --then do
      putStrLn "Jogo válido\nA carregar..."
      playIO janela corFundo fr estadoInicial desenha reageEvento tempof
  --  else do
    --  putStrLn "Jogo inválido"


{-

:: Display	 Display mode.

-> Color	 Background color.

-> Int	 Number of simulation steps to take for each second of real time.

-> world	The initial world.

-> (world -> Picture)	 A function to convert the world a picture.

-> (Event -> world -> world)	 A function to handle input events.

-> (Float -> world -> world)	 A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced.


-}


mapa2 :: Mapa
mapa2 =
  Mapa
    ((8.5, 6.5), Este)
    (5, 1.5)
    [ [Plataforma,Vazio,Plataforma],
      [Escada, Vazio, Vazio],
      [Plataforma,Alcapao,Plataforma]]

mapa01 :: Mapa
mapa01 =
  Mapa
    ((8.5, 6.5), Este)
    (5, 1.5)
    [ [Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio],
      [Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio],
      [Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio],
      [Vazio, Plataforma, Plataforma, Plataforma, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma, Vazio],
      [Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],
      [Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],
      [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ]

inimigoModelo =
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
      posicao = (4,10.5),
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
      inimigos = [inimigoModelo, inimigoModelo],
      colecionaveis = [(Moeda, (5.5,4.5)), (Martelo,(4,10))],
      jogador = jogadorParado}