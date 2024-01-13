module Desenha where
import LI12324
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Imagens
import Tarefa5
import Funcoes

-- | Função geral de desenhar o jogo, onde recebe todas as funções de desenho e desenha o modo que o jogo está de momento.
desenha :: Estado -> IO Picture
desenha estado@Estado {modo= modo} = case modo of
  MenuInicial op -> desenhaMenu estado
  Pausa op -> desenhaMenu estado
  EmJogo -> desenhaJogo estado
  Mensagem op -> desenhaMensagem op
  --OpcoesOp

-- | Desenha o menu
desenhaMenu :: Estado -> IO Picture
desenhaMenu Estado {modo = MenuInicial Menu, imagens= imgs} = obterimagem "menu" imgs
desenhaMenu Estado {modo = MenuInicial Jogar, imagens= imgs} = obterimagem "menujogar" imgs
desenhaMenu Estado {modo = MenuInicial Sair, imagens= imgs} = obterimagem "menusair" imgs
desenhaMenu Estado {modo = MenuInicial Opcoes, imagens= imgs} = obterimagem "menuopcoes" imgs
desenhaMenu Estado {modo = Pausa RetomaJogo} =
  return $ Pictures [Color blue opcaoRetomaJogo, Color white opcaoSair]
desenhaMenu Estado {modo = Pausa VoltaMenu} =
  return $ Pictures [Color white opcaoRetomaJogo, Color blue opcaoSair]
desenhaMenu _ = return $ Pictures []

opcaoSair = Translate (-150) (-100) $ Text "Sair"
opcaoRetomaJogo = Translate (-150) (100) $ Text "Jogar"

-- | Desenha a mesagem de vitória/derrota
desenhaMensagem :: MensagemOp -> IO Picture
desenhaMensagem op =
  return $
    Pictures
      [ Translate (-150) 100 $ Color blue $ mensagem,
        Translate (-850) (-100) $ Text "Pressione Enter para retornar ao menu"
      ]
  where
    mensagem = case op of
      Vitoria -> Text "Parabéns! Venceu!"
      Derrota -> Text "Perdeu"


-- | Desenha o jogador
desenhaJogador:: Estado -> IO Picture
desenhaJogador Estado {modo= EmJogo, tempo= t,imagens=imgs, jogo= Jogo {jogador=
  Personagem {velocidade= (0,0), posicao = pos, direcao=dir, tamanho= tam, emEscada= False, aplicaDano= (False, d)},mapa = mapa}} =
    let tamcomp = tamanhoCompMapa mapa
    in translateParaPos pos tamcomp . turnEste dir . tamanhoscale tam $ obterimagem "marioparado" imgs
desenhaJogador Estado {modo= EmJogo, tempo= t,imagens=imgs, jogo= Jogo {jogador=
  Personagem {velocidade= (0,0), posicao = pos, direcao=dir, tamanho= tam, emEscada= False, aplicaDano= (True, d)}, mapa = mapa}} =
    let tamcomp = tamanhoCompMapa mapa
    in translateParaPos pos tamcomp . turnEste dir . tamanhoscale tam $ if (mod (round (t * 1000)) 500) < 250 then
                                                                                           obterimagem "mariomarteloup" imgs
                                                                                           else obterimagem "mariomartelodown" imgs
desenhaJogador Estado {modo= EmJogo, tempo= t,imagens=imgs, jogo= Jogo {jogador=
  Personagem {posicao = pos, direcao=dir, tamanho= tam, emEscada= False, aplicaDano= (False, d)},mapa = mapa}} =
    let tamcomp = tamanhoCompMapa mapa
    in translateParaPos pos tamcomp . turnEste dir . tamanhoscale tam $ if (mod (round (t * 1000)) 500) < 250 then
                                                                                           obterimagem "marioanda1" imgs
                                                                                            else obterimagem "marioanda2" imgs
desenhaJogador Estado {modo= EmJogo, tempo= t,imagens=imgs, jogo= Jogo {jogador=
  Personagem {posicao = pos, direcao=dir, tamanho= tam, emEscada= False, aplicaDano= (True, d)},mapa = mapa}} =
    let tamcomp = tamanhoCompMapa mapa
    in translateParaPos pos tamcomp . turnEste dir . tamanhoscale tam $ if (mod (round (t * 1000)) 500) < 250 then
                                                                                           obterimagem "mariomarteloupanda" imgs
                                                                                            else obterimagem "mariomarteloandadown" imgs
desenhaJogador Estado {modo= EmJogo, tempo= t,imagens=imgs, jogo= Jogo {jogador=
  Personagem {posicao = pos, direcao=dir, tamanho= tam, emEscada= True}, mapa = mapa}}
  |blocopos pos mapa == Vazio = translateParaPos pos tamcomp . tamanhoscale tam $ if (mod (round (t * 1000)) 500) < 250 then
                                                                            obterimagem "mariosubirfim" imgs
                                                                              else obterimagem "mariosubir3" imgs                                                                           
  |otherwise= translateParaPos pos tamcomp . tamanhoscale tam $ if (mod (round (t * 1000)) 500) < 250 then
                                                                            obterimagem "mariosubir" imgs
                                                                              else turnEste Este (obterimagem "mariosubir" imgs)
    where tamcomp = tamanhoCompMapa mapa
-- falta morte

-- | Desenha os colecionáveis
desenhaColec :: Estado -> IO [Picture] 
desenhaColec Estado {modo = EmJogo, imagens= imgs, jogo = Jogo {colecionaveis = l,mapa= mapa}} = 
  let 
    t = tamanhoCompMapa mapa
  in
    mapM (\(c, pos) -> case c of                  --mapM converte a função de [IO Picture] para IO [Picture]
                        Martelo -> translateParaPos pos t (obterimagem "martelo" imgs)
                        Moeda   -> translateParaPos pos t ( obterimagem "coin" imgs)) l

-- | Desenha os fantasmas
desenhaFantasmas :: Estado -> IO [Picture]
desenhaFantasmas Estado {modo = EmJogo, imagens=imgs, jogo = Jogo {inimigos = [], mapa = mapa}, tempo = temp} = return [blank]
desenhaFantasmas Estado {modo = EmJogo, imagens=imgs, jogo = Jogo {inimigos = l@(Personagem {vida= v, tipo = Fantasma}:ps), mapa = mapa}, tempo = temp} =
    let
      t = tamanhoCompMapa mapa
      getFantasmaPic
        | v <= 0 = return blank
        | (mod (round (temp * 1000)) 1000) < 500 = obterimagem "fantasma1" imgs
        | otherwise= obterimagem "fantasma2" imgs
    in
      mapM (\Personagem
              { posicao = pos
              , direcao = dir
              , tamanho = tam
              } -> translateParaPos pos t . turnEste dir . tamanhoscale tam $ getFantasmaPic) l
desenhaFantasmas _ = return [blank]

desenhaDK :: Estado -> IO Picture
desenhaDK Estado {modo= EmJogo, tempo= t,imagens=imgs, jogo= Jogo {inimigos=
  (Personagem {tipo= MacacoMalvado, posicao = pos, tamanho= tam}:xs),mapa = mapa}}
  |(mod (round (t * 1000)) 3000) < 1500 = translateParaPos pos ta  . tamanhoscale tam $ obterimagem "dkparado" imgs
  |otherwise = dk 
    where 
      ta = tamanhoCompMapa mapa
      dk
          |(mod (round (t * 1000)) 1000) < 500 = translateParaPos pos ta  . tamanhoscale tam $ obterimagem "dkmove" imgs
          |otherwise= translateParaPos pos ta . tamanhoscale tam $ obterimagem "dkmove" imgs
desenhaDK _ = return blank


-- | Desenha a Pauline (o objetivo do jogo)
desenhaPauline :: Estado -> IO Picture 
desenhaPauline Estado {modo = EmJogo, imagens=imgs, jogo = Jogo {mapa= mapa@(Mapa _ p __)}, tempo= temp } 
  |(mod (round (temp * 1000)) 1000) < 500  = turnEste Este $ star
  |otherwise = star
    where
      t = tamanhoCompMapa mapa
      star
          |(mod (round (temp * 1000)) 500) < 250 = translateParaPos p t  . tamanhoscale (0.7,0.7) $ obterimagem "pauline1" imgs
          |otherwise= translateParaPos p t . tamanhoscale (0.7,0.7) $ obterimagem "pauline2" imgs

-- | Recebe as funções "desenha" que ocorrem durante o jogo e junta-as
desenhaJogo :: Estado -> IO Picture
desenhaJogo e = do
  pauline <- desenhaPauline e
  mapa <- mapapicture e
  fant <- desenhaFantasmas e
  colec <- desenhaColec e
  jogador <- desenhaJogador e
  let fant2 = pictures fant
      colec2 = pictures colec
  return $ mapa <> colec2 <> fant2 <> pauline <> jogador -- <> junta as imagens

tamanhoCompMapa :: Mapa -> (Float,Float)
tamanhoCompMapa (Mapa _ _  mapa) = (fromIntegral (length (head mapa)) * blockSize, fromIntegral (length mapa) * blockSize)

-- | Tamanho dos blocos por pixel.
blockSize :: Float
blockSize = 80

-- | Desenha o mapa
mapapicture :: Estado -> IO Picture
mapapicture e@(Estado {jogo = Jogo {mapa= mapa}, imagens= imgs}) = do
    let 
      (mapWidth, mapHeight) = tamanhoCompMapa mapa
      mapToPicture :: Estado -> IO Picture
      mapToPicture Estado {jogo = Jogo {mapa= Mapa _ _ mapa }} = do
        rows <- sequence $ zipWith (\row y -> Translate 0 (-fromIntegral y * blockSize) <$> rowToPicture row) mapa [0..]
        return $ Pictures rows
      
      rowToPicture :: [Bloco] -> IO Picture
      rowToPicture row = do
        blockPictures <- sequence $ zipWith (\block x -> Translate (fromIntegral x * blockSize) 0 <$> blockPicture block) row [0..]
        return $ Pictures blockPictures

      blockPicture :: Bloco -> IO Picture
      blockPicture Vazio = return $ Color black $ rectangleSolid blockSize blockSize
      blockPicture Plataforma = obterimagem "plataforma" imgs
      blockPicture Escada = obterimagem "escada" imgs
      blockPicture Alcapao = obterimagem "alcapao" imgs          

    mapaPic <- mapToPicture e
    return $ Translate (-mapWidth/2 + blockSize/2) (mapHeight/2 - blockSize/2) mapaPic

-- | Muda a direção das imagens baseado na direção dos personagens.
turnEste :: Direcao -> IO Picture -> IO Picture
turnEste Este p= do
  scale (-1) 1 <$> p
turnEste _ p = p

-- | Muda o tamanho das imagens baseado no tamanho dos personagens.
tamanhoscale :: (Double,Double) -> IO Picture -> IO Picture
tamanhoscale (x,y) p= do
  scale (realToFrac x) (realToFrac y) <$> p

-- | Move as imagens baseadas da sua posição e no tamanho do mapa
translateParaPos :: Posicao -> (Float,Float) -> IO Picture -> IO Picture
translateParaPos (x,y) (w,h) p = do
  translate ((realToFrac x)* blockSize -w/2) (h/2 - (realToFrac y)* blockSize) <$> p
