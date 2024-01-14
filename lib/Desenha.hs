{-# LANGUAGE BlockArguments #-}
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
  Mensagem op -> desenhaMensagem estado op
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
desenhaMensagem :: Estado -> MensagemOp -> IO Picture
desenhaMensagem (Estado {imagens = imgs, tempo = t, jogo = Jogo {jogador = Personagem {pontos = p}}}) op =
  let
    funcT :: Int -> (Int -> IO Picture) -> [Int] -> IO [Picture]
    funcT _ _ [] = return []
    funcT acc fun nums
      | acc == 0 = do
        pic <- fun (head nums)
        rest <- funcT (acc + 1) fun (tail nums)
        return (pic : rest)
      | otherwise = do
        pic <- fun (head nums)
        rest <- funcT (acc + 1) fun (tail nums)
        return (Translate (100 * fromIntegral acc) 0 pic : rest)

    digitize :: Int -> [Int]
    digitize num = map digitToInt (show num)
      where
        digitToInt :: Char -> Int
        digitToInt = read . return

    ima :: Int -> IO Picture  -- Change the type of ima
    ima num = case num of
      0 -> obterimagem "0" imgs
      1 -> obterimagem "1" imgs
      2 -> obterimagem "2" imgs
      3 -> obterimagem "3" imgs
      4 -> obterimagem "4" imgs
      5 -> obterimagem "5" imgs
      6 -> obterimagem "6" imgs
      7 -> obterimagem "7" imgs
      8 -> obterimagem "8" imgs
      9 -> obterimagem "9" imgs

    mensagem = case op of
      Derrota -> obterimagem "gameover" imgs
      Vitoria -> do
        winImg <- obterimagem "gamewin" imgs
        temponum <- funcT 0 ima (digitize (round t))
        pontosnum <- funcT 0 ima (digitize p)
        return $ winImg <> Translate (-200) 200 (pictures temponum) <> Translate (-100) 390 (pictures pontosnum)
  in mensagem






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
desenhaInimigos :: Estado -> IO [Picture]
desenhaInimigos Estado {modo = EmJogo, imagens=imgs, jogo = Jogo {inimigos = [], mapa = mapa}, tempo = temp} = return [blank]
desenhaInimigos Estado {modo = EmJogo, imagens=imgs, jogo = Jogo {inimigos = l, mapa = mapa}, tempo = temp} =
  mapM (desenhaFant mapa temp imgs) l <> (mapM (desenhaDK mapa temp imgs) l)

desenhaFant :: Mapa -> Tempo -> Imagem  -> Personagem -> IO Picture
desenhaFant mapa temp imgs (Personagem {vida = v, tipo = Fantasma, posicao = pos, direcao = dir, tamanho = tam})= do
  let
    t = tamanhoCompMapa mapa
    getFantasmaPic
      | v <= 0 = return blank
      | (mod (round (temp * 1000)) 1000) < 500 = obterimagem "fantasma1" imgs
      | otherwise = obterimagem "fantasma2" imgs
  translateParaPos pos t . turnEste dir . tamanhoscale tam $ getFantasmaPic
desenhaFant _ _ _ _ = return blank

desenhaDK :: Mapa -> Tempo -> Imagem -> Personagem -> IO Picture
desenhaDK _ _ _ (Personagem {tipo = Fantasma}) = return blank
desenhaDK mapa temp imgs (Personagem {vida = v, tipo = MacacoMalvado, posicao = pos, direcao = dir, tamanho = tam})
  | (mod (round (temp * 1000)) 3000) < 1500 = translateParaPos pos ta  . tamanhoscale tam $ obterimagem "dkparado" imgs
  | otherwise = dk
    where
      ta = tamanhoCompMapa mapa
      dk
        | (mod (round (temp * 1000)) 1000) < 500 = translateParaPos pos ta . tamanhoscale tam $ obterimagem "dkmove" imgs
        |otherwise = translateParaPos pos ta . turnEste Este . tamanhoscale tam $ obterimagem "dkmove" imgs



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
  inimigos <- desenhaInimigos e
  colec <- desenhaColec e
  jogador <- desenhaJogador e
  let ini = pictures inimigos
      colec2 = pictures colec
  return $ mapa <> colec2 <> pauline <> ini <> jogador -- <> junta as imagens


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
