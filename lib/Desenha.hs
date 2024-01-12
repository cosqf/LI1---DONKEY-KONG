module Desenha where
import LI12324
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Imagens
import Tarefa5



desenha :: Estado -> IO Picture
desenha estado@Estado {modo= modo} = case modo of
  MenuInicial op -> desenhaMenu estado
  Pausa op -> desenhaMenu estado
  EmJogo -> desenhaJogo estado
  Mensagem op -> desenhaMensagem op
  --OpcoesOp

desenhaMenu :: Estado -> IO Picture
desenhaMenu Estado {modo = MenuInicial Menu} = menu
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

desenhaColec :: Estado -> IO [Picture] 
desenhaColec Estado {modo = EmJogo, jogo = Jogo {colecionaveis = l,mapa= mapa}} = 
  let 
    t = tamanhoCompMapa mapa
  in
    mapM (\(c, pos) -> case c of                  --mapM converte a função de [IO Picture] para IO [Picture]
                        Martelo -> translateParaPos pos t martelo
                        Moeda   -> translateParaPos pos t coin) l


desenhaFantasmas :: Estado -> IO [Picture]
desenhaFantasmas Estado {modo = EmJogo, jogo = Jogo {inimigos = l, mapa = mapa}, tempo = temp} =
  let
    t = tamanhoCompMapa mapa
    isFantasma1 = mod (round temp) 2 == 0
    getFantasmaPic = if isFantasma1 then fantasma1 else fantasma2
  in
    mapM (\Personagem
            { posicao = pos
            , direcao = dir
            , tamanho = tam
            } -> translateParaPos pos t . turnEste dir . tamanhoscale tam $ getFantasmaPic) l


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


mapapicture :: Estado -> IO Picture
mapapicture e@(Estado {jogo = Jogo {mapa= mapa}}) = do
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
      blockPicture Plataforma = plataforma
      blockPicture Escada = escada
      blockPicture Alcapao = alcapao          

    mapaPic <- mapToPicture e
    return $ Translate (-mapWidth/2 + blockSize/2) (mapHeight/2 - blockSize/2) mapaPic

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


