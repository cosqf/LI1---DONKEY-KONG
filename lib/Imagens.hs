module Imagens where 
import Graphics.Gloss
import Graphics.Gloss.Juicy


type Imagem = [(String, Picture)]

scalePicture :: Picture -> Picture
scalePicture = scale 5.6 5.6

getImages :: IO Imagem
getImages = do
  Just marioanda1 <- fmap scalePicture <$> loadJuicyPNG "lib/Imagens/marioanda1.png"
  Just marioanda2 <-fmap scalePicture <$> loadJuicyPNG "lib/Imagens/marioanda2.png"
  Just mariomarteloandadown <-fmap scalePicture <$> loadJuicyPNG "lib/Imagens/mariomarteloandadown.png"
  Just mariomartelodown <-fmap scalePicture <$> loadJuicyPNG "lib/Imagens/mariomartelodown.png"
  Just mariomarteloup <-fmap scalePicture <$> loadJuicyPNG "lib/Imagens/mariomarteloup.png"
  Just mariomarteloupanda <- fmap scalePicture <$>loadJuicyPNG "lib/Imagens/mariomarteloupanda.png"
  Just mariomarteloupanda2 <-fmap scalePicture <$> loadJuicyPNG "lib/Imagens/mariomarteloupanda2.png"
  Just mariomorto1 <-fmap scalePicture <$> loadJuicyPNG "lib/Imagens/mariomorto1.png"
  Just mariomorto2 <-fmap scalePicture <$> loadJuicyPNG "lib/Imagens/mariomorto2.png"
  Just mariomorto3 <-fmap scalePicture <$> loadJuicyPNG "lib/Imagens/mariomorto3.png"
  Just mariomorto4 <-fmap scalePicture <$> loadJuicyPNG "lib/Imagens/mariomorto4.png"
  Just marioparado <-fmap scalePicture <$> loadJuicyPNG "lib/Imagens/marioparado.png"
  Just mariopulo <-fmap scalePicture <$> loadJuicyPNG "lib/Imagens/mariopulo.png"
  Just mariorip <-fmap scalePicture <$> loadJuicyPNG "lib/Imagens/mariorip.png"
  Just mariosubirfim <-fmap scalePicture <$> loadJuicyPNG "lib/Imagens/mariosubirfim.png"
  Just mariosubir <-fmap scalePicture <$> loadJuicyPNG "lib/Imagens/mariosubir.png"
  Just coin <-fmap scalePicture <$> loadJuicyPNG "lib/Imagens/coin.png"
  Just martelo <-fmap scalePicture <$> loadJuicyPNG "lib/Imagens/martelo.png"
  Just fantasma1 <- fmap scalePicture <$> loadJuicyPNG "lib/Imagens/fantasma.png"
  Just fantasma2 <- fmap scalePicture <$> loadJuicyPNG "lib/Imagens/fantasma2.png"
  Just menu <- fmap (scale 1 1) <$> loadJuicyPNG "lib/Imagens/donkeykongmenu.png"
  Just menujogar <- fmap (scale 1 1) <$> loadJuicyPNG "lib/Imagens/donkeykongmenujogar.png"
  Just menusair <- fmap (scale 1 1) <$> loadJuicyPNG "lib/Imagens/donkeykongmenusair.png"
  Just menuopcoes <- fmap (scale 1 1) <$> loadJuicyPNG "lib/Imagens/donkeykongmenuopcoes.png"
  Just alcapao <- fmap (scale 10 10) <$> loadJuicyPNG "lib/Imagens/alcapao.png"
  Just escada <- fmap (scale 10 10) <$> loadJuicyPNG "lib/Imagens/escada.png"
  Just plataforma <- fmap (scale 10 10) <$> loadJuicyPNG "lib/Imagens/plataforma.png"

  return [
    ("marioanda1", marioanda1),
    ("marioanda2", marioanda2),
    ("mariomarteloandadown", mariomarteloandadown),
    ("mariomartelodown", mariomartelodown),
    ("mariomarteloup", mariomarteloup),
    ("mariomarteloupanda", mariomarteloupanda),
    ("mariomarteloupanda2", mariomarteloupanda2),
    ("mariomorto1", mariomorto1),
    ("mariomorto2", mariomorto2),
    ("mariomorto3", mariomorto3),
    ("mariomorto4", mariomorto4),
    ("marioparado", marioparado),
    ("mariopulo", mariopulo),
    ("mariorip", mariorip),
    ("mariosubirfim", mariosubirfim),
    ("mariosubir", mariosubir),
    ("coin", coin),
    ("martelo", martelo),
    ("fantasma1", fantasma1),
    ("fantasma2", fantasma2),
    ("menu",menu),
    ("menujogar", menujogar),
    ("menusair", menusair),
    ("menuopcoes",menuopcoes),
    ("alcapao", alcapao),
    ("escada",escada),
    ("plataforma",plataforma)
    ]

obterimagem :: String -> IO Imagem -> IO Picture
obterimagem n imgs = do
  images <- imgs
  case lookup n images of
    Just img -> return img
    Nothing  -> error ""


marioanda1 = obterimagem "marioanda1" getImages
marioanda2 = obterimagem "marioanda2" getImages
mariomarteloandadown = obterimagem "mariomarteloandadown" getImages
mariomartelodown = obterimagem "mariomartelodown" getImages
mariomarteloup = obterimagem "mariomarteloup" getImages
mariomarteloupanda = obterimagem "mariomarteloupanda" getImages
mariomarteloupanda2 = obterimagem "mariomarteloupanda2" getImages
mariomorto1 = obterimagem "mariomorto1" getImages
mariomorto2 = obterimagem "mariomorto2" getImages
mariomorto3 = obterimagem "mariomorto3" getImages
mariomorto4 = obterimagem "mariomorto4" getImages
marioparado = obterimagem "marioparado" getImages
mariopulo = obterimagem "mariopulo" getImages
mariorip = obterimagem "mariorip" getImages
mariosubirfim = obterimagem "mariosubirfim" getImages
mariosubir = obterimagem "mariosubir" getImages
coin = obterimagem "coin" getImages 
martelo = obterimagem "martelo" getImages
fantasma1 = obterimagem "fantasma1" getImages
fantasma2 = obterimagem "fantasma2" getImages
menu = obterimagem "menu" getImages
menujogar = obterimagem "menujogar" getImages
menuopcoes = obterimagem "menuopcoes" getImages
menusair = obterimagem "menusair" getImages
alcapao = obterimagem "alcapao" getImages
escada = obterimagem "escada" getImages
plataforma = obterimagem "plataforma" getImages