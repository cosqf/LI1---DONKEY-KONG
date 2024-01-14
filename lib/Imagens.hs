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
  Just mariorip <-fmap scalePicture <$> loadJuicyPNG "lib/Imagens/mariorip.png"
  Just mariosubirfim <-fmap scalePicture <$> loadJuicyPNG "lib/Imagens/mariosubirfim.png"
  Just mariosubir <-fmap scalePicture <$> loadJuicyPNG "lib/Imagens/mariosubir.png"
  Just mariosubir3 <-fmap scalePicture <$> loadJuicyPNG "lib/Imagens/mariosubir3.png"
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
  Just pauline1 <-fmap scalePicture <$> loadJuicyPNG "lib/Imagens/pauline1.png"
  Just pauline2 <-fmap scalePicture <$> loadJuicyPNG "lib/Imagens/pauline2.png"
  Just dkmove <-fmap (scale 5 5) <$> loadJuicyPNG "lib/Imagens/dkmove.png"
  Just dkparado <-fmap (scale 5 5) <$> loadJuicyPNG "lib/Imagens/dkparado.png"
  Just gameover <- fmap (scale 1 1) <$> loadJuicyPNG "lib/Imagens/gameover.png"
  Just gamewin <- fmap (scale 1 1) <$> loadJuicyPNG "lib/Imagens/gamewin.png"
  Just zero <- fmap (scale 1 1) <$> loadJuicyPNG "lib/Imagens/0.png"
  Just um <- fmap (scale 1 1) <$> loadJuicyPNG "lib/Imagens/1.png"
  Just dois <- fmap (scale 1 1) <$> loadJuicyPNG "lib/Imagens/2.png"
  Just tres <- fmap (scale 1 1) <$> loadJuicyPNG "lib/Imagens/3.png"
  Just quatro <- fmap (scale 1 1) <$> loadJuicyPNG "lib/Imagens/4.png"
  Just cinco <- fmap (scale 1 1) <$> loadJuicyPNG "lib/Imagens/5.png"
  Just seis <- fmap (scale 1 1) <$> loadJuicyPNG "lib/Imagens/6.png"
  Just sete <- fmap (scale 1 1) <$> loadJuicyPNG "lib/Imagens/7.png"
  Just oito <- fmap (scale 1 1) <$> loadJuicyPNG "lib/Imagens/8.png"
  Just nove <- fmap (scale 1 1) <$> loadJuicyPNG "lib/Imagens/9.png"

  return [
    ("marioanda1", marioanda1),
    ("marioanda2", marioanda2),
    ("mariomarteloandadown", mariomarteloandadown),
    ("mariomartelodown",translate (-35) 0 mariomartelodown),
    ("mariomarteloup", translate 0 27 mariomarteloup),
    ("mariomarteloupanda", translate 0 40 mariomarteloupanda),
    ("mariomarteloupanda2", translate 0 40 mariomarteloupanda2),
    ("mariomorto1", mariomorto1),
    ("mariomorto2", mariomorto2),
    ("mariomorto3", mariomorto3),
    ("mariomorto4", mariomorto4),
    ("marioparado", marioparado),
    ("mariorip", mariorip),
    ("mariosubirfim", mariosubirfim),
    ("mariosubir", mariosubir),
    ("mariosubir3", mariosubir3),
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
    ("plataforma",plataforma),
    ("pauline1", pauline1),
    ("pauline2", pauline2),
    ("dkmove", translate 0 50 dkmove),
    ("dkparado",translate 0 50 dkparado),
    ("gameover", gameover),
    ("gamewin", gamewin),
    ("0", zero),
    ("1", um),
    ("2", dois),
    ("3", tres),
    ("4", quatro),
    ("5", cinco),
    ("6", seis),
    ("7", sete),
    ("8", oito),
    ("9", nove)
    ]

obterimagem :: String -> Imagem -> IO Picture
obterimagem n imgs = do
  case lookup n imgs of
    Just img -> return img
    Nothing  -> error "imagem não encontrada"

