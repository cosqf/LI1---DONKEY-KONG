module Imagens where 
import Graphics.Gloss
import Graphics.Gloss.Juicy


type Imagem = [(String, Picture)]

getImages :: IO Imagem
getImages = do
  Just marioanda1 <- loadJuicyPNG "lib/imagens/marioanda1.png"
  Just marioanda2 <- loadJuicyPNG "lib/imagens/marioanda2.png"
  Just mariomarteloandadown <- loadJuicyPNG "lib/imagens/mariomarteloandadown.png"
  Just mariomartelodown <- loadJuicyPNG "lib/imagens/mariomartelodown.png"
  Just mariomarteloup <- loadJuicyPNG "lib/imagens/mariomarteloup.png"
  Just mariomarteloupanda <- loadJuicyPNG "lib/imagens/mariomarteloupanda.png"
  Just mariomarteloupanda2 <- loadJuicyPNG "lib/imagens/mariomarteloupanda2.png"
  Just mariomorto1 <- loadJuicyPNG "lib/imagens/mariomorto1.png"
  Just mariomorto2 <- loadJuicyPNG "lib/imagens/mariomorto2.png"
  Just mariomorto3 <- loadJuicyPNG "lib/imagens/mariomorto3.png"
  Just mariomorto4 <- loadJuicyPNG "lib/imagens/mariomorto4.png"
  Just marioparado <- loadJuicyPNG "lib/imagens/marioparado.png"
  Just mariopulo <- loadJuicyPNG "lib/imagens/mariopulo.png"
  Just mariorip <- loadJuicyPNG "lib/imagens/mariorip.png"
  Just mariosubirfim <- loadJuicyPNG "lib/imagens/mariosubirfim.png"
  Just mariosubir <- loadJuicyPNG "lib/imagens/mariosubir.png"

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
    ("mariosubir", mariosubir)
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
