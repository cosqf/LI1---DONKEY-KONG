module Main where

import LI12324
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Tarefa2 (valida)
import Tarefa3 (movimenta)
import Desenha (desenha)
import Reage (reageEvento)
import Tarefa5
import Imagens (getImages, Imagem)

janela :: Display
janela = InWindow
       "DK"
       (1440, 1080)
       (0,0)

corFundo = black

fr:: Int
fr = 25

tempof :: Float -> Estado -> IO Estado
tempof _ e@(Estado {modo= Pausa _}) = return e
tempof _  e@(Estado {modo= Mensagem _}) = return e
tempof _ e@(Estado {modo= MenuInicial _}) = return e {tempo= 0}
tempof t e = 
  return $ e {jogo = movimenta 100 (realToFrac t) (jogo e), tempo= (tempo e) + realToFrac t}

main :: IO ()
main = do
  images <- getImages
  let estadoInicial = Estado {modo = MenuInicial Menu, jogo = jogo01, tempo = 0, imagens = images}
  if valida $ jogo estadoInicial
    then do
      putStrLn "Jogo válido\nA carregar..."
      playIO janela corFundo fr estadoInicial desenha reageEvento tempof
    else do
      putStrLn "Jogo inválido"

{-

:: Display	 Display mode.

-> Color	 Background color.

-> Int	 Number of simulation steps to take for each second of real time.

-> world	The initial world.

-> (world -> Picture)	 A function to convert the world a picture.

-> (Event -> world -> world)	 A function to handle input events.

-> (Float -> world -> world)	 A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced.


-}


mapa02 :: Mapa
mapa02 =
  Mapa
    ((8.5, 6.5), Este)
    (5, 1.5)
    [ [Plataforma,Vazio,Plataforma],
      [Escada, Vazio, Vazio],
      [Plataforma,Alcapao,Plataforma]]

mapa01 :: Mapa
mapa01 =
  Mapa
    ((6.5, 1.5), Este)
    (5, 1.5)
    [ [Plataforma, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Plataforma],
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

inimigoModelo2 =
  Personagem
    { velocidade = (0.0, 0.0),
      tipo = Fantasma,
      posicao = (3.5, 10.6),
      direcao = Este,
      tamanho = (1, 1),
      emEscada = False,
      ressalta = True,
      vida = 1,
      pontos = 0,
      aplicaDano = (False, 0)
    }
dk =
  Personagem
    { velocidade = (0.0, 0.0),
      tipo = MacacoMalvado,
      posicao = (5, 4.5),
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
      posicao = (6.5, 1.5),
      direcao = Oeste,
      tamanho = (0.8, 0.8),
      emEscada = False,
      ressalta = False,
      vida = 3,
      pontos = 0,
      aplicaDano = (False, 0)
    }
martelo = (Martelo, (7, 4.5))
moeda = (Moeda, (6, 7.5))

jogo01 :: Jogo
jogo01 =
  Jogo
    { mapa = mapa01,
      inimigos = [inimigoModelo, inimigoModelo2],
      colecionaveis = [moeda],
      jogador = jogadorParado
    }
