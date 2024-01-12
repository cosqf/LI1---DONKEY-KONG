{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : Ivo Filipe Mendes Vieira <a103999@alunos.uminho.pt>
              Filipa Cosquete Santos <a106837@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where
import Tarefa3
import LI12324
import Data.List
import System.Random 

{- | A função principal que verifica se o jogo é válido. Se tal não se verificar, o jogo crasha. 
Todas as funções que veremos de seguida são usadas por esta função.
-}
valida :: Jogo -> Bool
valida Jogo {mapa= m, inimigos= i, colecionaveis = c, jogador= j } = 
  chao m && validaJogador j && validaInimigo i && posicaoI i m && numI i && iniVida i && alcapaoL m j && escadaValida m && checkmario j m  && checkcolec c m

-- | Verifica se a última linha da matriz que estabelece o mapa é constituida apenas por plataformas.
chao :: Mapa -> Bool
chao (Mapa _ _ m) = all (==Plataforma) (last m)

-- | Verifica se a definição "ressalta" está definida no jogador como falsa.
validaJogador :: Personagem -> Bool
validaJogador (Personagem {ressalta=r}) = not r

-- | Verifica se a definição "ressalta" está definida nos inimigos como verdadeira.
validaInimigo :: [Personagem] -> Bool
validaInimigo = all ressalta

-- | Verifica se a posição inicial do jogador coincide com alguma dos inimigos.
posicaoI :: [Personagem] -> Mapa -> Bool
posicaoI [] m = True
posicaoI (Personagem {posicao = pf}:xs) m@(Mapa (pm,_) _ _) = pf/=pm && posicaoI xs m

-- | Verifica se o número de inimigos é pelo menos 2.
numI :: [Personagem] -> Bool    --numero de inimigos
numI l = length l >=2 

-- | A função iniVida confere se todos os fantasmas têm vida = 1
iniVida :: [Personagem] -> Bool
iniVida = all vida
  where
    vida :: Personagem -> Bool
    vida (Personagem {tipo = MacacoMalvado}) = True
    vida (Personagem {tipo = _, vida = v}) = v == 1


--| verificar se uma posição é do tipo Plataforma
ehPlataforma :: Bloco -> Bool
ehPlataforma Plataforma = True
ehPlataforma _          = False

--| verificar se uma posição é do tipo Alcapao
ehAlcapao :: Bloco -> Bool
ehAlcapao Alcapao = True
ehAlcapao _       = False

{-| Verifica se uma escada obedece às restrições, True indica que o mapa possui escadas válidas, e o valor False indica que não possui.-}
escadaValida :: Mapa -> Bool
escadaValida (Mapa _ _ matriz) = all verificaEscada todasEscadas
  where
    todasEscadas = concatMap (separarEscadas . filter (== Escada)) matriz
    --| divide uma lista de blocos em escadas
    separarEscadas :: [Bloco] -> [[Bloco]]
    separarEscadas [] = []
    separarEscadas lista =
      let (escada, resto) = span (== Escada) lista
      in escada : separarEscadas (dropWhile (== Escada) resto)
    --| verifica se a escada é válida
    verificaEscada escada =
      case escada of
        [] -> False
        (s : resto) ->
          (ehPlataforma s || ehPlataforma (last escada))
          && not (ehAlcapao s || ehAlcapao (last escada))

    --| Encontrar todas as escadas na matriz
    escadas =
      concatMap (filter (== Escada)) matriz



--| Verifica se a posição do jogador coincide com um bloco vazio.

checkmario :: Personagem -> Mapa -> Bool
checkmario mario mapa = 
  case blocopos (posicao mario) mapa of 
    Vazio -> True
    Escada -> True
    _ -> False

--| Verifica se a posição dos colecionáveis não coincide com nem uma plataforma, nem um alçapão.

checkcolec :: [(Colecionavel, Posicao)] -> Mapa -> Bool
checkcolec l mapa = notElem Plataforma pos &&  notElem Alcapao pos
  where pos = map (\(c, p) -> blocopos p mapa) l

--| provavelmente vai ser alterada
criarPersonagem :: Velocidade -> Entidade -> Posicao -> Direcao -> (Double, Double) -> Int -> Int -> (Bool, Double) -> [[Bloco]] -> Maybe Personagem
criarPersonagem vel ent pos dir tam vida pontos dano matrizBlocos =
  let tamanhoX = fst tam
      tamanhoY = snd tam
      --| Verifica se o bloco na posição inicial do personagem é Vazio
      blocoInicial = getBlocoNaPosicao pos matrizBlocos
  in if blocoInicial == Vazio &&
        all (\(x, y) -> getBlocoNaPosicao (x, y) matrizBlocos == Vazio) (posicoesBlocoPersonagem pos tam)
       then Just (Personagem vel ent pos dir tam False False vida pontos dano)
       else Nothing

--| talvez seja alterada
criarColecionavel :: Colecionavel -> Posicao -> [[Bloco]] -> Maybe (Colecionavel, Posicao)
criarColecionavel col pos matrizBlocos =
  let --| Verifica se o bloco na posição inicial do colecionável é Vazio
      blocoInicial = getBlocoNaPosicao pos matrizBlocos
  in if blocoInicial == Vazio
       then Just (col, pos)
       else Nothing

{-|verifica se algum bloco do mapa é um alçapão, e se o personagem cabe dentro deles-}
alcapaoL ::Mapa -> Personagem -> Bool
alcapaoL (Mapa _ _ l) Personagem {tamanho=(x,y)}= notElem Alcapao (concat l) || all ((>= (ceiling x)) . length) (filter (elem Alcapao) (concat (map group l)))
    



--| Função auxiliar para obter o bloco na posição dada na matriz
getBlocoNaPosicao :: Posicao -> [[Bloco]] -> Bloco
getBlocoNaPosicao (x, y) matrizBlocos =
  if 0 <= round y && round y < length matrizBlocos &&
     0 <= round x && round x < length (matrizBlocos !! round y)
    then matrizBlocos !! round y !! round x
    else Vazio

--| Função auxiliar para obter as posições ocupadas por um bloco do tamanho dado
posicoesBlocoPersonagem :: Posicao -> (Double, Double) -> [(Double, Double)]
posicoesBlocoPersonagem (x, y) (tamanhoX, tamanhoY) =
  [(x', y') | x' <- [x, x + tamanhoX - 1], y' <- [y, y + tamanhoY - 1]]



blocos1 = [ [ Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio]
          , [ Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio]
          , [ Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio]
          , [ Vazio, Plataforma, Plataforma, Plataforma, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma, Vazio]
          , [ Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
          , [ Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
          , [ Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]]


--| Função para gerar um mapa aleatório com mais plataformas
geraMapaAleatorio :: Semente -> [[Bloco]]
geraMapaAleatorio s = shuffleBlocos s blocos1

--| Função para embaralhar os blocos, mantendo algumas plataformas fixas
shuffleBlocos :: Semente -> [[Bloco]] -> [[Bloco]]
shuffleBlocos s blocos =
  let blocoFixo = Escada  -- Escolha um bloco fixo (por exemplo, Plataforma)
      matrizFixa = replicate 1 (replicate 10 blocoFixo)  --| Mantenha duas linhas fixas de plataformas que pode ser ajustado conforme necessário)
      (blocosRestantes, _) = shuffle (mkStdGen s) $ concat $ filter (\row -> head row /= blocoFixo) blocos  --| os blocos que não são fixos
      matrizEmbaralhada = matrizFixa ++ chunkList (length (head blocos)) blocosRestantes
  in matrizEmbaralhada


--| Função para embaralhar uma lista com base em uma semente
shuffle :: RandomGen g => g -> [a] -> ([a], g)
shuffle gen [] = ([], gen)
shuffle gen xs =
  let (idx, gen') = randomR (0, length xs - 1) gen
      (ys, z:zs) = splitAt idx xs
  in (z : ys ++ zs, gen')
               
--| Função para dividir uma lista em sub-listas de tamanho específico
chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs = take n xs : chunkList n (drop n xs)


{-

1. O mapa tem “chão”, i.e. uma plataforma que impede que o jogador            chao
ou outro personagem caia fora do mapa.

2. Todos os inimigos têm a propriedade ressalta a True, enquanto que       validaJogador, validaInimigo
o jogador a tem a False.

3. A posição inicial de um jogador não pode colidir com a posição inicial
de um outro personagem. Note que as posições iniciais de inimigos              posicaoI
podem colidir entre estes.

4. Número mı́nimo de inimigos: 2 (dois.)                                      numI

5. Inimigos Fantasma têm exactamente 1 (uma) vida                            iniVida

6. Escadas não podem começar/terminar em alçapões, e pelo menos uma        escadaValida
das suas extremidades tem que ser do tipo Plataforma.

7. Alçapões não podem ser menos largos que o jogador.                       alcapaoL

8. Não podem existir personagens nem coleccionáveis “dentro” de plata-
formas ou alçapões, i.e. o bloco (na matriz do mapa) correspendente
à posição de um personagem ou objecto tem que ser Vazio.
-}
