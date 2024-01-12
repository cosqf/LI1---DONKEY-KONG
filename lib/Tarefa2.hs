{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : Ivo Filipe Mendes Vieira <a103999@alunos.uminho.pt>
              Filipa Cosquete Santos <a106837@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where
import Tarefa1 (colisoesPersonagens)
import Funcoes
import LI12324
import Data.List (group)

{- | A função principal que verifica se o jogo é válido. Se tal não se verificar, o jogo crasha. 
Todas as funções que veremos de seguida são usadas por esta função.
-}

valida :: Jogo -> Bool
valida Jogo {mapa= m, inimigos= i, colecionaveis = c, jogador= j } = 
  chao m && validaJogador j && validaInimigo i && posicaoI i j m && numI i && iniVida i && alcapaoL m j && escadaValida m && checkmario j m  && checkcolec c m


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
posicaoI :: [Personagem] -> Personagem -> Mapa -> Bool
posicaoI [] _ _ = True
posicaoI (f:xs) mario m@(Mapa (pm,_) _ _) = not (colisoesPersonagens f mario {posicao= pm}) && posicaoI xs mario m

-- | Verifica se o número de inimigos é pelo menos 2.
numI :: [Personagem] -> Bool    --numero de inimigos
numI l = length l >=2 

-- | A função /iniVida/ confere se todos os fantasmas têm vida = 1
iniVida :: [Personagem] -> Bool
iniVida = all vida
  where
    vida :: Personagem -> Bool
    vida (Personagem {tipo = MacacoMalvado}) = True
    vida (Personagem {tipo = _, vida = v}) = v == 1


--verificar se uma posição é do tipo Plataforma
ehPlataforma :: Bloco -> Bool
ehPlataforma Plataforma = True
ehPlataforma _          = False

--verificar se uma posição é do tipo Alcapao
ehAlcapao :: Bloco -> Bool
ehAlcapao Alcapao = True
ehAlcapao _       = False

-- | Verifica se uma escada obedece às restrições.
escadaValida :: Mapa -> Bool
escadaValida (Mapa _ _ matriz) = all verificaEscada todasEscadas
  where
    todasEscadas = concatMap (separarEscadas . filter (== Escada)) matriz

    separarEscadas :: [Bloco] -> [[Bloco]]
    separarEscadas [] = []
    separarEscadas lista =
      let (escada, resto) = span (== Escada) lista
      in escada : separarEscadas (dropWhile (== Escada) resto)

    verificaEscada escada =
      case escada of
        [] -> False
        (s : resto) ->
          (ehPlataforma s || ehPlataforma (last escada))
          && not (ehAlcapao s || ehAlcapao (last escada))

    -- Encontrar todas as escadas na matriz
    escadas =
      concatMap (filter (== Escada)) matriz



-- | Verifica se a posição do jogador coincide com um bloco vazio.

checkmario :: Personagem -> Mapa -> Bool
checkmario mario mapa = 
  case blocopos (posicao mario) mapa of 
    Vazio -> True
    Escada -> True
    _ -> False

-- | Verifica se a posição dos colecionáveis não coincide com nem uma plataforma, nem um alçapão.

checkcolec :: [(Colecionavel, Posicao)] -> Mapa -> Bool
checkcolec l mapa = notElem Plataforma pos &&  notElem Alcapao pos
  where pos = map (\(c, p) -> blocopos p mapa) l

-- | Verifica se o jogador consegue cair num alçapão, tendo em conta o seu tamanho.

alcapaoL ::Mapa -> Personagem -> Bool
alcapaoL (Mapa _ _ l) Personagem {tamanho=(x,y)}= notElem Alcapao (concat l) || all ((>= (ceiling x)) . length) (filter (elem Alcapao) (concat (map group l)))
    --como os alcapoes têm tamanho 1x1, oq isto verifica é se o tamanho do mario cabe neles, ou seja, se o mario tiver tamanho 2x2, 


criarPersonagem :: Velocidade -> Entidade -> Posicao -> Direcao -> (Double, Double) -> Int -> Int -> (Bool, Double) -> [[Bloco]] -> Maybe Personagem
criarPersonagem vel ent pos dir tam vida pontos dano matrizBlocos =
  let tamanhoX = fst tam
      tamanhoY = snd tam
      -- Verifica se o bloco na posição inicial do personagem é Vazio
      blocoInicial = getBlocoNaPosicao pos matrizBlocos
  in if blocoInicial == Vazio &&
        all (\(x, y) -> getBlocoNaPosicao (x, y) matrizBlocos == Vazio) (posicoesBlocoPersonagem pos tam)
       then Just (Personagem vel ent pos dir tam False False vida pontos dano)
       else Nothing


criarColecionavel :: Colecionavel -> Posicao -> [[Bloco]] -> Maybe (Colecionavel, Posicao)
criarColecionavel col pos matrizBlocos =
  let -- Verifica se o bloco na posição inicial do colecionável é Vazio
      blocoInicial = getBlocoNaPosicao pos matrizBlocos
  in if blocoInicial == Vazio
       then Just (col, pos)
       else Nothing



-- Gera um mapa através da semente introduzida
{-
gerarMapaDonkeyKong :: Semente -> Int -> Int -> Mapa
gerarMapaDonkeyKong s largura altura numPlataformas = do
  let gen = mkStdGen s
  let matrizVazia = replicate altura (replicate largura Vazio)

  -- Gera as posições aleatórias para as plataformas
  let posicoesPlataformas = nub $ take numPlataformas $ geraPosicoesAleatorias gen largura altura

  -- Gera as posições aleatórias para as escadas (conectando plataformas na vertical)
  let posicoesEscadas =
        nub $
          take (numPlataformas - 1) $
            concatMap (\(x, y) -> [(x, y), (x, y + 1)]) posicoesPlataformas

  -- Gera as posições aleatórias para os alçapões
  let posicoesAlcapoes = take (numPlataformas `div` 2) $ geraPosicoesAleatorias gen largura altura

  -- Gera as posições aleatórias para os colecionáveis (moedas e martelos)
  let posicoesColecionaveis = take (numPlataformas * 2) $ geraPosicoesAleatorias gen largura altura

  -- Gera as posições aleatórias para o jogador e a estrela
  let posicaoJogador = head $ geraPosicoesAleatorias gen largura altura
  let posicaoEstrela = last $ geraPosicoesAleatorias gen largura altura

  -- Verifica se existe pelo menos um caminho para o jogador chegar à estrela
  let caminhoExiste = existeCaminho matrizVazia posicaoJogador posicaoEstrela

  -- Se não houver caminho, gera novamente as posições do jogador e da estrela
  let (posicaoJogadorFinal, posicaoEstrelaFinal) =
        if caminhoExiste
          then (posicaoJogador, posicaoEstrela)
          else (head $ geraPosicoesAleatorias gen largura altura, last $ geraPosicoesAleatorias gen largura altura)

  -- Preenche a matriz com os elementos nas posições geradas
  let matrizFinal =
        foldl (\m (pos, elemento) -> atualizaMatriz m pos elemento) matrizVazia $
          zip posicoesPlataformas (repeat Plataforma)
            ++ zip posicoesEscadas (repeat Escada)
            ++ zip posicoesAlcapoes (repeat Alcapao)
            ++ zip posicoesColecionaveis (cycle [Moeda, Martelo])
            ++ [(posicaoJogadorFinal, Jogador), (posicaoEstrelaFinal, Estrela)]

  let mapaFinal = Mapa ((0, 0), Este) posicaoEstrelaFinal

  -- Verifica se o mapa atende a todas as restrições
  if chao mapaFinal
        && validaJogador (jogador mapaFinal)
        && validaInimigo (inimigos mapaFinal)
        && posicaoI (inimigos mapaFinal) mapaFinal
        && numI (inimigos mapaFinal)
        && iniVida (inimigos mapaFinal)
        && escadaValida matrizFinal
        then mapaFinal
        else gerarMapaDonkeyKong s largura altura numPlataformas
-}


-- Função auxiliar para obter o bloco na posição dada na matriz
getBlocoNaPosicao :: Posicao -> [[Bloco]] -> Bloco
getBlocoNaPosicao (x, y) matrizBlocos =
  if 0 <= round y && round y < length matrizBlocos &&
     0 <= round x && round x < length (matrizBlocos !! round y)
    then matrizBlocos !! round y !! round x
    else Vazio

-- Função auxiliar para obter as posições ocupadas por um bloco do tamanho dado
posicoesBlocoPersonagem :: Posicao -> (Double, Double) -> [(Double, Double)]
posicoesBlocoPersonagem (x, y) (tamanhoX, tamanhoY) =
  [(x', y') | x' <- [x, x + tamanhoX - 1], y' <- [y, y + tamanhoY - 1]]




{-
chao :: Mapa -> Mapa
chao (Mapa x y m) = Mapa x y (init m ++ [replicate (length (last m)) Plataforma]) --oq isto faz é subsituir a ultima linha da matriz do mapa por uma q tem apenas plataformas

exemplo de mapa
Mapa ((0.5, 5.5), Oeste) (0.5, 2.5) [[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],[Plataforma, Plataforma, Vazio, Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]]

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