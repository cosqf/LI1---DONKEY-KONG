module Reage where
import LI12324
import Tarefa5 
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Tarefa4 (atualiza)
import Funcoes

reageEvento :: Event -> Estado -> IO Estado
reageEvento evento estado@Estado {modo= modo} = case modo of
  MenuInicial _ -> menureage evento estado -- Passar o estado corrente para o reageMenu
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
menureage (EventKey _ Down _ _) e@Estado {modo = MenuInicial Menu} = return e {modo= MenuInicial Jogar}
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
jogoreage event e@Estado {modo = EmJogo} = do 
    let 
        acao = marioMovTeclas event (jogo e)
    putStrLn (show (jogo e))
    return $ e {jogo= atualiza [] acao (jogo e)}
jogoreage _ e = return e




reageMensagem :: Event -> MensagemOp -> Estado -> IO Estado
reageMensagem (EventKey (SpecialKey KeyEnter) Down _ _) _ e@Estado {modo = modo}=
  return (e {modo= MenuInicial Jogar}) -- Retorne ao menu após pressionar Enter
reageMensagem _ estado e@Estado {modo = modo} = return (e{modo=Mensagem estado}) -- Mantenha o estado atual se outros eventos ocorrerem



marioMovTeclas :: Event -> Jogo -> Maybe Acao
marioMovTeclas (EventKey (SpecialKey KeyUp) Down _ _) (Jogo {mapa= mapa,  jogador= p})
  |emEscada p = Just Subir 
  |emEscada p && blocopos (posicao p) mapa == Plataforma = Just Subir
  |blocopos (posicao p) mapa == Escada && not (fst (aplicaDano p)) = Just Subir
  |otherwise= Just Parar 
marioMovTeclas (EventKey (SpecialKey KeyDown) Down _ _) (Jogo {mapa= mapa, jogador= p@(Personagem {posicao= (x,y)})})
  |emEscada p && blocodirecao p Sul mapa/= Plataforma = Just Descer 
  |emEscada p && blocopos (x,y) mapa == Plataforma = Just Descer
  |blocopos (x,y+2) mapa == Escada && not (fst (aplicaDano p)) = Just Descer
  |otherwise= Just Parar 
marioMovTeclas (EventKey (SpecialKey KeyLeft) Down _ _) (Jogo {mapa= mapa,  jogador= p})
  |emEscada p && blocodirecao p Sul mapa== Plataforma = Just AndarEsquerda
  |not (emEscada p) = Just AndarEsquerda
  |otherwise= Just Parar 
marioMovTeclas (EventKey (SpecialKey KeyRight) Down _ _) (Jogo {mapa= mapa,  jogador= p})
  |emEscada p && blocodirecao p Sul mapa== Plataforma = Just AndarDireita
  |not (emEscada p) = Just AndarDireita
  |otherwise= Just Parar 
marioMovTeclas (EventKey (SpecialKey KeySpace) Down _ _) (Jogo {mapa= mapa,  jogador= p})
  |emEscada p || fst (aplicaDano p) = Just Parar
  |otherwise = Just Saltar
marioMovTeclas _ _ = Just Parar

