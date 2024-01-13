module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import DataStruct
import Keyboard
import Maps
import Tarefa1

import Debug.Trace


fr :: Int
fr = 50

dm :: Display
dm = InWindow "Donkey Kong" (640, 640) (0, 0)

type EstadoGloss = (Jogo, [Key], [Picture], Jogador, Inimigo)

estadoInicial :: Jogo
estadoInicial = Jogo (Opcoes Jogar) mapaGrande malvados [] mario  

estadoGlossInicial :: [Picture] -> Jogador -> Inimigo -> EstadoGloss
estadoGlossInicial z skin inimigo = (estadoInicial,[], z, skin, inimigo)

reageEventoGloss :: Event -> EstadoGloss -> EstadoGloss
reageEventoGloss evento@(EventKey k Down _ _) (jogo@(Jogo (Opcoes Jogar) m mal b jo) ,keys, z,skin, inimigo) = (stageMenu k jogo, reactKey evento keys, z,skin, inimigo)
                                                                                             where op = menu estadoInicial
reageEventoGloss evento (estadoInicial,keys, z,skin, inimigo) = (estadoInicial, reactKey evento keys, z,skin, inimigo)


-- Pega uma tecla pressionada e o estado do jogo e vai devolver o estado atualizado
-- stageMenu :: Key -> Jogo -> Jogo
-- stageMenu k jogo = case k of 
--                                 (SpecialKey KeyEnter) -> case op of
--                                                             Jogar -> jogo {menu=ModoJogo}   --Faz com que o volte pro reageEventoGloss mas agora com a forma 'ModoJogo' entao o jogo segue normal
--                                                             Sair -> jogo {menu=ModoJogo}    --Isso aqui tá errado, era pra ser algum erro pra fechar o jogo
--                                 (SpecialKey KeyDown) -> jogo {menu = Opcoes (mudaOP op)}                           
--                                 (SpecialKey KeyUp)   -> jogo {menu = Opcoes (mudaOP op)}  
--                                 _ -> jogo  
--             where (Opcoes op) = menu jogo

stageMenu :: Key -> Jogo -> Jogo
stageMenu k w = case menu w of
  (Opcoes op) ->
    case k of
       (SpecialKey KeyEnter) -> case op of
                                 Jogar -> w {menu = ModoJogo}
                                 Sair -> error "Fim de Jogo"
       (SpecialKey KeyDown) -> trace "Pressed Down" $ w {menu = Opcoes (mudaOP op)}                          
       (SpecialKey KeyUp)   -> trace "Pressed Up" $ w {menu = Opcoes (mudaOP op)}   
       _                    -> w

--Vai mudar de 'Jogar' pra 'Sair', mas nao ta a funcionar nao sei pq, o jogo nao deixa vc voltar pra 'Jogar' quando fica em 'Sair'
mudaOP :: Opcao -> Opcao
mudaOP Jogar = Sair
mudaOP Sair = Jogar

desenhaEstadoGloss :: EstadoGloss -> Picture
desenhaEstadoGloss ((Jogo (Opcoes op) _ _ _ _),_,_,_,_)  = drawOptions op
desenhaEstadoGloss (estadoinicio@(Jogo ModoJogo _ _ _ _),keys, z,skin, inimigo) = desenhaMapa estadoinicio z skin inimigo

drawOptions op =   case op of
    Jogar -> Pictures [Translate (-50) 10 $ Color blue $ drawOption "Jogar",
                       Translate (-50) (-70) $ Color white $ drawOption "Sair"]
    Sair ->  Pictures [Color white $ Translate (-50) 10 $ drawOption "Jogar",
                       Color blue $ Translate (-50) (-70) $ drawOption "Sair"]

drawOption option =  Scale (0.5) (0.5) $ Text option


atualizaEstado :: Float -> EstadoGloss -> EstadoGloss
atualizaEstado n (estadoInicial, keys, z, skin, inimigo) = (aplicaListaKey keys estadoInicial, keys, z, skin, inimigo)


main :: IO ()
main = do
    loadMAPA <- carregaImagens
    marioOeste <- loadBMP "/home/hugo/Documents/LEI 1ano/git/img/Mariooeste.bmp"
    marioLeste <- loadBMP "/home/hugo/Documents/LEI 1ano/git/img/Marioleste.bmp"
    macaco <- loadBMP "/home/hugo/Documents/LEI 1ano/git/img/macaco.bmp"
    fantasma <- loadBMP "/home/hugo/Documents/LEI 1ano/git/img/fantasma.bmp"
    play dm                             -- janela onde esta a decorrer
        (black)                         -- cor do fundo da janela
        fr                              -- framerate
        (estadoGlossInicial loadMAPA (Jogador,[marioOeste,marioLeste]) [(MacacoMalvado,macaco),(Fantasma, fantasma)])    -- estado inicial
        desenhaEstadoGloss              -- desenha o estado do jogo
        reageEventoGloss                -- reage a um evento
        atualizaEstado                  -- reage ao passar do tempo-}
