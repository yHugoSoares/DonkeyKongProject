module Main where

import Graphics.Gloss
    ( Picture(Text, Pictures, Color, Translate, Scale),
      Display(InWindow),
      black,
      blue,
      white,
      play,
      loadBMP )
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import DataStruct
import Keyboard
import Maps
import Tarefa1
import Physics

fr :: Int
fr = 50

dm :: Display
dm = FullScreen
--dm = InWindow "Donkey Kong" (640, 640) (0, 0)

type EstadoGloss = (Jogo, [Key], [Picture], Jogador, Inimigo)

estadoInicial :: Jogo
estadoInicial = Jogo (Opcoes Jogar) mapaGrande malvados [] mario

estadoGlossInicial :: [Picture] -> Jogador -> Inimigo -> EstadoGloss
estadoGlossInicial z skin inimigo = (estadoInicial,[], z, skin, inimigo)

reageEventoGloss :: Event -> EstadoGloss -> EstadoGloss
reageEventoGloss evento@(EventKey k Down _ _) (jogo@(Jogo (Opcoes _) m mal b jo) ,keys, z,skin, inimigo) = (stageMenu k jogo, reactKey evento keys, z,skin, inimigo)
                                                                                             where op = menu estadoInicial
reageEventoGloss evento (estadoInicial,keys, z,skin, inimigo) = (estadoInicial, reactKey evento keys, z,skin, inimigo)


-- atualizaEstado :: Float -> EstadoGloss -> EstadoGloss
-- atualizaEstado n (estadoInicial@(Jogo _ mapa _ _ mario), keys, z, skin, inimigo) = (aplicaListaKey keys estadoInicial b, keys, z, skin, inimigo)
--     where b = (colisoesChao mapa mario || colisoesParede mapa mario)

atualizaEstado :: Float -> EstadoGloss -> EstadoGloss
atualizaEstado n (estadoInicial@(Jogo _ mapa malvados _ mario), keys, z, skin, inimigo) =
  (aplicaListaKey keys estadoAtualizado colisao, keys, z, skin, inimigo)
    where
      estadoAtualizado = Jogo ModoJogo mapa malvados novoB marioAtualizado
      novoB = atualizaCollectibles n b  -- Assuming `atualizaCollectibles` updates the collectibles based on time
      colisao = colisoesChao mapa marioAtualizado || colisoesParede mapa marioAtualizado
      marioAtualizado = atualizaPersonagem n mario
      b = colecionaveis estadoInicial  -- Assuming `coletaveis` is a function that gets the current list of collectibles

-- Pega uma tecla pressionada e o estado do jogo e vai devolver o estado atualizado
stageMenu :: Key -> Jogo -> Jogo
stageMenu k jogo = case menu jogo of
                    (Opcoes op) ->
                        case k of
                                (SpecialKey KeyEnter) -> case op of
                                                            Jogar -> jogo {menu=ModoJogo}   --Faz com que o volte pro reageEventoGloss mas agora com a forma 'ModoJogo' entao o jogo segue normal
                                                            Sair -> error "Fim do jogo"
                                (SpecialKey KeyDown) -> jogo {menu = Opcoes (mudaOP op)}
                                (SpecialKey KeyUp)   -> jogo {menu = Opcoes (mudaOP op)}
                                _ -> jogo
                    _ -> jogo

--Vai mudar de 'Jogar' pra 'Sair'
mudaOP :: Opcao -> Opcao
mudaOP op = case op of
               Jogar -> Sair
               Sair -> Jogar

desenhaEstadoGloss :: EstadoGloss -> Picture
desenhaEstadoGloss ((Jogo (Opcoes op) _ _ _ _),_,_,_,_)  = drawOptions op
desenhaEstadoGloss (estadoinicio@(Jogo ModoJogo _ _ _ _),keys, z,skin, inimigo) = desenhaMapa estadoinicio z skin inimigo

drawOptions op =   case op of
    Jogar -> Pictures [Translate (-50) 10 $ Color blue $ drawOption "Jogar",
                       Translate (-50) (-70) $ Color white $ drawOption "Sair"]
    Sair ->  Pictures [Color white $ Translate (-50) 10 $ drawOption "Jogar",
                       Color blue $ Translate (-50) (-70) $ drawOption "Sair"]

drawOption option =  Scale (0.5) (0.5) $ Text option

main :: IO ()
main = do
    loadMAPA <- carregaImagens
    marioOeste <- loadBMP "./img/Mariooeste.bmp"
    marioLeste <- loadBMP "./img/Marioleste.bmp"
    macaco <- loadBMP "./img/macaco.bmp"
    fantasma <- loadBMP "./img/fantasma.bmp"
    play dm                             -- janela onde esta a decorrer
        (black)                         -- cor do fundo da janela
        fr                              -- framerate
        (estadoGlossInicial loadMAPA (Jogador,[marioOeste,marioLeste]) [(MacacoMalvado,macaco),(Fantasma, fantasma)])    -- estado inicial
        desenhaEstadoGloss              -- desenha o estado do jogo
        reageEventoGloss                -- reage a um evento
        atualizaEstado                  -- reage ao passar do tempo-}
