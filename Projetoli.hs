module Projetoli where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import DataStruct
import qualified Data.Set as S
import Maps

mov :: Float
mov = 5  -- Velocidade do movimento do Mário
jump :: Float
jump = 40 -- Valor de salto do Mário

fr :: Int
fr = 50

dm :: Display
dm = InWindow "Donkey Kong" (640, 640) (0, 0)

type Estado = (Float, Float)
type EstadoGloss = (Estado, [Picture])
type Skins = [(Personagem,Picture)]


estadoInicial :: Estado
estadoInicial = (0, 0)  

estadoGlossInicial :: [Picture] -> EstadoGloss
estadoGlossInicial z = (estadoInicial, z)

reageEventoGloss :: Event -> EstadoGloss -> EstadoGloss
{-reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) ((x, y), z) = ((x, y + mov), z)
reageEventoGloss (EventKey (SpecialKey KeyUp) Up _ _) ((x, y), z) = ((x, y),z)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) ((x, y), z) = ((x, y - mov), z)
reageEventoGloss (EventKey (SpecialKey KeyDown) Up _ _) ((x, y), z) = ((x, y),z)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) ((x, y), z) = ((x - mov, y),z)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Up _ _) ((x, y), z) = ((x, y), z)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) ((x, y), z) = ((x + mov, y), z)
reageEventoGloss (EventKey (SpecialKey KeySpace) Down _ _) ((x, y), z) = ((x, y + jump), z)
reageEventoGloss (EventKey (SpecialKey KeySpace) Up _ _) ((x, y), z) = ((x, y - jump), z)-}
reageEventoGloss _ s = s -- Ignora qualquer outro evento


desenhaEstadoGloss :: EstadoGloss -> Picture
desenhaEstadoGloss ((x, y), z) = desenhaMapa z

carregaJogador :: IO[Picture]
carregaJogador = do 
                 mario <- loadBMP "/home/henrique/Code/img/Mario.bmp"
                 martelo <- loadBMP "/home/henrique/Code/img/Hammer.bmp"
                 moeda <- loadBMP "/home/henrique/Code/img/Hammer.bmp"
                 return [mario]

atualizaEstado :: Float -> EstadoGloss -> EstadoGloss
atualizaEstado _ ((x, y), z) = ((x, y), z)

main :: IO ()
main = do
    loadMAPA <- carregaImagens
    play dm                             -- janela onde esta a decorrer
        (black)                         -- cor do fundo da janela
        fr                              -- framerate
        (estadoGlossInicial loadMAPA)    -- estado inicial
        desenhaEstadoGloss              -- desenha o estado do jogo
        reageEventoGloss                -- reage a um evento
        atualizaEstado                  -- reage ao passar do tempo-}


{-data World = World
    { keys :: S.Set Key
    , counter :: Int }

handleInput :: Event -> World -> World
handleInput (EventKey k Down _ _) world = world { keys = S.insert k (keys world)}
handleInput (EventKey k Up _ _) world = world { keys = S.delete k (keys world)}
handleInput _ world = world -- Ignore non-keypresses for simplicity

update :: Float -> World -> World
update _ world
    | S.member (SpecialKey KeyUp) (keys world) = world { counter = 1 + counter world }
    | otherwise = world { counter = 0 }-}