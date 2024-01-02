module Gloss where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Maps

type Estado = (Float, Float)

type EstadoGloss = (Estado, Picture)

estadoInicial :: Estado
estadoInicial = (0,0)

estadoGlossInicial :: Picture -> EstadoGloss 
estadoGlossInicial z = (estadoInicial, z)

reageEventoGloss :: Event -> EstadoGloss -> EstadoGloss 
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _)        ((x,y), z) = ((x,y+5), z)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _)      ((x,y), z) = ((x,y-5), z)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _)      ((x,y), z) = ((x-5,y), z)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _)     ((x,y), z) = ((x+5,y), z)
reageEventoGloss _ s = s -- ignora qualquer outro evento

reageTempoGloss :: Float -> EstadoGloss -> EstadoGloss 
reageTempoGloss n ((x,y), z) = ((x,y), z)

fr :: Int
fr = 60

dm :: Display
dm = FullScreen
-- dm - InWindow "Novo Jogo" (400, 400) (0, 0)

desenhaEstadoGloss :: EstadoGloss -> Picture 
desenhaEstadoGloss ((x,y), z) = Translate x y z

main :: IO ()
main = do
    mario <- loadBMP "/home/hugo/Documents/LEI 1ano/git/images/8Bit_Mario.bmp"
    play dm                             -- janela onde esta a decorrer
        black                           -- cor do fundo da janela
        fr                              -- framerate
        (estadoGlossInicial (scale 0.15 0.15 mario))      -- estado inicial
        desenhaEstadoGloss              -- desenha o estado do jogo
        reageEventoGloss                -- reage a um evento
        reageTempoGloss                 -- reage ao passar do tempo