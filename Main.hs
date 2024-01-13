module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import DataStruct
import Keyboard
import Maps
import Tarefa1


fr :: Int
fr = 50

dm :: Display
dm = InWindow "Donkey Kong" (640, 640) (0, 0)

type EstadoGloss = (Jogo, [Key], [Picture], Jogador, Inimigo)

estadoInicial :: Jogo
estadoInicial = Jogo mapaGrande malvados [] mario  

estadoGlossInicial :: [Picture] -> Jogador -> Inimigo -> EstadoGloss
estadoGlossInicial z skin inimigo = (estadoInicial,[], z, skin, inimigo)

reageEventoGloss :: Event -> EstadoGloss -> EstadoGloss
reageEventoGloss evento (estadoInicial,keys, z,skin, inimigo) = (estadoInicial, reactKey evento keys, z,skin, inimigo)


desenhaEstadoGloss :: EstadoGloss -> Picture
desenhaEstadoGloss (estadoInicial,keys, z,skin, inimigo) = desenhaMapa estadoInicial z skin inimigo

atualizaEstado :: Float -> EstadoGloss -> EstadoGloss
atualizaEstado n (estadoInicial, keys, z, skin, inimigo) = (aplicaListaKey keys estadoInicial, keys, z, skin, inimigo)

carregaJogador :: IO[Picture]
carregaJogador = do 
                 mario <- loadBMP "/home/henrique/Code/img/Mario.bmp"
                 martelo <- loadBMP "/home/henrique/Code/img/Hammer.bmp"
                 moeda <- loadBMP "/home/henrique/Code/img/Hammer.bmp"
                 return [mario]

main :: IO ()
main = do
    loadMAPA <- carregaImagens
    marioOeste <- loadBMP "/home/henrique/Code/img/Mariooeste.bmp"
    marioLeste <- loadBMP "/home/henrique/Code/img/Marioleste.bmp"
    macaco <- loadBMP "/home/henrique/Code/img/Mariooeste.bmp"
    fantasma1 <- loadBMP "/home/henrique/Code/img/Marioleste.bmp"
    play dm                             -- janela onde esta a decorrer
        (black)                         -- cor do fundo da janela
        fr                              -- framerate
        (estadoGlossInicial loadMAPA (Jogador,[marioOeste,marioLeste]) [(MacacoMalvado,macaco),(Fantasma, fantasma1)])    -- estado inicial
        desenhaEstadoGloss              -- desenha o estado do jogo
        reageEventoGloss                -- reage a um evento
        atualizaEstado                  -- reage ao passar do tempo-}
