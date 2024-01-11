module Maps where

import DataStruct
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Juicy
import Data.Maybe
import DataStruct (Entidade(MacacoMalvado))

plataforma :: Picture
plataforma = (Polygon [(0,0),(40,0),(40,40),(0,40)])

alcapao :: Picture
alcapao= translate 20 20 $ color green (ThickCircle 0 20)

escada :: Picture
escada = color red (Polygon [(0,0),(40,0),(40,40),(0,40)])

pieceToPic :: (Bloco,[Picture]) -> Picture
pieceToPic (Plataforma,pics) = pics !! 0
pieceToPic (Escada,pics) = pics !! 1
pieceToPic (Alcapao,pics) = pics !! 2
pieceToPic (Vazio,pics) = Blank    

drawLine :: ([Bloco],[Picture]) -> Int -> [Picture]
drawLine ([],_) _ = []
drawLine ((h:t),pics) x = (Translate (fromIntegral(x*16)) 0 (pieceToPic (h,pics))):(drawLine (t,pics) (x+1))

drawMap :: (Mapa,[Picture]) -> (Int,Int) -> [Picture]
drawMap (Mapa _ _ [],_) _ = []
drawMap (Mapa a b (h:t),pics) (x,y) = (Translate 0 (fromIntegral(y*(-16))) (Pictures (drawLine (h,pics) x))):(drawMap (Mapa a b t,pics) (x,y+1))

carregaImagens :: IO [Picture]
carregaImagens = do plataforma <- loadBMP "/home/henrique/Code/img/Platform.bmp"
                    escada <- loadBMP "/home/henrique/Code/img/Ladder.bmp"
                    alcapao <- loadBMP "/home/henrique/Code/img/Ladder.bmp"
                    jogador <- loadBMP "/home/henrique/Code/img/Mario.bmp"
                    macaco <- loadBMP "/home/henrique/Code/img/Mario.bmp"
                    fantasma <- loadBMP "/home/henrique/Code/img/Mario.bmp"
                    return ([plataforma,escada,alcapao,jogador,macaco,fantasma])  

players :: (Entidade, [Picture]) -> Picture
players (Jogador,pics) = pics !! 3
players (MacacoMalvado,pics) = pics !! 4
players (Fantasma,pics) = pics !! 5



displayMode :: Display
displayMode = InWindow "Game" (640,640) (0,0)


{-main :: IO()
main = do loadIMG <- carregaImagens
          display displayMode black (desenhaMapa loadIMG)-}

mapaGrande :: Mapa
mapaGrande =  Mapa ((0,0),Oeste) (0,0)
              [[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],
               [Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma],
               [Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma],
               [Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma],
               [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],
               [Plataforma,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Plataforma],
               [Plataforma,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Plataforma],
               [Plataforma,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Plataforma],
               [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Plataforma],
               [Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],
               [Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Plataforma],
               [Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Plataforma],
               [Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Plataforma],
               [Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Plataforma],
               [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]]

desenhaMapa :: [Picture] -> Picture
desenhaMapa pics = translate (-120) (104) $ pictures (drawMap (mapaGrande,pics) (0,0) ++ [])

--Mapas utilizados para teste

{-mapaPequeno :: Mapa2
mapaPequeno = [[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],
               [Plataforma,Escada,Plataforma,Alcapao,Plataforma],
               [Plataforma,Alcapao,Plataforma,Escada,Plataforma],
               [Plataforma,Escada,Plataforma,Alcapao,Plataforma],
               [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]]


desenhaPequeno :: Picture 
desenhaPequeno = translate (-100) (60) $ pictures (drawMap mapaPequeno (0,0))-}
