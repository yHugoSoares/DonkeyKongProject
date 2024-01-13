module Maps where

import DataStruct
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import Data.Maybe

type Jogador = (Entidade,[Picture])
type Inimigo = [(Entidade,Picture)]
type PictureM = [Picture]

desenhaJogador :: Personagem -> Jogador -> Picture
desenhaJogador (Personagem _ _ (i,j) d _ _ _ _ _ _) = desenhoJogador i j d

desenhoJogador :: Float -> Float -> Direcao -> Jogador-> Picture
desenhoJogador i j d (_,[marioO,marioL]) | d == Oeste = Translate i j  marioO
                                         | d == Leste = Translate i j  marioL

desenhaInimigo :: [Personagem] -> Inimigo -> [Picture]
desenhaInimigo ((Personagem _ _ _ _ _ _ _ 0 _ _):t) skins        = desenhaInimigo t skins
desenhaInimigo ((Personagem _ tipo (i,j) _ _ _ _ _ _ _):t) skins = desenhoInimigo i j tipo skins:desenhaInimigo t skins
desenhaInimigo _ _                                               = []

desenhoInimigo :: Float -> Float -> Entidade -> Inimigo -> Picture
desenhoInimigo i j tipo malvados = Translate i j image
        where image = (fromJust . lookup tipo) malvados


pieceToPic :: (Bloco,[Picture]) -> Picture
pieceToPic (Plataforma,pics) = scale 2 2 $ pics !! 0
pieceToPic (Escada,pics) = scale 2 2 $ pics !! 1
pieceToPic (Alcapao,pics) = scale 2 2 $ pics !! 2
pieceToPic (Vazio,pics) = scale 2 2 $ Blank

drawLine :: ([Bloco],[Picture]) -> Int -> [Picture]
drawLine ([],_) _ = []
drawLine (h:t,pics) x = Translate (fromIntegral (x*16)) 0 (pieceToPic (h,pics)) : drawLine (t,pics) (x+1)

drawMap :: (Mapa,[Picture]) -> (Int,Int) -> [Picture]
drawMap (Mapa _ _ [],_) _ = []
drawMap (Mapa a b (h:t),pics) (x,y) = Translate 0 (fromIntegral (y*(-16))) (Pictures (drawLine (h,pics) x)) : drawMap (Mapa a b t,pics) (x,y+1)

carregaImagens :: IO [Picture]
carregaImagens = do plataforma <- loadBMP "./img/Platform.bmp"
                    escada <- loadBMP "./img/Ladder.bmp"
                    alcapao <- loadBMP "./img/Ladder.bmp"
                    return [plataforma,escada,alcapao]

displayMode :: Display
displayMode = InWindow "Game" (640,640) (0,0)



mapaGrande :: Mapa
mapaGrande =  Mapa ((20,-200),Oeste) (0,0)
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

desenhaMapa :: Jogo -> [Picture] -> Jogador -> Inimigo -> Picture
desenhaMapa (Jogo menu mapa mal colec jog) pics skin inimigo = translate (-120) (104) $ pictures (drawMap (mapa,pics) (0,0) ++ [desenhaJogador jog skin] ++ desenhaInimigo mal inimigo )


mario :: Personagem
mario = Personagem mov Jogador (20,-204) Oeste (16,16) False False 1 0 (False,0)

malvados :: [Personagem]
malvados = [Personagem 1 MacacoMalvado (100,26) Oeste (39,32) False False 1 0 (False,0),Personagem 1 Fantasma (100, -204) Oeste (16,16) False False 1 0 (False,0)]
