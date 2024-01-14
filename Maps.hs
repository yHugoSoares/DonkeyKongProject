-- | Módulo contendo funções relacionadas ao desenho do mapa e personagens.
module Maps where

import DataStruct
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import Data.Maybe

-- | Tipo representando um jogador com sua aparência.
type Jogador = (Entidade,[Picture])

-- | Tipo representando um inimigo com sua aparência.
type Inimigo = [(Entidade,Picture)]

-- | Tipo representando uma lista de imagens.
type PictureM = [Picture]

-- | Função que desenha o jogador no ambiente.
desenhaJogador :: Personagem -> Jogador -> Picture
desenhaJogador (Personagem _ _ (i,j) d _ _ _ _ _ _ _) skin = desenhoJogador i j d skin

-- | Função que desenha o jogador no ambiente de acordo com sua aparência.
desenhoJogador :: Float -> Float -> Direcao -> Jogador-> Picture
desenhoJogador i j d (_,[marioO,marioL]) | d == Oeste = Translate i j  marioO
                                         | d == Leste = Translate i j  marioL
                                         |otherwise = Translate i j marioO

-- | Função que desenha os inimigos no ambiente.
desenhaInimigo :: [Personagem] -> Inimigo -> [Picture]
desenhaInimigo ((Personagem _ _ _ _ _ _ _ 0 _ _ _):t) skins        = desenhaInimigo t skins
desenhaInimigo ((Personagem _ tipo (i,j) _ _ _ _ _ _ _ _):t) skins = desenhoInimigo i j tipo skins:desenhaInimigo t skins
desenhaInimigo _ _                                               = []

-- | Função que desenha um inimigo no ambiente de acordo com sua aparência.
desenhoInimigo :: Float -> Float -> Entidade -> Inimigo -> Picture
desenhoInimigo i j tipo malvados = Translate i j image
        where image = (fromJust . lookup tipo) malvados

-- | Função que converte um bloco com sua aparência para uma imagem.
pieceToPic :: (Bloco,[Picture]) -> Picture
pieceToPic (Plataforma,pics) = pics !! 0
pieceToPic (Escada,pics) = pics !! 1
pieceToPic (Alcapao,pics) = pics !! 2
pieceToPic (Vazio,pics) = Blank

-- | Função que desenha uma linha do mapa.
drawLine :: ([Bloco],[Picture]) -> Int -> [Picture]
drawLine ([],_) _ = []
drawLine (h:t,pics) x = Translate (fromIntegral (x* round l)) 0 (pieceToPic (h,pics)):drawLine (t,pics) (x+1)

-- | Função que desenha o mapa.
drawMap :: (Mapa,[Picture]) -> (Int,Int) -> [Picture]
drawMap (Mapa _ _ [],_) _ = []
drawMap (Mapa a b (h:t),pics) (x,y) = Translate 0 (fromIntegral (y*(- round l))) (Pictures (drawLine (h,pics) x)):drawMap (Mapa a b t,pics) (x,y+1)

-- | Função que carrega as imagens necessárias para o jogo.
carregaImagens :: IO [Picture]
carregaImagens = do plataforma <- loadBMP "./img/Platform.bmp"
                    escada <- loadBMP "./img/Ladder.bmp"
                    alcapao <- loadBMP "./img/Ladder.bmp"
                    return [plataforma,escada,alcapao]

-- | Mapa utilizado no jogo.
mapaGrande :: Mapa
mapaGrande =  Mapa ((20,-200),Oeste) (0,0)
              [[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],
               [Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma],
               [Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma],
               [Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma],
               [Plataforma,Plataforma,Escada,Plataforma,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Plataforma,Escada,Plataforma,Plataforma],
               [Plataforma,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Plataforma],
               [Plataforma,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Plataforma],
               [Plataforma,Vazio,Escada,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Plataforma],
               [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Plataforma],
               [Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Plataforma,Escada,Plataforma,Plataforma,Plataforma,Plataforma],
               [Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Plataforma],
               [Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Plataforma],
               [Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Plataforma],
               [Plataforma,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Escada,Vazio,Vazio,Vazio,Plataforma],
               [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]]



desenhaMapa :: Jogo -> [Picture] -> Jogador -> Inimigo -> Picture
desenhaMapa (Jogo menu mapa mal colec jog) pics skin inimigo = translate (-120) 104 $ pictures (drawMap (mapa,pics) (0,0) ++ [desenhaJogador jog skin] ++ desenhaInimigo mal inimigo )


mario :: Personagem
mario = Personagem mov Jogador (40,-180) Oeste (12,12) False False 1 0 (False,0) 0

malvados :: [Personagem]
malvados = [Personagem 1 MacacoMalvado (100,26) Oeste (39,32) False True 1 0 (False,0) 0,Personagem 1 Fantasma (100,-204) Oeste (16,16) False True 1 0 (False,0) 0,Personagem 1 Fantasma (160,-204) Oeste (16,16) False True 1 0 (False,0) 0]
