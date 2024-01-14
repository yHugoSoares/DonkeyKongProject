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

type Itens = [(Colecionavel,Picture)]

-- | Tipo representando uma lista de imagens.
type PictureM = [Picture]

-- | Função que desenha o jogador no ambiente.
desenhaJogador :: Personagem -> Jogador -> Picture
desenhaJogador (Personagem _ _ (i,j) d _ _ _ _ _ _ _) skin = desenhoJogador i j d skin

-- | Função que desenha o jogador no ambiente de acordo com sua aparência.
desenhoJogador :: Float -> Float -> Direcao -> Jogador-> Picture
desenhoJogador i j d (_,[marioO,marioL]) | d == Oeste = scale 2 2 $ Translate i j  marioO
                                         | d == Leste = scale 2 2 $ Translate i j  marioL
                                         |otherwise = scale 2 2 $ Translate i j marioO

-- | Função que desenha os inimigos no ambiente.
desenhaInimigo :: [Personagem] -> Inimigo -> [Picture]
desenhaInimigo ((Personagem _ _ _ _ _ _ _ 0 _ _ _):t) skins        = desenhaInimigo t skins
desenhaInimigo ((Personagem _ tipo (i,j) _ _ _ _ _ _ _ _):t) skins = desenhoInimigo i j tipo skins:desenhaInimigo t skins
desenhaInimigo _ _                                               = []

-- | Função que desenha um inimigo no ambiente de acordo com sua aparência.
desenhoInimigo :: Float -> Float -> Entidade -> Inimigo -> Picture
desenhoInimigo i j tipo malvados = scale 2 2 $ Translate i j image
        where image = (fromJust . lookup tipo) malvados

-- | Função que desenha os colecionaveis no ambiente.
desenhaColecionavel :: [(Colecionavel, Posicao)] -> Itens -> [Picture]
desenhaColecionavel ((col,(x,y)):t) itens = desenhoColecionavel x y col itens : desenhaColecionavel t itens
desenhaColecionavel _ _ = []

-- | Função que desenha um colecionavel no ambiente de acordo com sua aparência.
desenhoColecionavel :: Float -> Float -> Colecionavel -> Itens -> Picture
desenhoColecionavel i j col itens = scale 3 3 $ Translate i j image
        where image = (fromJust . lookup col) itens

desenhaEstrela :: (Colecionavel,Posicao) -> Itens -> Picture
desenhaEstrela _ [] = Blank
desenhaEstrela (estrela,(x,y)) itens@((colec,pic):t) | estrela == colec = Translate x y pic  
                                                     |otherwise = desenhaEstrela (estrela,(x,y)) t


-- | Função que converte um bloco com sua aparência para uma imagem.
pieceToPic :: (Bloco,[Picture]) -> Picture
pieceToPic (Plataforma,pics) = scale 2 2 $ pics !! 0
pieceToPic (Escada,pics) = scale 2 2 $ pics !! 1
pieceToPic (Alcapao,pics) = scale 2 2 $ pics !! 2
pieceToPic (Vazio,pics) = scale 2 2 $ Blank

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
mapaGrande =  Mapa ((20,-200),Oeste) (0,0) mapa1
type Matriz a = [[a]]

mapa1 :: [[Bloco]]
mapa1 =   stringParaMapa ["-------------------------",
                          "-                       -",
                          "-                       -",
                          "-                       -",
                          "----|--------------------",
                          "-   |                   -",
                          "-   |                   -",
                          "-   |                   -",
                          "-   |                   -",
                          "--------------------|----",
                          "-                   |   -",
                          "-                   |   -",
                          "-                   |   -",
                          "-                   |   -",
                          "--|----------------------",
                          "- |                     -",
                          "- |                     -",           
                          "- |                     -",
                          "- |                     -",
                          "---------------------|---",
                          "-                    |  -",
                          "-                    |  -",
                          "-                    |  -",
                          "-                    |  -",
                          "-------------------------"
                          ]

stringParaMapa :: Matriz Char -> [[Bloco]]
stringParaMapa  =map  linha 
  where
    linha :: String -> [Bloco]
    linha "" = []
    linha (h:t)
      | h == '-' = Plataforma : linha t
      | h == ' ' = Vazio : linha t
      | h == '=' = Alcapao : linha t
      | h == '|' = Escada : linha t


desenhaMapa :: Jogo -> [Picture] -> Jogador -> Inimigo -> Itens -> Picture
desenhaMapa (Jogo menu mapa mal colec jog) pics skin inimigo itens = translate (-400) 368 $ pictures (drawMap (mapa,pics) (0,0) ++ [desenhaJogador jog skin] ++ desenhaInimigo mal inimigo ++ desenhaColecionavel colec itens ++ [desenhaEstrela estrela itens])


mario :: Personagem
mario = Personagem mov Jogador (90,-365) Leste (12,12) False False 1 0 (False,0) 0

malvados :: [Personagem]
malvados = [Personagem 1 MacacoMalvado (100,26) Oeste (39,32) False True 1 0 (True,1) 0,Personagem 1 Fantasma (100,-288) Oeste (16,16) False True 1 0 (True,1) 0,Personagem 1 Fantasma (250,-205) Oeste (16,16) False True 1 0 (True,1) 0]

estrela :: (Colecionavel,Posicao)
estrela = (Estrela,(650,-90))

colec :: [(Colecionavel, Posicao)]
colec = [(Martelo,(70,-190)),(Moeda, (100,-135))]