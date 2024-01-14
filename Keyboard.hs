module Keyboard where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import DataStruct 
import Maps 
import Data.List
import Tarefa1

reactKey :: Event -> [Key] -> [Key]
reactKey (EventKey key Up _ _)   = delete key
reactKey (EventKey key Down _ _) = (:) key
reactKey _                      = id

aplicaListaKey :: [Key] -> Jogo -> Jogo
aplicaListaKey [] j          = j
aplicaListaKey (h:t) e = aplicaListaKey t (aplicaKey h e) 

aplicaKey :: Key -> Jogo -> Jogo             
aplicaKey (SpecialKey KeyUp) j@(Jogo _ _ _ _ p)        = if colisoesParede (mapa j) p Norte then j else jogada Norte j
aplicaKey (SpecialKey KeyDown) j@(Jogo _ _ _ _ p)      = if colisoesParede (mapa j) p Sul then j else jogada Sul j
aplicaKey (SpecialKey KeyLeft) j@(Jogo _ _ _ _ p)      = if colisoesParede (mapa j) p Oeste then j else jogada Oeste j
aplicaKey (SpecialKey KeyRight) j@(Jogo _ _ _ _ p)     = if colisoesParede (mapa j) p Leste then j else jogada Leste j
aplicaKey _ j                         = j

{-aplicaKey k@(SpecialKey KeyUp) j _       = jogada Norte j
aplicaKey k@(SpecialKey KeyDown) j _       = jogada Sul j
aplicaKey k@(SpecialKey KeyLeft) j _       = jogada Oeste j
aplicaKey k@(SpecialKey KeyRight) j _      = jogada Leste j
aplicaKey _ j _ = j-}


plataformaNaFrente :: Mapa -> Posicao -> Direcao -> Bool
plataformaNaFrente (Mapa _ _ mapa) (x, y) direcao =
  case direcao of
    Oeste -> isPlataforma (round ((x-16)/16),round (y/16))
    Leste -> isPlataforma (round ((x+16)/16),round (y/16))
    Norte -> isPlataforma (round (x/16),round ((y+16)/16))
    Sul   -> isPlataforma (round (x/16),round ((y-16)/16))
  where
    isPlataforma (col, row) =
      case obterValor mapa (col, row) of
        Plataforma -> True
        _          -> False                         

{-aplicaColisao :: Key -> Jogo -> Jogo
aplicaColisao k j@(Jogo _ _ _ _ jog@(Personagem {posicao = (x,y)})) = 
                case k of 
                    (SpecialKey KeyUp) -> j-}


jogada ::Direcao -- A 'Jogada' a efetuar.
        -> Jogo -- O 'Estado' anterior.
        -> Jogo -- O 'Estado' resultante após o jogador efetuar a jogada.      
jogada d (Jogo menu m ini colec jo) = Jogo menu m ini colec (playerNovo jo)
                                               where 
                                                     playerNovo jo = alteraJogada jo d (Jogo menu m ini colec jo)

alteraJogada :: Personagem -> Direcao -> Jogo -> Personagem
alteraJogada p@(Personagem {posicao = (x,y), velocidade = velo}) d e |d == Norte     = p {posicao =(x,y+jump)}
                                                                     |d == Sul       = p {posicao =(x,y-jump)}
                                                                     |d == Oeste     = p {posicao =(x-velo,y)}
                                                                     |d == Leste     = p {posicao =(x+velo,y)}