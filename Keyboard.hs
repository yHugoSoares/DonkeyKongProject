module Keyboard where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import DataStruct 
import Maps 
import Data.List

reactKey :: Event -> [Key] -> [Key]
reactKey (EventKey key Up _ _)   = delete key
reactKey (EventKey key Down _ _) = (:) key
reactKey _                      = id

aplicaListaKey :: [Key] -> Jogo -> Jogo
aplicaListaKey [] j           = j
aplicaListaKey (h:t) e = aplicaListaKey t (aplicaKey h e)

aplicaKey :: Key -> Jogo -> Jogo
aplicaKey (SpecialKey KeyUp) j        = jogada Norte j
aplicaKey (SpecialKey KeyDown) j      = jogada Sul j
aplicaKey (SpecialKey KeyLeft) j      = jogada Oeste j
aplicaKey (SpecialKey KeyRight) j     = jogada Leste j
aplicaKey _ j = j


jogada ::Direcao -- A 'Jogada' a efetuar.
        -> Jogo -- O 'Estado' anterior.
        -> Jogo -- O 'Estado' resultante após o jogador efetuar a jogada.      
jogada d (Jogo menu m ini colec jo) = (Jogo menu m ini colec (playerNovo jo)) 
                                               where 
                                                     playerNovo jo = alteraJogada jo d (Jogo menu m ini colec jo)

alteraJogada :: Personagem -> Direcao -> Jogo -> Personagem
alteraJogada (Personagem velo a (x,y) dire c f g h i j) d e |d == Norte = (Personagem velo a (x,y+jump) dire c f g h i j)
                                                            |d == Sul   = (Personagem velo a (x,y-jump) dire c f g h i j)
                                                            |d == Oeste = (Personagem velo a (x-velo,y) Oeste c f g h i j)
                                                            |d == Leste = (Personagem velo a (x+velo,y) Leste c f g h i j)