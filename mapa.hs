module Maps where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import DataStruct

jogoInicial :: Jogo
jogoInicial =  Jogo (stringParaMapa      ["PPPPPPPPPP",
                                          "VVVVVVVVVV",
                                          "VVVVVVVVVV",
                                          "PPPVVVVPPP",
                                          "VVVVVVVVVV",
                                          "VVVVVVVVVV",
                                          "PPPPPPPPPP"])

stringParaMapa :: [String] -> Mapa2
stringParaMapa n = map (linha) n
        where 
        linha :: String -> [Bloco]
        linha "" = []
        linha (h:t) |h == ' ' = Vazio : (linha t)
                    |h == 'P' = Plataforma : (linha t)
                    |h == 'A' = Alcapao : (linha t)
                    |h == 'E' = Escada : (linha t)