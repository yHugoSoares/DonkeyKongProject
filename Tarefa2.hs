module Tarefa2 where

import Maps 
import DataStruct 
import Main
import Tarefa1 
import Keyboard 


valida :: Jogo -> Bool
valida (Jogo _ mapa inimigos _ jog) = verificaChao mapa && verificaRessalta inimigos jog && verificaPosicao inimigos jog && numeroInimigos inimigos && vidaFantasma inimigos && verificaMapa mapa && verificaLargura jog l


verificaChao :: Mapa -> Bool
verificaChao (Mapa _ _ mapa) = all (==Plataforma) (last mapa)

verificaRessalta :: [Personagem] -> Personagem ->  Bool
verificaRessalta ((Personagem _ _ _ _ _ _ ressini _ _ _ _):t) p@(Personagem _ _ _ _ _ _ ressjog _ _ _ _)= ressini && ressjog && verificaRessalta t p
verificaRessalta [] _ = True

verificaPosicao :: [Personagem] -> Personagem -> Bool
verificaPosicao ((Personagem _ _ (x1,y1) _ (a1,b1) _ _ _ _ _ _):t) p@(Personagem _ _ (x2,y2) _ (a2,b2) _ _ _ _ _ _) = 
        not (x1 + (a1/2) < x2 - (a2/2)|| x2 + (a2/2) < x1 - (a1/2) || y1 + (b1/2) < y2 - (b2/2)|| y2 + (b2/2) < y1 - (a1/2)) && verificaPosicao t p
verficaPosicao [] _ = True

numeroInimigos :: [Personagem] -> Bool
numeroInimigos ini = length ini == 3

vidaFantasma :: [Personagem] -> Bool
vidaFantasma ((Personagem _ enti _ _ _ _ _ v _ _ _):t) |enti == Fantasma = v == 1 && vidaFantasma t
                                                     |otherwise = vidaFantasma t
vidaFantasma [] = True

blocoabaixo :: [[Bloco]] -> (Int,Int) -> Bloco
blocoabaixo mapa (x,y) = obterValor mapa (x,y+1)

blocoacima :: [[Bloco]] -> (Int,Int) -> Bloco
blocoacima mapa (x,y) = obterValor mapa (x,y-1)

verificaEscada :: Mapa -> (Int,Int) -> Bool
verificaEscada map@(Mapa _ _ mapa) (x,y) = 
                    case obterValor mapa (x,y) of 
                            Escada -> 
                                case (blocoacima mapa (x,y), blocoabaixo mapa (x,y)) of
                                    (Plataforma,_) -> True
                                    (_,Plataforma) -> True
                                    (Vazio,Vazio) -> False
                                    (Alcapao,_) -> False
                                    (_,Alcapao) -> False
                                    (Escada,Vazio) -> verificaEscada map (x,y+1)
                                    (Vazio,Escada) -> verificaEscada map (x ,y-1)
                                    (Escada,Escada) -> if verificaEscada map (x,y+1) == True then True
                                                            else if verificaEscada map (x,(y-1)) 
                                                                then True
                                                                else False

                            _ -> True 

verificaMapa :: Mapa -> Bool
verificaMapa map@(Mapa _ _ mapa) = all (\coord -> verificaEscada map coord) coordenadas
        where
            coordenadas = [(x,y) | x <- [0 .. length mapa -1], y <- [0 .. length (head mapa) - 1]]

verificaLargura :: Personagem -> (Float,Float) -> Bool
verificaLargura (Personagem _ _ _ _ (a,l) _ _ _ _ _ _) (a2,l2) = l < l2

--verificaDentro :: 