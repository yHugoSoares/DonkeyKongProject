{-|
Module : Tarefa 3
Description : Modulo Haskell Gloss Tarefa3
Copyright : Hugo Soares (a107293)
            Henrique Brito (a107378)
Movimentaçãao de personagens
O objectivo desta tarefa é implementar a função
movimenta :: Semente -> Tempo -> Jogo -> Jogo
A função movimenta deve animar todos os personagens, i.e. calcular as
suas novas posições e respectivas consequências. 
-}

module Tarefa3 where

import DataStruct
import Tarefa1
import Maps

-- | Função principal que movimenta os personagens no jogo.
movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta semente tempo jogo = 
    jogoColide
      where moveInimigo = map (movimentaInimigo semente tempo) (inimigos jogo)
            jogoGravidade = aplicaGravidade jogo {inimigos = moveInimigo}
            pegarColec = pegarcolecionaveis jogoGravidade
            jogoColide = bater pegarColec

-- | Função que lida com as colisões entre personagens e colecionáveis.
bater :: Jogo -> Jogo
bater jogo@Jogo {jogador = mario@Personagem {aplicaDano = (danoX, _), vida = v}, inimigos = (ini:resto)} = 
          case (colisaohitbox mario ini, colisoesPersonagem mario ini) of
            (True, _) -> bater jogo {inimigos = resto ++ [ini {vida = 0, aplicaDano = (False, 0)}]}
            (_, True) -> jogo {jogador = mario {vida = v-1}}
            _ -> jogo

-- | Verifica colisões entre as hitboxes dos personagens.
colisaohitbox :: Personagem -> Personagem -> Bool
colisaohitbox p1@Personagem {tipo = Jogador, aplicaDano = (True, g), direcao = dir, posicao = (x, y), tamanho = (larg, alt)}
      p2@Personagem {posicao = (x1, y1), tamanho = t} = 
          case dir of 
            Leste -> colisoesPersonagem p1{posicao = (x+larg, y)} p2
            Oeste -> colisoesPersonagem p1{posicao = (x-larg, y)} p2
            _ -> False
colisaohitbox _ _ = False

-- | Aplica a gravidade ao jogador.
aplicaGravidade :: Jogo -> Jogo
aplicaGravidade jogo@(Jogo menu mapa mal colec jog) =
    jogo { jogador = aplicarGravidadeAoPersonagem mapa jog }

-- | Aplica a gravidade a um personagem.
aplicarGravidadeAoPersonagem :: Mapa -> Personagem -> Personagem
aplicarGravidadeAoPersonagem mapa p@(Personagem vel tipo (x, y) dir tam emEsc res vida pontos dano velVert) =
    p { posicao = (x, y + velVert), velocidadeVertical = novaVelocidadeVert }
  where
    novaVelocidadeVert = if colisoesParede mapa p Sul then 0 else velVert + gravidade

-- | Atualiza o jogo ao pegar colecionáveis.
pegarcolecionaveis :: Jogo -> Jogo
pegarcolecionaveis jogo@Jogo {colecionaveis = colec, jogador = mario} =
      if checaProximo jogo then
        let (jogoAtualizado, personagemAtualizado) = atualizarEstadoJogador jogo
        in jogoAtualizado {jogador = personagemAtualizado, colecionaveis = filter (naocolide personagemAtualizado) colec}
      else jogo
    where naocolide Personagem {posicao = (x, y), tamanho = (larg, alt)} (_, pItem) = 
              procuraMatriz l pItem /= procuraMatriz l (x-(larg/2), y) &&
              procuraMatriz l pItem /= procuraMatriz l (x+(larg/2),y) 

-- | Procura a posição de um ponto na matriz do mapa.
procuraMatriz :: TBloco -> Posicao -> (Int, Int)
procuraMatriz l (x, y) = (round (x/16), round (y/16))

-- | Verifica se o jogador está próximo a algum colecionável.
checaProximo :: Jogo -> Bool
checaProximo Jogo {jogador = Personagem {posicao = (x, y), aplicaDano = (d, tempo), tamanho = (larg, alt)}, colecionaveis = colec} =
      any (\(_, pItem) -> procuraMatriz l pItem == procuraMatriz l (x-(larg/2), y) || procuraMatriz l pItem == procuraMatriz l (x+(larg/2),y)) colec

-- | Atualiza o estado do jogador ao pegar um colecionável.
atualizarEstadoJogador :: Jogo -> (Jogo, Personagem)
atualizarEstadoJogador jogo@Jogo {colecionaveis = ((colec, pItem):t), jogador = p@(Personagem {tipo = Jogador, pontos = ponto, posicao = posicaoPersonagem, tamanho = (larg, alt)})} = 
    (jogoAtualizado, personagemAtualizado)
    where (jogoAtualizado, personagemAtualizado) = case colec of 
            Moeda -> (jogo { colecionaveis = t }, p {pontos = ponto+200})
            Martelo -> (jogo { colecionaveis = t, jogador = pAtualizado}, pAtualizado)
              where pAtualizado =  p { aplicaDano = (True, 10), pontos = ponto}

-- | Movimenta os inimigos do jogo.
movimentaInimigo :: Semente -> Tempo -> Personagem -> Personagem
movimentaInimigo semente tempo ini@Personagem {direcao = dire, velocidade = v}= 
            case dire of
              Leste -> if colisoesParede mapaGrande ini Leste then moverInimigo ini {direcao = Oeste , velocidade = -v}
                                                              else moverInimigo ini
              Oeste -> if colisoesParede mapaGrande ini Oeste then moverInimigo ini {direcao = Leste, velocidade = -v}
                                                              else moverInimigo ini

-- | Move um inimigo na direção especificada.
moverInimigo :: Personagem -> Personagem 
moverInimigo ini@Personagem {velocidade = v, posicao = (x,y)} = ini {posicao = (x-v,y)}