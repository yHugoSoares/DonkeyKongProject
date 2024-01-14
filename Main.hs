{-|
Module : Main
Description : Modulo Haskell Gloss Main
Copyright : Hugo Soares (a107293)
            Henrique Brito (a107378)
Este modulo usa a biblioteca Gloss de
maneira a fazer um jogo do Donkey Kong.
-}
module Main where

import Graphics.Gloss
    ( Picture(Text, Pictures, Color, Translate, Scale),
      Display(InWindow),
      black,
      blue,
      white,
      play,
      loadBMP )
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import DataStruct
import Tarefa4
import Maps
import Tarefa1
import Tarefa3

-- | Taxa de quadros por segundo do jogo.
fr :: Int
fr = 50

-- | Configurações de exibição para a janela do jogo.
dm :: Display
dm = FullScreen
--dm = InWindow "Donkey Kong" (640, 640) (0, 0)

-- | Alias de tipo para o estado do jogo.
type EstadoGloss = (Jogo, [Key], [Picture], Jogador, Inimigo, Itens)

-- | Estado inicial do jogo.
estadoInicial :: Jogo
estadoInicial = Jogo (Opcoes Jogar) mapaGrande malvados colec mario  

-- | Função para criar o estado inicial para o Gloss, que inclui uma lista de imagens.
estadoGlossInicial :: [Picture] -> Jogador -> Inimigo -> Itens -> EstadoGloss
estadoGlossInicial z skin inimigo itens = (estadoInicial,[], z, skin, inimigo,itens)


-- | Função para lidar com eventos no Gloss.
reageEventoGloss :: Event -> EstadoGloss -> EstadoGloss
reageEventoGloss evento@(EventKey k Down _ _) (jogo@(Jogo (Opcoes _) m mal b jo) ,keys, z,skin, inimigo, itens) = (stageMenu k jogo, reactKey evento keys, z,skin, inimigo,itens)
                                                                                             where op = menu estadoInicial
reageEventoGloss (EventKey (SpecialKey KeySpace) Down _ _) (j@(Jogo _ _ _ _ jog@(Personagem {posicao = (x,y)})),keys,z,skin,inimigo,itens) = (if colisoesParede (mapa j) jog Norte then j else jogada (Just Saltar) j,keys,z,skin,inimigo,itens)
reageEventoGloss evento (estadoInicial,keys, z,skin, inimigo,itens) = (estadoInicial, reactKey evento keys, z,skin, inimigo,itens)

-- | Função para atualizar o estado do jogo.
atualizaEstado :: Float -> EstadoGloss -> EstadoGloss
atualizaEstado n (estadoInicial@(Jogo _ mapa ini _ mario@Personagem {posicao = (x,y)}), keys, z, skin, inimigo,itens) 
      |x > 650 && y > -90 =(estadoInicial {menu = VenceuJogo},keys, z, skin, inimigo, itens)
      |otherwise = (aplicaListaKey keys (movimenta 10 10 estadoInicial), keys, z, skin, inimigo,itens)
   -- where colisoes= colisoesChao mapa mario || colisoesParede mapa mario || colisoesPersonagem ini mario


-- | Função que atualiza o estado do jogo com base na tecla pressionada e no estado atual do jogo.
stageMenu :: Key -> Jogo -> Jogo
stageMenu k jogo = case menu jogo of 
                    (Opcoes op) ->
                        case k of 
                                (SpecialKey KeyEnter) -> case op of
                                                            Jogar -> jogo {menu=ModoJogo}   --Faz com que o volte pro reageEventoGloss mas agora com a forma 'ModoJogo' entao o jogo segue normal
                                                            Sair -> error "Fim do jogo"    
                                (SpecialKey KeyDown) -> jogo {menu = Opcoes (mudaOP op)}                           
                                (SpecialKey KeyUp)   -> jogo {menu = Opcoes (mudaOP op)}  
                                _ -> jogo  
                    _ -> jogo

-- | Função que muda a opção selecionada no menu
mudaOP :: Opcao -> Opcao
mudaOP op = case op of  
               Jogar -> Sair
               Sair -> Jogar

-- | Desenha menu vitória
drawVitoria :: Picture
drawVitoria = Pictures [scale 2 2 $ Translate (-200) 50 $ Color red $ text "Donkey Kong"
                                                ,Translate (-50) (-30) $ Color blue $ text "Jogar",
                                                 Translate (-50) (-120) $ Color white $ text "Sair"]
   
-- | Função que desenha o estado atual do jogo na tela usando a biblioteca Gloss.
desenhaEstadoGloss :: EstadoGloss -> Picture
desenhaEstadoGloss (Jogo {menu = VenceuJogo},_,_,_,_,_) = drawVitoria
desenhaEstadoGloss (Jogo {menu = Opcoes op},_,_,_,_,_)  = drawOptions op
        where
        drawOptions op =   case op of
                    Jogar -> Pictures [scale 2 2 $ Translate (-200) 50 $ Color red $ drawOption "Donkey Kong"
                                                ,Translate (-50) (-30) $ Color blue $ drawOption "Jogar",
                                                 Translate (-50) (-120) $ Color white $ drawOption "Sair"]
                    Sair ->  Pictures [scale 2 2 $ Translate (-200) 50 $ Color red $ drawOption "Donkey Kong",
                                                Color white $ Translate (-50) (-30) $ drawOption "Jogar",
                                                Color blue $ Translate (-50) (-120) $ drawOption "Sair"]
        drawOption option =  Scale 0.5 0.5 $ Text option
desenhaEstadoGloss (estadoinicio@(Jogo ModoJogo _ _ _ p@Personagem {posicao = (x,y)}),keys, z,skin, inimigo, itens) = desenhaMapa estadoinicio z skin inimigo itens
        
-- | Função principal que inicia o jogo.
main :: IO ()
main = do
    loadMAPA <- carregaImagens
    marioOeste <- loadBMP "./img/Mariooeste.bmp"
    marioLeste <- loadBMP "./img/Marioleste.bmp"
    macaco <- loadBMP "./img/macaco.bmp"
    fantasma <- loadBMP "./img/fantasma.bmp"
    martelo <- loadBMP "./img/Hammer.bmp"
    moeda <- loadBMP "./img/coin.bmp"
    estrela <- loadBMP "./img/star.bmp"
    play  
        dm                              -- janela onde esta a decorrer
        black                           -- cor do fundo da janela
        fr                              -- framerate
        (estadoGlossInicial loadMAPA (Jogador,[marioOeste,marioLeste]) [(MacacoMalvado,macaco),(Fantasma, fantasma)] [(Martelo,martelo),(Moeda,moeda),(Estrela,estrela)])    -- estado inicial
        desenhaEstadoGloss              -- desenha o estado do jogo
        reageEventoGloss                -- reage a um evento
        atualizaEstado                  -- reage ao passar do tempo-}
