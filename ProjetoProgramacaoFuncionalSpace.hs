{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import CodeWorld
import System.Random
type Pixel = Char
type Pic = [String]

alien12 = [
    #     #   ,
     #   #    ,
    #######   ,
   ## ### ##  ,
  ########### ,
  ########### ,
  # #     # # ,
     ## ##    
 ]

alien11 = [
    #     # ,
  #  #   #  #,
  # ####### #,
  ### ### ###,
   ######### ,
    ####### ,
     #   # ,
   ###   ###
 ]

alien21 = [
      ###      ,
   #########   ,
  ###########,
  ##  ###  ## ,
  ########### ,
    ##   ##   ,
   ## ###  ##,
  ##        ##
 
 ]

alien22 = [
      ###      ,
   #########   ,
  ###########,
  ##  ###  ## ,
  ########### ,
    ##   ##   ,
   ## ### ##,
    ##   ##
 
 ]

alien31 = [
      ##      ,
     ####   ,
    ###### ,
   ## ## ## ,
   ######## ,
     #  #   ,
    # ## #,
   # #  # #
 
 ]

alien32 = [
      ##      ,
     ####   ,
    ###### ,
   ## ## ## ,
   ######## ,
    # ## #   ,
   #      #,
    #    # 
 
 ]

desNave = [
     ##    ,
    ####   ,
   ##  ## ,
   ###### ]

blocoAlien = [
  # # # # # # # # # # # ,
  # # # # # # # # # # # ,
  # # # # # # # # # # # ,
  # # # # # # # # # # # ,
  # # # # # # # # # # # 
 ]


geraMatriz m = zip [1..] $ map (map h . filter g) (map f (m))
    where 
      f ns = zip [1..] ns
      g (a,b) = b == '#'
      h (a,b) = a 
      i str = [1..(length str)]

aplicaDistr str = concatMap distribui str 
distribui (a,b) = zip (replicate (length b) a) b

desenha  Pic - Picture
desenha a = reflected pi . 
                pictures . 
                map posicionaPixel $ aplicaDistr $ 
                geraMatriz a
  where
    posicionaPixel ls = translated (snd ls) (fst ls) (pixel)

alternaImagem (a,b) t  sin (3pi2t)  0 = a
                       otherwise = snd (a,b)

montaBloco (t) = translated (-9) 3 $ pictures $ map posicionaElementos coordenadasBlocoAlien
  where
    posicionaElementos (linha,coluna) 
       linha == 1  linha == 2 = translated (0.7coluna) linha (alienAnimado alien21 alien22 t)
       linha == 3  linha == 4 = translated (0.7coluna) linha (alienAnimado alien11 alien12 t)
       otherwise = translated (0.7coluna) linha (alienAnimado alien31 alien32  t)
             
             
pixel = colored white (solidRectangle 1 1)             
coordenadasBlocoAlien = aplicaDistr $ geraMatriz blocoAlien
alienAnimado a b t = dilated 0.1 $ alternaImagem (desenha a, desenha b) t
----------------------------------
--Criar um objeto explosao que vai ter um timer pequeno, a bala morre, o alien morre
--Efetuar dois tipos de colisao, para a nave defensora e para os aliens 

main = activityOf mundoInicial atualize visualize 

--type World = (Nave, Bala, Alien, Relogio)
--type Nave = (Double, Double, Vidas)
--type Alien = (Double, Double, Double, Double, [Point])
--type Bala = (Point, Bool, Double)
type Relogio = Double
type Vida = Int

--Registros

data Nave = Nave {posNave  Point,
                  vidas  Vida,
                  direcaoNave  Double,
                  velNave  Double}
                  deriving (Eq, Ord)
                  
data Alien = Alien {posAlien  Point,
                    timerAlien  Relogio,
                    direcaoAlien  Double,
                    coordAl  [Point]}
                    deriving (Eq, Ord)
                    
data Bala = Bala {posBala  Point,
                  velBala  Double,
                  estaAtiva  Bool}
                  deriving (Eq, Ord)
                  
data World = World {nave  Nave,
                    alien  Alien,
                    bala  Bala,
                    tempo  Relogio}
                    

mundoInicial = World {nave = naveInicial,
                      bala = balaInicial,
                      alien = alienInicial,
                      tempo = tempoInicial}


tempoInicial = 0
scenario = solidRectangle 20 20
tela (x,y) =  -10 = x && x = 10 &&
              -10 = y && y = 10
              
--
naveY = (-8.5)
velocidadeNave = 7
comprimentoNave = 0.1

naveInicial = Nave {posNave = (0,0),
                    direcaoNave = 0,
                    vidas = 3,
                    velNave = 0}

atualizeNave w n1 = w {nave = n1}
navePraDireita nv@Nave {..} = nv {direcaoNave = 1}
navePraEsquerda nv@Nave {..} =  nv {direcaoNave = -1}
naveParada nv@Nave {..} = nv {direcaoNave = 0}
morreNave t nv@Nave {..} = nv {direcaoNave = 0, vidas = 0} --jogo acaba
passaTempoNave t nv@Nave {direcaoNave = d, posNave = (x,y)} = nv {posNave = (x+ dvelocidadeNavet,y)}
visualizeNave Nave {posNave = (x, y), vidas = vidaAtual}  x  -10.5 = translated (-10.5) naveY $ colored (dark green) (dilated 0.22 (desenha desNave))
                     x  8 = translated 8 naveY $ colored (dark green) (dilated 0.22 (desenha desNave))
                     vidaAtual == -3
                    = blank
                     otherwise = translated x naveY $ colored (dark green) (dilated 0.22 (desenha desNave))


velocidadeBala = 7
balaInicial = Bala {posBala = (0, 0),
                    estaAtiva = False,
                    velBala = 0}

visualizeBala Bala {posBala = (a,b), estaAtiva = True} = translated a b $ colored red $ solidRectangle 0.18 0.18 
visualizeBala _ = blank 
atualizeBala w b1 = w {bala = b1}
tiro _ ba@ Bala {estaAtiva = True} = ba
tiro nv@ Nave {posNave = (x, _), direcaoNave = d} _ = Bala {posBala = (x+1.2, naveY + comprimentoNave2), estaAtiva = True, velBala = dvelocidadeNave}
apagaBala t ba = ba {estaAtiva = False}
passaTempoBala t ba@Bala {estaAtiva = False} = ba
passaTempoBala t ba@Bala {posBala = p, estaAtiva = True, velBala = vx}
   tela newP = ba {posBala = newP}
   otherwise         = ba {posBala = p, estaAtiva = False}
  where
    newP = vectorSum p (scaledVector t (vx, velocidadeBala))
    
--
alienY = 9
dx = 0.05
alienTimer = 0.5

alienInicial = Alien {posAlien = (0,0),
                      direcaoAlien = -1, 
                      timerAlien = alienTimer,
                      coordAl = []}
                      
visualizeAlien Alien {posAlien = (x, y), direcaoAlien = d} t =  translated x y $ montaBloco t
passaTempoAlien t al@Alien {posAlien = (x, y), direcaoAlien = d, timerAlien = tmp, coordAl = newpCoord}
    tmp  0               = al {timerAlien = tmp-t}
    d == 1 && newX  2.5 
     d == -1 && newX  -2.5  = al {posAlien = (x,newY), direcaoAlien = -d, coordAl = newpCoord}
    otherwise             = al {posAlien = (newX,y), direcaoAlien = d, coordAl = newpCoord}
   where
     newX = x + ddx    
     newY = y-1
     newpCoord = map (soma (newX,newY)) ajusCoord
      where
        soma (a,b) (c,d) = (c + a, d + b) --só fiz pra testar no lugar de listaPosicoesBA

ajusCoord = map (translCoord) coordenadasBlocoAlien --ajusta a coordenada transladada
  where
    translCoord (a, b) = (a - 93, b + 3) --numeros arbitrarios

coordFilt p = filter colisao ajusCoord
  where colisao x = distanciaPontos p x = 1

distanciaPontos (a,b) (c,d) = sqrt $ (a-c)^2 + (b-d)^2

primFilaAlien [] = []
primFilaAlien [x] = [x]
primFilaAlien (xyys) --filtra só a primeira linha
     snd x  snd y = x primFilaAlien ys
     otherwise = primFilaAlien (yys)

ultFilaAlien [] = []
ultFilaAlien [x] = [x]
ultFilaAlien (xyys)
   snd x  snd y = ultFilaAlien (xys)
   otherwise = xultFilaAlien (yys)

comparaLista  Point - [Point] - Bool
comparaLista _ [] = False
comparaLista n (xxs)  distanciaPontos n x = 0.05 = True
                       otherwise = comparaLista n xs
  
comparaLista2  Point - [Point] - Bool
comparaLista2 n xs = any compDist xs
 where
  compDist x = distanciaPontos n x = 2.5
                      
visualize World {nave = nv, bala = ba, alien = al, tempo = tmp}  = visualizeNave nv & visualizeBala ba & visualizeAlien al tmp & scenario

atualize  Event - World - World
atualize (KeyPress Left)     w@ World {nave = nv} = atualizeNave w (navePraEsquerda nv)
atualize (KeyRelease Left)   w@ World {nave = nv@ Nave{direcaoNave = d}} = atualizeNave w (naveParada nv)
atualize (KeyPress Right)    w@ World {nave = nv} = atualizeNave w (navePraDireita nv)
atualize (KeyRelease Right)  w@ World {nave = nv@ Nave{direcaoNave = d}} = atualizeNave w (naveParada nv)
atualize (KeyRelease  )      w@ World {nave = nv, bala = ba} = atualizeBala w (tiro nv ba)
atualize (TimePassing t)       w@ World {nave = nv@ Nave{posNave = (p,naveY)}, bala = ba@Bala{posBala = posiBala}, alien = al @ Alien{posAlien = (newX,newY), coordAl = newpCoord}, tempo = tmp} 
     comparaLista2 (posiBala) newpCoord == True = World {nave = passaTempoNave t nv, bala = apagaBala t ba, alien = passaTempoAlien tmp al, tempo = tmp + t}
     comparaLista (p, naveY) (ultFilaAlien(newpCoord)) == True = World {nave = morreNave t nv, bala = apagaBala t ba, alien = passaTempoAlien tmp al, tempo = tmp + t}
     otherwise = World {nave = passaTempoNave t nv, bala = passaTemoBala t ba, alien = passaTempoAlien tmp al, tempo = tmp + t}

atualize _ w                     = w   

 
