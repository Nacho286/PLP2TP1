module Arbol23 where

data Arbol23 a b = Hoja a | Dos b (Arbol23 a b) (Arbol23 a b) | Tres b b (Arbol23 a b) (Arbol23 a b) (Arbol23 a b)

{- Funciones para mostrar el árbol. -}

instance (Show a, Show b) => Show (Arbol23 a b) where
    show = ("\n" ++) . (padTree 0 0 False)

padlength = 5

padTree:: (Show a, Show b) => Int -> Int -> Bool -> (Arbol23 a b)-> String
padTree nivel acum doPad t = case t of
				  (Hoja x) -> initialPad ++ stuff x
                                  (Dos x i d) -> initialPad ++ stuff x ++
                                                 pad padlength ++ rec x False i ++ "\n" ++
                                                 rec x True d ++ "\n"
                                  (Tres x y i m d) -> initialPad ++ stuff x ++ --(' ':tail (stuff y)) ++
                                                      pad padlength ++ rec x False i ++ "\n" ++
                                                      pad levelPad ++ stuff y ++ pad padlength ++ rec x False m ++ "\n" ++
                                                      rec x True d ++ "\n"
  where l = length . stuff
	levelPad = (padlength*nivel + acum)
	initialPad = (if doPad then pad levelPad else "")
	rec x = padTree (nivel+1) (acum+l x)

stuff:: Show a => a -> String
stuff x = if n > l then pad (n-l) ++ s else s
  where s = show x
        l = length s
        n = padlength

pad:: Int -> String
pad i = replicate i ' '

{- Funciones pedidas. -}

foldA23::(a->c)->(b->c->c->c)->(b->b->c->c->c->c)->(Arbol23 a b)->c
foldA23 f g h arbol = case arbol of
  (Hoja a) -> f a
  (Dos l a1 a2) -> g l (recu a1) (recu a2)
  (Tres l1 l2 a1 a2 a3) -> h l1 l2 (recu a1) (recu a2) (recu a3)
  where recu = foldA23 f g h
--Lista en preorden de los internos del árbol.
internos::Arbol23 a b->[b]
internos = foldA23 (const []) (\ l1 l2 l3->[l1]++l2++l3) ( \ l1 l2 l3 l4 l5 ->[l1]++[l2]++l3++l4++l5)

--Lista las hojas de izquierda a derecha.
hojas::Arbol23 a b->[a]
hojas = foldA23 (\ l -> [l]) (\ l1 l2 l3->l2++l3) ( \ l1 l2 l3 l4 l5 ->l3++l4++l5)

esHoja::Arbol23 a b->Bool
esHoja = foldA23 (\h -> True) (\i h1 h2 -> False) (\i1 i2 h1 h2 h3 -> False)

mapA23::(a->c)->(b->d)->Arbol23 a b->Arbol23 c d
mapA23 f g = foldA23 (\h -> (Hoja (f h))) (\i h1 h2 -> (Dos (g i) h1 h2)) (\i1 i2 h1 h2 h3 -> (Tres (g i1) (g i2) h1 h2 h3))

--Ejemplo de uso de mapA23.
--Incrementa en 1 el valor de las hojas.
incrementarHojas::Num a =>Arbol23 a b->Arbol23 a b
incrementarHojas = mapA23 (+1) id
        
--Trunca el árbol hasta un determinado nivel. Cuando llega a 0, reemplaza el resto del árbol por una hoja con el valor indicado.
--Funciona para árboles infinitos.

--Para truncar se utilizo la recursión por los naturales
foldNat::Num a => Eq a =>(a -> b) -> (a -> b -> b) -> a -> b
foldNat = (\fBase fRec n -> case n of
    0 -> fBase 0
    n -> fRec n (foldNat fBase fRec (n-1)) )

--Crea una arbol23 Hoja
unaHoja::a->Arbol23 a b
unaHoja = \a -> (Hoja a)

--trunco
truncar::a->Integer->Arbol23 a b->Arbol23 a b
truncar =(\def num arbol -> foldNat (\num-> unaHoja def) (\n rec -> armando arbol rec n def) num )
    where
        armando (Hoja v) rec n def = Hoja v
        armando (Dos v h1 h2) rec n def = Dos v (truncar def (n-1) h1) (truncar def (n-1) h2)
        armando (Tres v1 v2 h1 h2 h3) rec n def = Tres v1 v2 (truncar def (n-1) h1) (truncar def (n-1) h2) (truncar def (n-1) h3)


--Evalúa las funciones tomando los valores de los hijos como argumentos.
--En el caso de que haya 3 hijos, asocia a izquierda.
evaluar::Arbol23 a (a->a->a)->a
evaluar = foldA23 id (\v rec1 rec2 -> v rec1 rec2) (\v1 v2 rec1 rec2 rec3-> v2 (v1 rec1 rec2) rec3)

--Ejemplo:
--evaluar (truncar 0 6 arbolito3) = 22 = (1*2-3)+(2*3-4)+(3*4-5)+(4*5-6)

{- Árboles de ejemplo. -}
arbolito1::Arbol23 Char Int
arbolito1 = Tres 0 1
        (Dos 2 (Hoja 'a') (Hoja 'b'))
        (Tres 3 4 (Hoja 'c') (Hoja 'd') (Dos 5 (Hoja 'e') (Hoja 'f')))
        (Dos 6 (Hoja 'g') (Dos 7 (Hoja 'h') (Hoja 'i')))

arbolito2::Arbol23 Int Bool
arbolito2 = Dos True (Hoja (-1)) (Tres False True (Hoja 0) (Hoja (-2)) (Hoja 4))

arbolito3::Arbol23 Int (Int->Int->Int)
arbolito3 = Dos (+) (Tres (*) (-) (Hoja 1) (Hoja 2) (Hoja 3)) (incrementarHojas arbolito3)

arbolito4::Arbol23 Int Char
arbolito4 = Dos 'p' (Dos 'l' (Dos 'g' (Hoja 5) (Hoja 2)) (Tres 'r' 'a' (Hoja 0)(Hoja 1)(Hoja 12)))
                    (Dos 'p' (Tres 'n' 'd' (Hoja (-3))(Hoja 4)(Hoja 9)) (Dos 'e' (Hoja 20)(Hoja 7)))
