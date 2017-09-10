import Diccionario
import Data.Maybe
import Arbol23
import Test.HUnit

--Este módulo sirve para utilizar el diccionario sin acceder a su estructura interna. Pueden agregar otras funciones o casos de prueba.

{- Función a implementar. -}

búsquedaDelTesoro::Eq a=>a->(a->Bool)->Diccionario a a->Maybe a
búsquedaDelTesoro x f dicc = encontrar f (generarLista x dicc)

{- Funciones auxiliares: -}

encontrar:: (a->Bool)->[Maybe a]->Maybe a
encontrar f = foldr (\x recu -> if isNothing x then Nothing else (if  f (fromJust x)  then x else recu)) Nothing

generarLista::  Eq a => a-> Diccionario a a -> [Maybe a]
generarLista x dicc = iterate (\y-> if isNothing y then Nothing else obtener (fromJust y) dicc) (Just(x))


{- Diccionarios de prueba: -}

dicc1::Diccionario Int String
dicc1 = definirVarias [(0,"Hola"),(-10,"Chau"),(15,"Felicidades"),(2,"etc."),(9,"a")] (vacio (<))

dicc2::Diccionario String String
dicc2 = definirVarias [("inicio","casa"),("auto","flores"),("calle","auto"),("casa","escalera"),("ropero","alfajor"),("escalera","ropero")] (vacio (<))

dicc3::Diccionario Int String
dicc3 = definirVarias [(0,"Hola"),(-10,"Chau"),(15,"Felicidades"),(2,"etc."),(9,"a")] (vacio (\x y->x `mod` 5 < y `mod` 5))

dicc4::Diccionario String String
dicc4 = definirVarias [("0","Hola"),("10","Chau"),("15","Felicidades"),("2","etc."),("9","a")] (vacio (<))



--arboles de prueba
arbol1 = Tres 'A' 'B' (Hoja 1) (Dos 'C' (Hoja 2) (Hoja 3)) (Tres 'D' 'E' (Dos 'F' (Hoja 4) (Hoja 5)) (Hoja 6) (Hoja 7))

--Ejecución de los tests
main :: IO Counts
main = do runTestTT allTests

allTests = test [
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7,
  "ejercicio8" ~: testsEj8,
  "ejercicio9" ~: testsEj9,
  "ejercicio10" ~: testsEj10
  ]

testsEj2 = test [
  [0,1,2,3,4,5,6,7] ~=? internos arbolito1,
  "abcdefghi" ~=? hojas arbolito1,
  [True,False,True] ~=? internos arbolito2,
  [1,2,3,2,3,4,3,4,5,4] ~=? take 10 (hojas arbolito3)
  ]

testsEj3 = test [
  [0,1,-1,5] ~=? hojas (incrementarHojas arbolito2)
  ]

testsEj4 = test [
  [1,2,3,2,3,4,3,4,5,4,5,6,0,0,0,0,0] ~=? hojas (truncar 0 6 arbolito3)
  ]

testsEj5 = test [
  22 ~=? evaluar (truncar 0 6 arbolito3)
  ]

testsEj6 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]

testsEj7 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]

testsEj8 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]

testsEj9 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]

testsEj10 = test [
  Just "alfajor" ~=? búsquedaDelTesoro "inicio" ((=='a').head) dicc2,
  Just "Chau" ~=? búsquedaDelTesoro "10" ((=='C').head) dicc4,
  Nothing ~=? búsquedaDelTesoro "10" ((=='F').head) dicc4
  ]
