{- [1] Programá las siguientes funciones:
(a) esCero :: Int -> Bool, que verifica si un entero es igual a 0. -}
esCero :: Int -> Bool
esCero x | (x == 0) = True
         | (x /= 0) = False
        
esCerob :: Int -> Bool
esCerob x = x == 0
        
{- (b) esPositivo :: Int -> Bool, que verifica si un entero es estrictamente mayor a 0. -}
esPositivo :: Int -> Bool
esPositivo x = x >= 0

{- (c) esVocal :: Char -> Bool, que verifica si un caracter es una vocal en minúscula. -}
esVocal :: Char -> Bool
esVocal a = ((a == 'a') || (a == 'e') || (a == 'i') || (a == 'o') || (a == 'u'))
          

{- [2] Programá las siguientes funciones usando recursión o composición:
(a) paratodo :: [Bool] -> Bool, que verifica que todos los elementos de una lista sean True. -}
paraTodo :: [Bool] -> Bool
paraTodo [] = True
paraTodo (x:xs) = x && paraTodo xs

{- (b) sumatoria :: [Int] -> Int, que calcula la suma de todos los elementos de una lista de enteros. -}
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs 

{- (c) productoria :: [Int] -> Int, que calcula el producto de todos los elementos de la lista de enteros. -}
productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs

{- (d) factorial :: Int -> Int, que toma un número n y calcula n!. -}
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x-1)

{- (e) Utilizá la función sumatoria para definir, promedio :: [Int] -> Int, que toma una lista de números no 
vacía y calcula el valor promedio (truncado, usando división entera). -}
promedio :: [Int] -> Int
promedio [] = 0
promedio (x:xs) = div (sumatoria (x:xs)) (length (x:xs))


{- [3] Programá la función pertenece :: Int -> [Int] -> Bool, que verifica si un número se encuentra en una lista. -}
pertenece :: Int -> [Int] -> Bool
pertenece y [] = False
pertenece y (x:xs) = (x == y) || pertenece y xs

{- [4] Programá las siguientes funciones que implementan los cuantificadores generales. Notá que el segundo parámetro de cada 
función, es otra función.
(a) paratodo' :: [a] -> (a -> Bool) -> Bool, dada una lista xs de tipo [a] y un predicado t :: a -> Bool, determina si 
todos los elementos de xs satisfacen el predicado t. -}
paraTodo' :: [a] -> (a -> Bool) -> Bool
paraTodo' [] t = True
paraTodo' (x:xs) t = (t x) && paraTodo' xs t

{- (b) existe' :: [a] -> (a -> Bool) -> Bool, dada una lista xs de tipo [a] y un predicado t :: a -> Bool, determina si algún 
elemento de xs satisface el predicado t. -}
existe' :: [a] -> (a -> Bool) -> Bool
existe' [] t = False
existe' (x:xs) t = (t x) || existe' xs t

{- (c) sumatoria' :: [a] -> (a -> Int) -> Int, dada una lista xs de tipo [a] y una función t :: a -> Int (toma elementos de 
tipo a y devuelve enteros), calcula la suma de los valores que resultan de la aplicación de t a los elementos de xs. -}
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] t = 0
sumatoria' (x:xs) t = (t x) + sumatoria' xs t 

{- (d) productoria’ :: [a] -> (a -> Int) -> Int, dada una lista de xs de tipo [a] y una función t :: a -> Int, calcula el producto 
de los valores que resultan de la aplicación de t a los elementos de xs. -}
productoria' :: [a] -> (a -> Int) -> Int
productoria' [] t = 1
productoria' (x:xs) t = (t x) * productoria' xs t 


{- [5] Definí nuevamente la función paratodo, pero esta vez usando la función paratodo’ (¡sin recursión ni análisis por casos!). -}
paraTodo'' :: [Bool] -> Bool
paraTodo'' xs = paraTodo' xs id
         
{- [6] Utilizando las funciones del ejercicio 4, programá las siguientes funciones por composición, sin usar recursión ni análisis por casos.
(a) todosPares :: [Int] -> Bool, verifica que todos los números de una lista sean pares. -}
todosPares :: [Int] -> Bool
todosPares [] = True
todosPares (x:xs) = paraTodo' (x:xs) even

{- (b) hayMultiplo :: Int -> [Int] -> Bool, verifica si existe algún número dentro del segundo parámetro que sea múltiplo del primer parámetro. -}
esMultiplo :: Int -> Int -> Bool
esMultiplo n x = mod x n == 0

hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo n [] = True
hayMultiplo n (x:xs) = existe' (x:xs) (esMultiplo n)

{- (c) sumaCuadrados :: Int -> Int, dado un número no negativo n, calcula la suma de los primeros n cuadrados, es decir <Ei : 0 <= i < n : i^2>
Ayuda: En Haskell se puede escribir la lista que contiene el rango de números entre n y m como [n..m]. -}
cuadrados :: Int -> Int
cuadrados x = x*x

sumaCuadrados :: Int -> Int
sumaCuadrados x = sumatoria' [0..(x-1)] cuadrados

{- (d) ¿Se te ocurre como redefinir factorial (ej. 2d) para evitar usar recursión? -}
idenp :: Int -> Int
idenp x | (x==0)=1
        | otherwise = x

factorial' :: Int -> Int
factorial' x = productoria' [0..x] idenp

{- (e) multiplicaPares :: [Int] -> Int, que calcula el producto de todos los números pares de una lista. -}
esPar :: Int -> Int
esPar x |(mod x 2 == 0) = x
        |(mod x 2 /= 0) = 1
        
multiplicaPares :: [Int] -> Int
multiplicaPares (x:xs) = productoria' (x:xs) esPar


{- [8] Programá una función que dada una lista de números xs, devuelve la lista que resulta de duplicar cada valor de xs.
(a) Definila usando recursión. -}
duplicaA :: [Int] -> [Int]
duplicaA [] = []
duplicaA (x:xs) = 2*x : duplicaA xs

porDos :: Int -> Int
porDos x = x*2

{- (b) Definila utilizando la función map. -}
duplicaB :: [Int] -> [Int]
duplicaB [] = []
duplicaB (x:xs) = map porDos (x:xs)


{- [9] Programá una función que dada una lista de números xs, calcula una lista que tiene como elementos aquellos números de xs que son pares.
(a) Definila usando recursión. -}
paresA :: [Int] -> [Int]
paresA [] = []
paresA (x:xs) | (mod x 2 == 0) = x : paresA xs
              | (mod x 2 /= 0) = paresA xs   

{- (b) Definila utilizando la función filter. -}
paresB :: [Int] -> [Int]
paresB [] = []
paresB (x:xs) = filter even (x:xs)

{- (c) Revisá tu definición del ejercicio 6e. ¿Cómo podés mejorarla? -}
multiplicaParesB :: [Int] -> Int
multiplicaParesB [] = 1
multiplicaParesB (x:xs) = productoria (filter even (x:xs))


{- [10] La función primIgualesA toma un valor y una lista, y calcula el tramo inicial más largo dela lista cuyos elementos son iguales 
a ese valor. Por ejemplo:
primIgualesA 3 [3,3,4,1] = [3,3]
primIgualesA 3 [4,3,3,4,1] = []
primIgualesA 3 [] = []
primIgualesA ’a’ "aaadaa" = "aaa"

(a) Programá primIgualesA por recursión. -}
primIgualesA :: Eq a => a -> [a] -> [a]
primIgualesA y [] = []
primIgualesA y (x:xs) | (y==x) = x : primIgualesA y xs
                      | (y/=x) = []

{- (b) Programá nuevamente la función utilizando takeWhile. -}
igualAY :: Eq a => a -> a -> Bool
igualAY y x = (y==x) 

primIgualesA' :: Eq a => a -> [a] -> [a]
primIgualesA' y [] = []
primIgualesA' y (x:xs) = takeWhile (igualAY y) (x:xs)


{- [11] La función primIguales toma una lista y devuelve el mayor tramo inicial de la lista cuyos elementos son todos iguales entre 
sí. Por ejemplo:
primIgualesA 3 [3,3,4,1] = [3,3]
primIgualesA 3 [4,3,3,4,1] = []
primIgualesA 3 [] = []
primIgualesA ’a’ "aaadaa" = "aaa"

(a) Programá primIguales por recursión. -}
primIguales :: Eq a => [a] -> [a]
primIguales [] = []
primIguales [x] = [x]
primIguales (x:y:xs) | (x==y) = x: primIguales (y:xs)
                     | (x/=y) = x:[]

{- (b) Usá cualquier versión de primIgualesA para programar primIguales. Está permitido dividir en casos, pero no usar recursión. -}
primIguales' :: Eq a => [a] -> [a]
primIguales' [] = []
primIguales' (x:xs) = primIgualesA x (x:xs)