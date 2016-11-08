import System.IO
import Data.List
import Data.Tuple
--by zainstalować bibioteki graficzne należy wywołać następujące komendy w terminalu:
--cabal update
--cabal install chart-diagrams
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
-- liczenie wartości dowolnego wielomianu dla zadanego x przy współczynnikach [args]:
f :: Num a => [a] -> a -> a
f [] _ = 0
f args x = a*x^n + f (tail args) x
 where 
  a = head args
  n = length args - 1
type Punkt = (Double, Double) -- krotka współrzędnych x, y
type Funkcja = [Punkt] -- lista punktów, czyli funkcja
--rzutowanie funkcji matematycznej na typ Funkcja:
funkcja :: [Double] -> Double -> Double -> Double -> Funkcja
funkcja args x0 dx x = [head(zip [a] [f args a]) | a <- [x0, (x0 + dx) .. x]]
--funkcja różniczkująca współczynniki wielomianu:
pochodna :: Num a => [a] -> [a]
pochodna xs = take n t 
 where 
  t = [(xs !! a) * fromIntegral(n-a-1) | a <- [0,1..length xs - 1], n <- [length xs]]
  n = length t - 1
--funkcja całkująca współczynniki wielomianu
całka :: Fractional a => [a] -> [a]
całka xs = [(xs !! a)  / fromIntegral b | l <- [length xs], a <- [0,1.. l - 1], b <- [l - a]] ++ [0]
--średnia z funckji
średnia :: Funkcja -> Double
średnia xs = sum (map snd xs) / genericLength xs
--długość krzywej danej wielomianem na małym dx 
ds :: Floating a => a -> a -> [a] -> a  
ds x dx args = sqrt ( 1 + (f (pochodna args) x) ** 2 ) * dx
--długość krzywej danej wielomianem na długości od a do b przy czułości dx i współczynnikach args
--długość :: Double a => a -> a -> a -> [a] -> a
długość a b dx args = sum $ map (\x -> ds x dx args) [a, a + dx .. b]
--wszystkie możliwe parabole postaci ax^2 + bx przechodzące przez (0,0) i (a,b)
możliwe :: Punkt -> Double -> [[Double]]
możliwe (x,y) dx = [[a,b,0] | a <- [0, 0 + dx .. 0.5], b <- [(y - a*x**2) / x]]
--rysowanie wszystkich zadanych funkcji
draw :: (PlotValue y0, PlotValue x0) => [[(x0, y0)]] -> IO ()
draw funkcje = toFile def "brachistochrona.svg" $ do
 layout_title .= "Brachistochrona"
 setColors [opaque blue, opaque red]
 rysuj funkcje
 where
  rysuj [] = return ()
  rysuj (x:xs) = do
  plot (line "f(x)" [x])
  rysuj xs
--nachylenia pochodnej z postaci dy/dx = tan alfa
alfy :: [Double] -> Double -> Double -> Double -> [Double]
alfy args x0 dx x = [alfa dx dy | [dx, dy] <- delty (funkcja args x0 dx x) args]
 where
  alfa dx dy = atan $ dy / dx
  delty (x:[]) _ = []
  delty (a:b:end) args = [[fst b - fst a, snd b - snd a]] ++ delty (b:end) args
--parametry podczas ślizgu
parametry _ _ _ _ _ _ _ _ [] = [] 
parametry g k args dx v0 a0 t0 (p1:punkty) (kąt:kąty) = [[v, a, t]] ++ parametry g k args dx v a t punkty kąty
 where
  v = dv v0 a0 t0
  a = if fst p1 < fst (last punkty) then da g kąt k v else -1 * (da g kąt k v) 
  t = dt (ds (fst p1) dx args) v a
  dv v0 a t = a * t + v0
  da g alfa k v = if alfa >= 0 then g * (sin alfa) - k * v**2 else g * (sin (pi + alfa)) - k * v**2
  dt s v a = if v == 0 then sqrt(2*s / a) else s/v
czas g k args dx v0 a0 t0 (p1:punkty) (kąt:kąty) = sum $ map (\x -> x!!2) $ parametry g k args dx v0 a0 t0 (p1:punkty) (kąt:kąty)
wyniki g k dx v0 a0 t0 punkt = zip (map head (możliwe punkt dx)) $ map (\args -> czas g k args dx v0 a0 t0 (funkcja args 0 dx (fst punkt)) (alfy args 0 dx (fst punkt))) $ możliwe punkt dx
-- chyba nie działa przez dx
