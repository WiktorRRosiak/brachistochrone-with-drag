import System.IO
import Data.List
import Data.Tuple
-- liczenie wartości dowolnego wielomianu dla zadanego x przy współczynnikach [args]:
f :: Num a => [a] -> a -> a
f [] _ = error "Brak wykładników"
f [a] x = a
f (a:b:[]) x = a * x + b 
f (a:b:c:end) x -- f args x
 | n == 0 = 1
 | n == 1 = a 
 | n == 3 = a*x^2 + b*x + c
 | otherwise = a*x^n + b*x^(n-1) + c*x^(n-2) + f end x
 where n = length(a:b:c:end)
type Punkt = (Double, Double) -- krotka współrzędnych x, y
type Funkcja = [Punkt] -- lista punktów, czyli funkcja
--rzutowanie funkcji matematycznej na typ Funkcja:
funkcja :: [Double] -> Double -> Double -> Double -> Funkcja
funkcja args x0 x1 x = [head(zip [a] [f args a]) | a <- [x0, x1 .. x]]
--funkcja różniczkująca współczynniki wielomianu:
derivative :: Num a => [a] -> [a]
derivative xs = take n t 
 where 
  t = [(xs !! a) * fromIntegral(n-a-1) | a <- [0,1..length xs - 1], n <- [length xs]]
  n = length t - 1
--funkcja całkująca współczynniki wielomianu
integral :: Fractional a => [a] -> [a]
integral xs = [(xs !! a)  / fromIntegral b | l <- [length xs], a <- [0,1.. l- 1], b <- [l - a]] ++ [0]
--średnia z funckji
średnia :: Funkcja -> Double
średnia xs = sum (map snd xs) / genericLength xs
--długość krzywej danej wielomianem na długości od a do b przy czułości dx i współczynnikach args
len a b dx args = sum $ map (\x -> ds x dx args) [a, a + dx .. b]
 where
 ds :: Floating a => a -> a -> [a] -> a  
 ds x dx args = sqrt ( 1 + (f (derivative args) x) ** 2 ) * dx
-- równa się około
approx :: Double -> Double -> Bool
approx a b = abs (a - b) < 0.002
--wszystkie możliwe parabole przechodzące przez (0,0) i (a,b)
możliwe :: Punkt -> [[Double]]
możliwe (x,y) = [[a,b,0] | a <- [0, 0.001 .. 2], b <- [(y - a*x**2) / x], approx (a*x**2 + b*x) y == True]
