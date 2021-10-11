module Nd_strongEWC_DWC where
import Data.List


-- Auxiliar function
noInt x = not (x == fromInteger (round x))

-- List of the hypercubes of dimension n
hypercubes_list n = iteration n [[a] | a <- [0,1]]

-- Auxiliar function
iteration 1 xs = xs
iteration n xs = iteration (n-1) [x ++ [a] | x <- xs, a <- [0,1]]


-- Cells of given dimension.
cell 0 hs = hs
cell i hs= nub (cell (i-1) (suma_general hs))


suma h = [take i h++[ t + (h!!i)]++(drop (i+1) h)| i <- [0..(length h)-1],t <- [-0.5,0.5], not (noInt (h!!i)) ]
suma_general = concat. map suma


not_adjacent xss = [xs | xs <- xss, (elem 0.5 xs == False)]

adjacents xss = [xs | xs <- xss, not (elem 0.5 xs == False)]
 
adjacent x xs = [y | y <- xs, hammingDistance x y == 1]


removeItems xs ys = [x | x <- xs, not (elem x ys)]


dualHcubes n xs = removeItems (hypercubes_list n) xs


-- Determine if there is a path between two elements. 

isthereapath :: (Eq a, Eq t, Num t) => t -> [a] -> [a] -> [[a]] -> Bool
isthereapath n x y xs | x == y = True
                      | n == 0 = False
                      | otherwise = or [isthereapath (n-1) t y xs | t <- (adjacent x xs)]


-- Is dwc?
dwc_aux :: Eq a => [[a]] -> Bool
dwc_aux xss = and [isthereapath (length xss) xs ys xss | ys <- xss, xs <- xss]

dwc xss =  and [dwc_aux (star 0 v xss) | v <- cell n xss]
  where
    n = length (head xss)


-- Is strong XWC?

xwc xss = and [euler_star v xss == 0 | i<-[1..n], v <- cell i xss, boundary v xss]
  where
    n = length (head xss)

-- ------------

hammingDistance :: Eq a => [a] -> [a] -> Int
hammingDistance = (sum .) . zipWith ((fromEnum .) . (/=))


remove xs xss = [ys | ys <- xss, xs/= ys]

intersection xss = nub (concat [intersect x y | x <- xss, y <- xss, x/=y])


-- Determine if a cell c is in the boundary of a configuration hs.
boundary c hs = or [face c bi | bi <- b]
    where
       fs = cell 1 hs
       g f hs = sum [1 | h <- hs, face f h] == 1 -- to determine if it is a face of the boundary
       b= [f | f <- fs, g f hs ]



-- Dimension of a cell
dim e = sum [1 | i <- e, i==0||i==1]


-- Determine if a cell e is a face of a cell c
face e c | dim c >= dim e = elem e (cell k [c])
         | otherwise = False
     where
        k = dim c - dim e

-- Computes the cells of a given dimension in a configuration hs that contains e as a face.
star k e hs = [c | c <- cell k hs, face e c] -- c /=e]


-- Computes the Euler characteristic of the star of a cell e in a configuration hs.
euler_star e hs =  sum [(length (star k e hs)) * (-1)^k | k <- [0..n] ]
           where
                n =  length e


-- List of hypercubes that contains a cell e as a face.
adjacent_hips e = [h | h <- hypercubes_list_general n, face e h]
              where
                n = length e
                
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs,
                             ys <- combinations (n-1) xs']          

-- All possible configurations of hypercubes that contain the cell e as a face. 
all_conf e = [c | c <- concat [combinations k hs | k <- [1..length hs]], elem (replicate n 0) c, f e c]
         where
                hs = adjacent_hips e
                n = length e
                f e c = length c <= 2^(n-dim e)-1


hypercubes_list_general n = iteration n [[a] | a <- [0,1,-1]]



-- ---------------------------------------------------------------------------------
-- Lemma 1

-- => X+=>DWC n=2,3

lemma1_imp1 = [and [dwc hs && dwc (dualHcubes 2 hs)| hs <- hss1, xwc hs],and [dwc hs && dwc (dualHcubes 3 hs)| hs <- hss2, xwc hs ]] 
 where
   hss1 = all_conf [0.5,0.5]
   hss2 = all_conf [0.5,0.5,0.5]

-- <= DWC => X+ n=2,3

lemma1_imp2 =   [and [xwc hs  | hs <- hss1, dwc hs, dwc (dualHcubes 2 hs)], and [xwc hs  | hs <- hss2, dwc hs, dwc (dualHcubes 3 hs)]] 
 where
   hss1 = all_conf [0.5,0.5]
   hss2 = all_conf [0.5,0.5,0.5]

-- > [lemma1_imp1, lemma1_imp2]
-- [[True,True],[True,True]]
