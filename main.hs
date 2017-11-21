
data PinPong = Pin | Pong deriving (Show,Eq)

f xs = [(last xs)] ++ xs ++ [(head xs)]

pp x y z = do
  let no = if y /= Pin then Pin else Pong
  if x /= z then y else no

ppp :: [PinPong] -> Int -> PinPong
ppp xs i = do
  pp (xs !! (i-1)) (xs !! i) (xs !! (i+1))

main = do
  let xs = replicate 3 Pin
  print $ xs
  print $ map (ppp $ f xs) [1..(length xs)]
