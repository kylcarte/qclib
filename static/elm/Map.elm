
module Map where

mapS : (a -> Signal b) -> [a] -> Signal [b]
mapS f xs = case xs of
  [] -> constant []
  x::xs' -> (::) <~ f x ~ mapS f xs'

