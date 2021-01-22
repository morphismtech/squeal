module Has where

class Has (s :: fld) (xs :: [(fld, k)]) (x :: k) | fld xs -> x
