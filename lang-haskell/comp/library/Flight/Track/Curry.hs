module Flight.Track.Curry (uncurry4, uncurry7) where

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 fn ~(a, b, c, d) = fn a b c d

uncurry7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> (a, b, c, d, e, f, g) -> h
uncurry7 fn ~(a, b, c, d, e, f, g) = fn a b c d e f g
