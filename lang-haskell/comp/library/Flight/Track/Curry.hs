module Flight.Track.Curry (uncurry4, uncurry5, uncurry6, uncurry7) where

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 fn ~(a, b, c, d) = fn a b c d

uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 fn ~(a, b, c, d, e) = fn a b c d e

uncurry6 :: (a -> b -> c -> d -> e -> f -> g) -> (a, b, c, d, e, f) -> g
uncurry6 fn ~(a, b, c, d, e, f) = fn a b c d e f

uncurry7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> (a, b, c, d, e, f, g) -> h
uncurry7 fn ~(a, b, c, d, e, f, g) = fn a b c d e f g
