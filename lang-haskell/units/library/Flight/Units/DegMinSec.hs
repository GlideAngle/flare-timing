{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Flight.Units.DegMinSec
    ( DMS(..)
    , DMS_(..)
    , DiffDMS
    , diffDMS
    , absDiffDMS
    , diffDMS180
    , absDiffDMS180
    , toDeg
    , toQDeg
    , toQRad
    , fromQ
    ) where

import Prelude hiding (min)
import Data.Fixed (mod', divMod')
import Data.Text.Lazy (unpack)
import Formatting (format)
import Text.Printf (printf)
import qualified Formatting.ShortFormatters as Fmt (sf)
import Data.UnitsOfMeasure ((+:), (-:), u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.UnitsOfMeasure.Convert (Convertible)

import Flight.Units.Angle (Angle(..))

newtype DMS = DMS (Int, Int, Double) deriving Eq

type DiffDMS = DMS -> DMS -> DMS

instance Show DMS where
    show = showDMS

-- | Like @DMS@ but with fewer decimal places in the seconds when shown.
newtype DMS_ = DMS_ (Int, Int, Double) deriving Eq

instance Show DMS_ where
    show (DMS_ x) = showDMS_ (DMS x)

secToShow :: Double -> String
secToShow sec =
    if fromIntegral isec == sec
        then show (abs isec)
        else unpack $ format Fmt.sf (abs sec)
    where
        isec :: Int
        isec = floor sec

secToShow_ :: Double -> String
secToShow_ sec =
    if fromIntegral isec == sec
        then show (abs isec)
        else printf "%.6f" (abs sec)
    where
        isec :: Int
        isec = floor sec

showDMS :: DMS -> String
showDMS (DMS (deg, 0, 0)) =
    show deg ++ "°"
showDMS (DMS (0, 0, sec)) =
    secToShow_ sec ++ "''"
showDMS dms@(DMS (deg, min, 0)) =
    signSymbolDMS dms
    ++ show (abs deg)
    ++ "°"
    ++ show (abs min)
    ++ "'"
showDMS dms@(DMS (0, min, sec)) =
    signSymbolDMS dms
    ++ show (abs min)
    ++ "'"
    ++ secToShow sec
    ++ "''"
showDMS dms@(DMS (deg, min, sec)) =
    signSymbolDMS dms
    ++ show (abs deg)
    ++ "°"
    ++ show (abs min)
    ++ "'"
    ++ secToShow sec
    ++ "''"

showDMS_ :: DMS -> String
showDMS_ (DMS (deg, 0, 0)) =
    show deg ++ "°"
showDMS_ (DMS (0, 0, sec)) =
    secToShow sec ++ "''"
showDMS_ dms@(DMS (deg, min, 0)) =
    signSymbolDMS dms
    ++ show (abs deg)
    ++ "°"
    ++ show (abs min)
    ++ "'"
showDMS_ dms@(DMS (0, min, sec)) =
    signSymbolDMS dms
    ++ show (abs min)
    ++ "'"
    ++ secToShow_ sec
    ++ "''"
showDMS_ dms@(DMS (deg, min, sec)) =
    signSymbolDMS dms
    ++ show (abs deg)
    ++ "°"
    ++ show (abs min)
    ++ "'"
    ++ secToShow_ sec
    ++ "''"

-- |
-- >>> toDeg $ DMS (0, 0, 0)
-- 0.0
--
-- >>> toDeg $ DMS (289, 30, 0)
-- 289.5
--
-- prop> \d -> (toDeg $ DMS (d, 0, 0.0)) == fromIntegral d
-- prop> \m -> (toDeg $ DMS (0, m, 0.0)) == (fromIntegral m) / 60.0
-- prop> \s -> (toDeg $ DMS (0, 0, s)) == s / 3600.0
--
-- prop> \d m -> (toDeg $ DMS (d, m, 0.0)) == sign_ (fromIntegral d, fromIntegral m, 0.0) * ((fromIntegral $ abs d) + (fromIntegral $ abs m) / 60.0)
-- prop> \d s -> (toDeg $ DMS (d, 0, s)) == sign_ (fromIntegral d, 0.0, s) * ((fromIntegral $ abs d) + abs s / 3600.0)
-- prop> \m s -> (toDeg $ DMS (0, m, s)) == sign_ (0.0, fromIntegral m, s) * ((fromIntegral $ abs m) / 60.0 + abs s / 3600.0)
--
-- prop> \d m s -> (toDeg $ DMS (d, m, s)) == sign_ (fromIntegral d, fromIntegral m, s) * ((fromIntegral $ abs d) + (fromIntegral $ abs m) / 60.0 + abs s / 3600.0)
toDeg :: DMS -> Double
toDeg dms@(DMS (deg, min, s)) =
    signDMS dms * (abs d + abs m / 60 + abs s / 3600)
    where
        d = fromIntegral deg
        m = fromIntegral min

sign_ :: (Double, Double, Double) -> Double
sign_ (d, m, s) =
    if elem (-1) $ signum <$> [d, m, s] then -1 else 1

signSymbolDMS :: DMS -> String
signSymbolDMS dms =
    if signDMS dms < 0 then "-" else ""

signDMS :: DMS -> Double
signDMS (DMS (deg, min, s)) =
    sign_ (d, m, s)
    where
        d = fromIntegral deg
        m = fromIntegral min

toQDeg :: DMS -> Quantity Double [u| deg |]
toQDeg =
    MkQuantity . toDeg

toQRad :: DMS -> Quantity Double [u| rad |]
toQRad =
    convert . toQDeg

-- |
-- >>> fromQ [u| 0.0 deg |]
-- 0°
--
-- >>> fromQ [u| 1.0 deg |]
-- 1°
--
-- >>> fromQ [u| -1.0 deg |]
-- -1°
--
-- >>> fromQ [u| 169.06666666622118 deg |]
-- 169°3'59.99999839625161''
--
-- >>> fromQ [u| -169.06666666622118 deg |]
-- -169°3'59.99999839625161''
fromQ :: Convertible u [u| deg |] => Quantity Double u -> DMS
fromQ q' =
    DMS (truncate s * dd, mm, mFrac * 60.0)
    where
        MkQuantity d = convert q' :: Quantity Double [u| deg |]

        s = signum d
        dAbs = abs d

        dd :: Int
        dd = floor dAbs

        dFrac :: Double
        dFrac = dAbs - fromIntegral dd

        (mm, mFrac) = divMod' (dFrac * 60.0) 1

-- |
-- >>> normalize (fromQuantity [u| 169.06666666622118 deg |] :: DMS)
-- 169°3'59.99999839625161''
--
-- >>> normalize (fromQuantity [u| 0.0 deg |] :: DMS)
-- 0°
--
-- >>> normalize (fromQuantity [u| 180.0 deg |] :: DMS)
-- 180°
--
-- >>> normalize (fromQuantity [u| 1.0 deg |] :: DMS)
-- 1°
--
-- >>> normalize (fromQuantity [u| -180.0 deg |] :: DMS)
-- 180°
--
-- >>> normalize (fromQuantity [u| -1.0 deg |] :: DMS)
-- 359°
--
-- >>> normalize (DMS (0, 0, 0))
-- 0°
--
-- >>> normalize (DMS (180, 0, 0))
-- 180°
--
-- >>> normalize (DMS (1, 0, 0))
-- 1°
--
-- >>> normalize (DMS (-180, 0, 0))
-- 180°
--
-- >>> normalize (DMS (-1, 0, 0))
-- 359°
--
-- >>> normalize (DMS (190,56,1.6037483874242753e-6))
-- 190°56'1.6037483874242753e-6''
--
-- >>> normalize ((toQuantity $ DMS (190,56,1.6037483874242753e-6)) :: Quantity Double [u| deg |])
-- [u| 190.93333333377882 deg |]
--
-- >>> fromQuantity [u| 190.93333333377882 deg |] :: DMS
-- 190°56'1.6037483874242753e-6''
--
-- >>> normalize (DMS (-190,56,1.603721102583222e-6))
-- 169°3'59.99999839625161''
--
-- >>> normalize ((toQuantity $ DMS (-190,56,1.603721102583222e-6)) :: Quantity Double [u| deg |])
-- [u| 169.06666666622118 deg |]
--
-- >>> plusMinusPi (DMS (0, 0, 0))
-- 0°
--
-- >>> plusMinusPi (DMS (90, 0, 0))
-- 90°
--
-- >>> plusMinusPi (DMS (-90, 0, 0))
-- -90°
--
-- >>> plusMinusPi (DMS (180, 0, 0))
-- 180°
--
-- >>> plusMinusPi (DMS (-180, 0, 0))
-- -180°
--
-- >>> plusMinusPi (DMS (181, 0, 0))
-- -179°
--
-- >>> plusMinusPi (DMS (-181, 0, 0))
-- 179°
--
-- >>> plusMinusHalfPi (DMS (91, 0, 0))
-- Nothing
--
-- >>> plusMinusHalfPi (DMS (-91, 0, 0))
-- Nothing
instance Angle DMS where
    normalize dms =
        fromQuantity n
        where
            n :: Quantity Double [u| deg |]
            n = MkQuantity $ d `mod'` 360.0

            (MkQuantity d) = toQuantity dms :: Quantity Double [u| deg |]

    plusMinusPi dms =
        fromQuantity n
        where
            n :: Quantity Double [u| deg |]
            n =
                MkQuantity $
                    case (divMod' d 180.0 :: (Integer, Double)) of
                        (a, 0.0) -> if even a then 0.0 else fromIntegral (signum a) * 180.0
                        (a, b) -> if even a then b else b - 180.0

            (MkQuantity d) = toQuantity dms :: Quantity Double [u| deg |]

    plusMinusHalfPi d =
        let x = plusMinusPi d in
        if | toQuantity x < [u| -90 deg |] -> Nothing
           | toQuantity x > [u|  90 deg |] -> Nothing
           | otherwise -> Just x

    rotate rotation dms =
        normalize . fromQuantity $ d +: r
        where
            r :: Quantity Double [u| deg |]
            r = toQuantity rotation

            d :: Quantity Double [u| deg |]
            d = toQuantity dms

    fromQuantity = fromQ
    toQuantity = convert . toQDeg

instance Angle DMS_ where
    normalize (DMS_ x) = let (DMS y) = normalize (DMS x) in DMS_ y
    plusMinusPi (DMS_ x) = let (DMS y) = plusMinusPi (DMS x) in DMS_ y
    plusMinusHalfPi (DMS_ x) = do DMS y <- plusMinusHalfPi (DMS x); return $ DMS_ y
    rotate (DMS_ r) (DMS_ x) = let (DMS y) = rotate (DMS r) (DMS x) in DMS_ y
    fromQuantity x = let DMS y = fromQ x in DMS_ y
    toQuantity (DMS_ x) = toQuantity (DMS x)

instance Ord DMS where
    x <= y = x' <= y'
        where
            x' :: Quantity Double [u| deg |]
            x' = toQuantity $ normalize x

            y' :: Quantity Double [u| deg |]
            y' = toQuantity $ normalize y

-- | The difference between two angles of DMS. The result is in the [0, 360)
--
-- >>> diffDMS (DMS (0,0,0)) (DMS (0,0,0))
-- 0°
--
-- >>> diffDMS (DMS (0,0,0)) (DMS (-0,0,0))
-- 0°
--
-- >>> diffDMS (DMS (0,0,0)) (DMS (360,0,0))
-- 0°
--
-- >>> diffDMS (DMS (0,0,0)) (DMS (-360,0,0))
-- 0°
--
-- >>> diffDMS (DMS (360,0,0)) (DMS (360,0,0))
-- 0°
--
-- >>> diffDMS (DMS (360,0,0)) (DMS (0,0,0))
-- 0°
--
-- >>> diffDMS (DMS (90,0,0)) (DMS (90,0,0))
-- 0°
--
-- >>> diffDMS (DMS (180,0,0)) (DMS (180,0,0))
-- 0°
--
-- >>> diffDMS (DMS (270,0,0)) (DMS (270,0,0))
-- 0°
--
-- >>> diffDMS (DMS (270,0,0)) (DMS (-90,0,0))
-- 0°
--
-- >>> diffDMS (DMS (-90,0,0)) (DMS (270,0,0))
-- 0°
--
--
-- >>> diffDMS (DMS (0,0,0)) (DMS (90,0,0))
-- 270°
--
-- >>> diffDMS (DMS (360,0,0)) (DMS (90,0,0))
-- 270°
--
-- >>> diffDMS (DMS (90,0,0)) (DMS (180,0,0))
-- 270°
--
-- >>> diffDMS (DMS (180,0,0)) (DMS (270,0,0))
-- 270°
--
-- >>> diffDMS (DMS (270,0,0)) (DMS (0,0,0))
-- 270°
--
-- >>> diffDMS (DMS (270,0,0)) (DMS (360,0,0))
-- 270°
--
--
-- >>> diffDMS (DMS (0,0,0)) (DMS (180,0,0))
-- 180°
--
-- >>> diffDMS (DMS (0,0,0)) (DMS (-180,0,0))
-- 180°
--
-- >>> diffDMS (DMS (360,0,0)) (DMS (180,0,0))
-- 180°
--
-- >>> diffDMS (DMS (90,0,0)) (DMS (270,0,0))
-- 180°
--
-- >>> diffDMS (DMS (90,0,0)) (DMS (-90,0,0))
-- 180°
--
-- >>> diffDMS (DMS (180,0,0)) (DMS (0,0,0))
-- 180°
--
-- >>> diffDMS (DMS (-180,0,0)) (DMS (0,0,0))
-- 180°
--
-- >>> diffDMS (DMS (180,0,0)) (DMS (360,0,0))
-- 180°
--
-- >>> diffDMS (DMS (270,0,0)) (DMS (90,0,0))
-- 180°
--
--
-- >>> diffDMS (DMS (0,0,0)) (DMS (270,0,0))
-- 90°
--
-- >>> diffDMS (DMS (360,0,0)) (DMS (270,0,0))
-- 90°
--
-- >>> diffDMS (DMS (90,0,0)) (DMS (0,0,0))
-- 90°
--
-- >>> diffDMS (DMS (90,0,0)) (DMS (360,0,0))
-- 90°
--
-- >>> diffDMS (DMS (180,0,0)) (DMS (90,0,0))
-- 90°
--
-- >>> diffDMS (DMS (270,0,0)) (DMS (180,0,0))
-- 90°
--
--
-- >>> diffDMS (DMS (95,27,59.63089)) (DMS (-95,28,0.3691116037646225))
-- 190°56'1.6037483874242753e-6''
--
-- >>> diffDMS (DMS (-95,28,0.3691116037646225)) (DMS (95,27,59.63089))
-- 169°3'59.99999839625161''
diffDMS :: DiffDMS
diffDMS y x =
    dyx
    where
        y' :: Quantity Double [u| deg |]
        y' = toQuantity y

        x' :: Quantity Double [u| deg |]
        x' = toQuantity x

        dyx' :: Quantity Double [u| deg |]
        dyx' = normalize $ y' -: x'

        dyx :: DMS
        dyx = normalize $ fromQuantity dyx'

-- | The absolute difference between two angles of DMS. The result is in the
-- [0, 180)
--
-- >>> absDiffDMS (DMS (0,0,0)) (DMS (0,0,0))
-- 0°
--
-- >>> absDiffDMS (DMS (0,0,0)) (DMS (-0,0,0))
-- 0°
--
-- >>> absDiffDMS (DMS (0,0,0)) (DMS (360,0,0))
-- 0°
--
-- >>> absDiffDMS (DMS (0,0,0)) (DMS (-360,0,0))
-- 0°
--
-- >>> absDiffDMS (DMS (360,0,0)) (DMS (360,0,0))
-- 0°
--
-- >>> absDiffDMS (DMS (360,0,0)) (DMS (0,0,0))
-- 0°
--
-- >>> absDiffDMS (DMS (90,0,0)) (DMS (90,0,0))
-- 0°
--
-- >>> absDiffDMS (DMS (180,0,0)) (DMS (180,0,0))
-- 0°
--
-- >>> absDiffDMS (DMS (270,0,0)) (DMS (270,0,0))
-- 0°
--
-- >>> absDiffDMS (DMS (270,0,0)) (DMS (-90,0,0))
-- 0°
--
-- >>> absDiffDMS (DMS (-90,0,0)) (DMS (270,0,0))
-- 0°
--
--
-- >>> absDiffDMS (DMS (0,0,0)) (DMS (90,0,0))
-- 90°
--
-- >>> absDiffDMS (DMS (360,0,0)) (DMS (90,0,0))
-- 90°
--
-- >>> absDiffDMS (DMS (90,0,0)) (DMS (180,0,0))
-- 90°
--
-- >>> absDiffDMS (DMS (180,0,0)) (DMS (270,0,0))
-- 90°
--
-- >>> absDiffDMS (DMS (270,0,0)) (DMS (0,0,0))
-- 90°
--
-- >>> absDiffDMS (DMS (270,0,0)) (DMS (360,0,0))
-- 90°
--
--
-- >>> absDiffDMS (DMS (0,0,0)) (DMS (180,0,0))
-- 180°
--
-- >>> absDiffDMS (DMS (0,0,0)) (DMS (-180,0,0))
-- 180°
--
-- >>> absDiffDMS (DMS (360,0,0)) (DMS (180,0,0))
-- 180°
--
-- >>> absDiffDMS (DMS (90,0,0)) (DMS (270,0,0))
-- 180°
--
-- >>> absDiffDMS (DMS (90,0,0)) (DMS (-90,0,0))
-- 180°
--
-- >>> absDiffDMS (DMS (180,0,0)) (DMS (0,0,0))
-- 180°
--
-- >>> absDiffDMS (DMS (-180,0,0)) (DMS (0,0,0))
-- 180°
--
-- >>> absDiffDMS (DMS (180,0,0)) (DMS (360,0,0))
-- 180°
--
-- >>> absDiffDMS (DMS (270,0,0)) (DMS (90,0,0))
-- 180°
--
--
-- >>> absDiffDMS (DMS (0,0,0)) (DMS (270,0,0))
-- 90°
--
-- >>> absDiffDMS (DMS (360,0,0)) (DMS (270,0,0))
-- 90°
--
-- >>> absDiffDMS (DMS (90,0,0)) (DMS (0,0,0))
-- 90°
--
-- >>> absDiffDMS (DMS (90,0,0)) (DMS (360,0,0))
-- 90°
--
-- >>> absDiffDMS (DMS (180,0,0)) (DMS (90,0,0))
-- 90°
--
-- >>> absDiffDMS (DMS (270,0,0)) (DMS (180,0,0))
-- 90°
--
--
-- >>> absDiffDMS (DMS (359,0,0)) (DMS (0,0,0))
-- 1°
--
-- >>> absDiffDMS (DMS (181,0,0)) (DMS (0,0,0))
-- 179°
--
-- >>> absDiffDMS (DMS (95,27,59.63089)) (DMS (-95,28,0.3691116037646225))
-- 169°3'59.99999839625161''
--
-- >>> absDiffDMS (DMS (-95,28,0.3691116037646225)) (DMS (95,27,59.63089))
-- 169°3'59.99999839625161''
absDiffDMS :: DiffDMS
absDiffDMS y x =
    let d = diffDMS y x
    in if d > DMS (180, 0, 0) then diffDMS (DMS (360, 0, 0)) d else d

-- | Some of the papers have test data that flip the reverse azimuth 180°. The
-- sign of the numerator and denominator vary in implementations of Vincenty's
-- inverse solution and the call to atan2 to get the reverse azimuth is
-- sensitive to this.
diffDMS180 :: DiffDMS
diffDMS180 y = diffDMS (rotate (DMS (180, 0, 0)) y)

absDiffDMS180 :: DiffDMS
absDiffDMS180 y = absDiffDMS (rotate (DMS (180, 0, 0)) y)

-- $ setup
-- >>> import Test.QuickCheck
