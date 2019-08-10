{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}

module Flight.Units.DegMinSec
    ( DMS(..)
    , DMS_(..)
    , DiffDMS
    , diffDMS
    , diffDMS180
    , toDeg
    , toQDeg
    , toQRad
    , fromQ
    ) where

import Prelude hiding (min)
import Data.Fixed (mod')
import Data.Text.Lazy (unpack)
import Formatting (format)
import Text.Printf (printf)
import qualified Formatting.ShortFormatters as Fmt (sf)
import Data.UnitsOfMeasure ((+:), (-:), u, convert)
import Data.UnitsOfMeasure.Internal (Quantity(..))
import Data.UnitsOfMeasure.Convert (Convertible)

import Flight.Units.Angle (Angle(..))

type DiffDMS = DMS -> DMS -> DMS

newtype DMS = DMS (Int, Int, Double) deriving Eq

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
toDeg :: DMS -> Double
toDeg dms@(DMS (deg, min, s)) =
    signDMS dms * (abs d + abs m / 60 + abs s / 3600)
    where
        d = fromIntegral deg
        m = fromIntegral min

signSymbolDMS :: DMS -> String
signSymbolDMS dms =
    if signDMS dms < 0 then "-" else ""

signDMS :: DMS -> Double
signDMS (DMS (deg, min, s)) =
    if elem (-1) $ signum <$> [d, m, s] then -1 else 1
    where
        d = fromIntegral deg
        m = fromIntegral min 

toQDeg :: DMS -> Quantity Double [u| deg |]
toQDeg =
    MkQuantity . toDeg

toQRad :: DMS -> Quantity Double [u| rad |]
toQRad =
    convert . toQDeg

fromQ :: Convertible u [u| deg |] => Quantity Double u -> DMS
fromQ q' =
    DMS (s * dd, mm, ss)
    where
        MkQuantity d = convert q' :: Quantity Double [u| deg |]

        totalSecs :: Int
        totalSecs = round $ 3600.0 * d

        s = signum totalSecs

        (dd, ms) = quotRem (abs totalSecs) 3600
        mm = quot ms 60

        ss =
            (abs d - fromIntegral dd) * 3600.0
            - fromIntegral (mm * 60)

instance Angle DMS where
    normalize dms =
        fromQuantity n
        where
            n :: Quantity Double [u| deg |]
            n = MkQuantity $ d `mod'` 360.0

            (MkQuantity d) = toQuantity dms :: Quantity Double [u| deg |]

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

-- |
-- >>> diffDMS (DMS (0,0,0)) (DMS (0,0,0))
-- 0°
--
-- >>> diffDMS (DMS (0,0,0)) (DMS (360,0,0))
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
-- >>> diffDMS (DMS (360,0,0)) (DMS (180,0,0))
-- 180°
--
-- >>> diffDMS (DMS (90,0,0)) (DMS (270,0,0))
-- 180°
--
-- >>> diffDMS (DMS (180,0,0)) (DMS (0,0,0))
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

-- | Some of the papers have test data that flip the reverse azimuth 180°. The
-- sign of the numerator and denominator vary in implementations of Vincenty's
-- inverse solution and the call to atan2 to get the reverse azimuth is
-- sensitive to this.
diffDMS180 :: DiffDMS
diffDMS180 x y = diffDMS180 (rotate (DMS (180, 0, 0)) x) y

