module Flight.Earth.Ellipsoid

open Flight.Units
open Flight.Zone

type Andoyer
    = AndoyerLambert
    | ForsytheAndoyerLambert
    | FsAndoyer

/// <param name="equatorialR">Equatorial radius or semi-major axis of the ellipsoid.</param>
/// <param name="recipF">Reciprocal of the flattening of the ellipsoid or 1/ƒ.</param>
type Ellipsoid = {equatorialR : Radius; recipF : float}

type AbnormalLatLng
    = LatUnder
    | LatOver
    | LngUnder
    | LngOver

/// <param name="GeodeticDirectAbnormal">Vincenty requires normalized latitude
/// and longitude inputs, in radians the equivalent of -90 <= latitude <= 90 and
/// -180 <= longitude <= 180 degrees.
/// </param>
/// <param name="GeodeticDirectEquatorial">Vincenty's solution to the direct
/// problem is indeterminate if the points are equatorial, checked for when abs λ
/// > π.
/// </param>
/// <param name="GeodeticDirectAntipodal">Vincenty's solution to the direct
/// problem is indeterminate if the points are antipodal, checked for when abs λ
/// > π.
/// </param>
/// <param name="GeodeticDirect">Vincenty's solution to the inverse problem.</param>
type GeodeticDirect<'a>
    = GeodeticDirectAbnormal of AbnormalLatLng
    | GeodeticDirectEquatorial
    | GeodeticDirectAntipodal
    | GeodeticDirect of 'a

/// <param name="GeodeticInverseAbnormal ">Vincenty requires normalized latitude
/// and longitude inputs, in radians the equivalent of -90 <= latitude <= 90 and
/// -180 <= longitude <= 180 degrees.
/// </param>
/// <param name="GeodeticInverseAntipodal">Vincenty's solution to the inverse
/// problem is indeterminate if the points are antipodal, checked for when abs λ
/// > π.
/// </param>
/// <param name="GeodeticInverse ">Vincenty's solution to the inverse
/// problem.
/// </param>
type GeodeticInverse<'a>
    = GeodeticInverseAbnormal of AbnormalLatLng
    | GeodeticInverseAntipodal
    | GeodeticInverse of 'a

// SEE: https://en.wikipedia.org/wiki/World_Geodetic_System
// https://en.wikipedia.org/wiki/World_Geodetic_System#A_new_World_Geodetic_System:_WGS_84
let wgs84 =
    { equatorialR = Radius 6378137.0<m>
    ; recipF = 298.257223563
    }

// As used by the National Geodetic Survey tool inverse when selecting the
// ellipsoid 1) GRS80 / WGS84 (NAD83) SEE:
// https://www.ngs.noaa.gov/PC_PROD/Inv_Fwd/
let nad83 = {wgs84 with recipF = 298.25722210088}

// The Bessel ellipsoid from Vincenty 1975. Note that the flattening from
// Wikipedia for the Bessel ellipsoid is 299.1528153513233 not 299.1528128. SEE:
// https://en.wikipedia.org/wiki/Bessel_ellipsoid
let bessel =
    { equatorialR = Radius 6377397.155<m>
    ; recipF = 299.1528128
    }

// The International ellipsoid 1924 also known as the Hayford ellipsoid from
// Vincenty 1975. SEE: https://en.wikipedia.org/wiki/Hayford_ellipsoid
let hayford =
    { equatorialR = Radius 6378388.0<m>
    ; recipF = 297.0
    }

// Clarke's 1866 ellipsoid approximated in metres. "Clarke actually defined his
// 1866 spheroid as a = 20,926,062 British feet, b = 20,855,121 British feet"
// SEE: https://en.wikipedia.org/wiki/North_American_Datum
let clarke =
    { equatorialR = Radius 6378206.4<m>
    ; recipF = 294.978698214
    }

// The ellipsoid used in Evaluation Direct and Inverse Geodetic Algorithms, by
// Paul Delorme, Bedford Institute of Oceanography, Dartmouth, Nova Scotia,
// Canada, 11978.
let bedfordClarke = {clarke with recipF = 294.9786986}