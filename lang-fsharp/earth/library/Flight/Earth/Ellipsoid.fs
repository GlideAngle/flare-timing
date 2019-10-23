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