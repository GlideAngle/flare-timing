module Flight.Geodesy.Solution

open Flight.Units
open Flight.Units.Angle
open Flight.LatLng
open Flight.Zone
open Flight.Earth.Ellipsoid
open Flight.Geodesy.Problem

type IGeodesySolutions =
    abstract member AzimuthFwd<'g> : 'g -> AzimuthFwd
    abstract member AzimuthRev<'g> : 'g -> AzimuthRev
    abstract member ArcLength<'g> : 'g -> SpanLatLng

    abstract member Inverse<'g>
        : InverseProblem<LatLng>
        -> GeodeticInverse<InverseSolution<TaskDistance, float<rad>>>

    abstract member Direct<'g>
        : DirectProblem<LatLng, TrueCourse, Radius>
        -> GeodeticDirect<DirectSolution<LatLng, TrueCourse>>