module TestHaversine

open Xunit
open Hedgehog

open Flight.Units
open Flight.Units.Angle
open Flight.Units.Convert
open Flight.Haversine

[<Fact>]
let ``haversines is not negative`` () = Property.check <| property {
        let! x = Gen.double <| Range.constantBounded ()
        return haversine (x * 1.0<rad>) >= 0.0<rad>
    }
