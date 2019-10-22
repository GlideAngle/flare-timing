module Flight.Units.DegMinSec

open System
open FSharp.Data.UnitSystems.SI.UnitSymbols
open Flight.Units.Angle
open Flight.Units.Convert

type DmsTuple = int<deg> * int<min> * float<s>

let sign' (x : float) : int = sign x |> function | 0 -> 1 | y -> y

/// If any of degree, minute or second are negative then -1 otherwise +1.
let signDMS (d : int<deg>, m : int<min>, s : float<s>) : int =
    if List.map sign [float d; float m; float s] |> List.contains (-1) then -1 else 1

let toDeg ((d : int<deg>, m : int<min>, s : float<s>) as dms) : float<deg> =
    let deg = float (abs d) * 1.0<deg>
    let min = float (abs m) * 1.0<min>
    let sec = abs s
    float (signDMS dms) * (deg + convertMinToDeg min + convertSecToDeg sec)

let toRad : DmsTuple -> float<rad> = toDeg >> convertDegToRad

let fromDeg (d : float<deg>) : DmsTuple =
    let dAbs : float<deg> = abs d
    let dd : int<deg> = int dAbs * 1<deg>
    let dFrac : float<deg> = dAbs - float dd * 1.0<deg>
    let minsFrac : float<min> = convertDegToMin dFrac
    let mins : float<min> = floor (float minsFrac) * 1.0<min>
    let secs : float<s> = convertMinToSec (minsFrac - mins)
    (sign' (float d) * dd, int mins * 1<min>, secs)

let fromRad : float<rad> -> DmsTuple = convertRadToDeg >> fromDeg

type DMS =
    | DMS of DmsTuple

    /// 0 <= a <= 2π
    static member Normalize (x : DMS) = x |> DMS.ToDeg |> (%) 360.0<deg> |> DMS.FromDeg

    /// -π <= a <= +π
    static member PlusMinusPi (x : DMS) = x
    /// -π/2 <= a <= +π/2
    static member PlusMinusHalfPi (x : DMS) = Some x

    static member Rotate (rotation : DMS) (x : DMS) = x

    static member ToRad (DMS dms) = toRad dms
    static member FromRad (r : float<rad>) = fromRad r |> DMS

    static member ToDeg (DMS dms) = toDeg dms
    static member FromDeg (d : float<deg>) = fromDeg d |> DMS

    member x.Sign : int =
        match x with
        | DMS dms -> signDMS dms

    override x.ToString() =
        let signSymbolDMS (dms : DMS) : String =
            if x.Sign < 0 then "-" else ""

        let secToShow (sec : float<s>) : String =
            let isec = Math.Floor (float sec)
            if isec = float sec
                // NOTE: Use String.Format instead of sprintf because it can do
                // roundtrip formatting of floating point.
                then String.Format("{0:R}", abs (int isec))
                else String.Format("{0:R}", abs sec)

        let secToShow' (sec : float<s>) : String =
            let isec = Math.Floor (float sec)
            if isec = float sec
                then String.Format("{0:R}", abs (int isec))
                else sprintf "%.6f" (abs sec)

        match x with
        | DMS (deg, 0<min>, 0.0<s>) -> string deg + "°"
        | DMS (0<deg>, 0<min>, sec) -> secToShow' sec + "''"
        | DMS (deg, min, 0.0<s>) as dms ->
            signSymbolDMS dms
            + string (abs deg)
            + "°"
            + string (abs min)
            + "'"
        | DMS (0<deg>, min, sec) as dms ->
            signSymbolDMS dms
            + string (abs min)
            + "'"
            + secToShow sec
            + "''"
        | DMS (deg, min, sec) as dms ->
            signSymbolDMS dms
            + string (abs deg)
            + "°"
            + string (abs min)
            + "'"
            + secToShow sec
            + "''"

and DiffDMS = DMS -> DMS -> DMS

module DegMinSecTests =
    open Xunit
    open Swensen.Unquote
    open Hedgehog

    [<Theory>]
    [<InlineData(0, 0, 0.0, 0.0)>]
    [<InlineData(1.0, 0, 0.0, 1.0)>]
    [<InlineData(289, 30, 0.0, 289.5)>]
    let ``convert dms to deg`` deg min sec deg' =
        test <@ DMS (deg, min, sec) |> DMS.ToDeg = deg' * 1.0<deg> @>

    [<Theory>]
    [<InlineData(0, 0, 0.0, 0.0)>]
    [<InlineData(289, 30, 0.0, 289.5)>]
    let ``(d, m, s) to deg`` d m s deg = test <@ toDeg (d, m, s) = deg @>

    [<Theory>]
    [<InlineData(0.0, 0, 0, 0.0)>]
    [<InlineData(1.0, 1, 0, 0.0)>]
    [<InlineData(289.5, 289, 30, 0.0)>]
    let ``from deg to (d, m, s)`` deg d m s = test <@ fromDeg deg = (d, m, s) @>

    [<Theory>]
    [<InlineData(0.0, "0°")>]
    [<InlineData(1.0, "1°")>]
    [<InlineData(-1.0, "-1°")>]
    [<InlineData(169.06666666622118, "169°3'59.99999839625161''")>]
    [<InlineData(-169.06666666622118, "-169°3'59.99999839625161''")>]
    let ``show from deg to dms`` deg s = test <@ DMS.FromDeg deg |> string = s @>

    [<Fact>]
    let ``deg via DMS`` () = Property.check <| property {
            let! d = Gen.int <| Range.constantBounded ()
            return (toDeg (d * 1<deg>, 0<min>, 0.0<s>)) = float d * 1.0<deg>
        }

    [<Fact>]
    let ``min via DMS`` () = Property.check <| property {
            let! m = Gen.int <| Range.constantBounded ()
            return (toDeg (0<deg>, m * 1<min>, 0.0<s>)) = convertMinToDeg (float m * 1.0<min>)
        }

    [<Fact>]
    let ``sec via DMS`` () = Property.check <| property {
            let! sec = Gen.double <| Range.constantBounded ()
            return (toDeg (0<deg>, 0<min>, sec * 1.0<s>)) = convertSecToDeg (sec * 1.0<s>)
        }

    [<Fact>]
    let ``deg min via DMS`` () = Property.check <| property {
            let! d = Gen.int <| Range.constantBounded ()
            let! m = Gen.int <| Range.constantBounded ()
            let dms = (d * 1<deg>, m * 1<min>, 0.0<s>)
            let plusMinus = float (signDMS dms)
            let deg = float (abs d) * 1.0<deg>
            let min = float (abs m) * 1.0<min>
            return (toDeg dms) = plusMinus * (deg + convertMinToDeg min)
        }

    [<Fact>]
    let ``deg sec via DMS`` () = Property.check <| property {
            let! d = Gen.int <| Range.constantBounded ()
            let! s = Gen.double <| Range.constantBounded ()
            let dms = (d * 1<deg>, 0<min>, s * 1.0<s>)
            let plusMinus = float (signDMS dms)
            let deg = float (abs d) * 1.0<deg>
            let sec = abs s * 1.0<s>
            return (toDeg dms) = plusMinus * (deg + convertSecToDeg sec)
        }

    [<Fact>]
    let ``min sec via DMS`` () = Property.check <| property {
            let! m = Gen.int <| Range.constantBounded ()
            let! s = Gen.double <| Range.constantBounded ()
            let dms = (0<deg>, m * 1<min>, s * 1.0<s>)
            let plusMinus = float (signDMS dms)
            let sec = abs s * 1.0<s>
            let min = float (abs m) * 1.0<min>
            return (toDeg dms) = plusMinus * (convertMinToDeg min + convertSecToDeg sec)
        }

    [<Fact>]
    let ``deg min sec via DMS`` () = Property.check <| property {
            let! d = Gen.int <| Range.constantBounded ()
            let! m = Gen.int <| Range.constantBounded ()
            let! s = Gen.double <| Range.constantBounded ()
            let dms = (d * 1<deg>, m * 1<min>, s * 1.0<s>)
            let deg = float (abs d) * 1.0<deg>
            let min = float (abs m) * 1.0<min>
            let sec = abs s * 1.0<s>
            let plusMinus = float (signDMS dms)
            return (toDeg dms) = plusMinus * (deg + convertMinToDeg min + convertSecToDeg sec)
        }