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

let fromDeg (d : float) : int * int * float =
    let dAbs = abs d
    let dd = int dAbs
    let dFrac = dAbs - float dd
    let minsFrac = dFrac * 60.0
    let mins = int (floor minsFrac)
    let secs = (minsFrac - float mins) * 60.0
    (sign' d * dd, int mins, secs)

type DMS =
    | DMS of deg : int * min : int * sec : float

    /// 0 <= a <= 2π
    static member Normalize (x : DMS) = x |> DMS.ToDeg |> (%) 360.0<deg> |> DMS.FromDeg

    /// -π <= a <= +π
    static member PlusMinusPi (x : DMS) = x
    /// -π/2 <= a <= +π/2
    static member PlusMinusHalfPi (x : DMS) = Some x

    static member Rotate (rotation : DMS) (x : DMS) = x

    static member ToRad (DMS (d, m, s)) = toRad (d * 1<deg>, m * 1<min>, s * 1.0<s>) 
    static member FromRad (_ : float<rad>) = DMS (0, 0, 0.0)

    static member ToDeg (DMS (d, m, s)) = toDeg (d * 1<deg>, m * 1<min>, s * 1.0<s>)
    static member FromDeg (d : float<deg>) = fromDeg (float d) |> DMS

    member x.Sign : int =
        match x with
        | DMS (deg, min, s) ->
            if List.map sign [float deg; float min; s] |> List.contains (-1) then -1 else 1

    override x.ToString() =
        let signSymbolDMS (dms : DMS) : String =
            if x.Sign < 0 then "-" else ""

        let secToShow (sec : float) : String =
            let isec = Math.Floor sec
            if isec = sec
                then sprintf "%A" (abs (int isec))
                else sprintf "%A" (abs sec)

        let secToShow' (sec : float) : String =
            let isec = Math.Floor sec
            if isec = sec
                then sprintf "%A" (Math.Abs (int isec))
                else sprintf "%.6f" (Math.Abs sec)

        match x with
        | DMS (deg, 0, 0.0) -> string deg + "°"
        | DMS (0, 0, sec) -> secToShow' sec + "''"
        | DMS (deg, min, 0.0) as dms ->
            signSymbolDMS dms
            + string (abs deg)
            + "°"
            + string (abs min)
            + "'"
        | DMS (0, min, sec) as dms ->
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