module Flight.Units.DegMinSec

open System
open Flight.Units.Angle

let sign' d = sign d |> function | 0 -> 1 | s -> s

let toDeg (d : int, m : int, s : float) : float =
    let n = sign' (float d) * sign' (float m) * sign' s
    float n * (abs (float d) + abs (float m) / 60.0 + (abs s) / 3600.0)

let fromDeg (d : float) =
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

    static member ToRad (x : DMS) = 1.0<rad>
    static member FromRad (_ : float<rad>) = DMS (0, 0, 0.0)

    static member ToDeg (x : DMS) = 1.0<deg>
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

    let ToQuantity x =
        match x with
        | DMS (deg, min, s) as dms ->
            float dms.Sign * (float (abs deg) + float (abs min) / 60.0 + abs s / 3600.0) * 1.0<deg>
            |> degToRad

    [<Theory>]
    [<InlineData(0, 0, 0.0, 0.0)>]
    [<InlineData(1.0, 0, 0.0, 1.0)>]
    [<InlineData(289, 30, 0.0, 289.5)>]
    let ``convert dms to deg`` deg min sec deg' =
        test <@ DMS (deg, min, sec) |> ToQuantity |> radToDeg = deg' @>

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
            return (toDeg (d, 0, 0.0)) = float d
        }

    [<Fact>]
    let ``min via DMS`` () = Property.check <| property {
            let! m = Gen.int <| Range.constantBounded ()
            return (toDeg (0, m, 0.0)) = (float m) / 60.0
        }

    [<Fact>]
    let ``sec via DMS`` () = Property.check <| property {
            let! s = Gen.double <| Range.constantBounded ()
            return (toDeg (0, 0, s)) = s / 3600.0
        }

    [<Fact>]
    let ``deg min via DMS`` () = Property.check <| property {
            let! d = Gen.int <| Range.constantBounded ()
            let! m = Gen.int <| Range.constantBounded ()
            return (toDeg (d, m, 0.0)) = float d + (float m) / 60.0
        }

    [<Fact>]
    let ``deg sec via DMS`` () = Property.check <| property {
            let! d = Gen.int <| Range.constantBounded ()
            let! s = Gen.double <| Range.constantBounded ()
            return (toDeg (d, 0, s)) = float d + s / 3600.0
        }

    [<Fact>]
    let ``min sec via DMS`` () = Property.check <| property {
            let! m = Gen.int <| Range.constantBounded ()
            let! s = Gen.double <| Range.constantBounded ()
            return (toDeg (0, m, s)) = (float m) / 60.0 + s / 3600.0
        }

    [<Fact>]
    let ``deg min sec via DMS`` () = Property.check <| property {
            let! d = Gen.int <| Range.constantBounded ()
            let! m = Gen.int <| Range.constantBounded ()
            let! s = Gen.double <| Range.constantBounded ()
            return (toDeg (d, m, s)) = float d + (float m) / 60.0 + s / 3600.0
        }