module Flight.Units.DegMinSec

open System

type DMS =
    | DMS of deg : int * min : int * sec : float
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

let toDeg ((DMS (deg, min, s)) as dms) : float =
    float dms.Sign * (float (abs deg) + float (abs min) / 60.0 + abs s / 3600.0)

module DegMinSecTests =
    open Xunit

    [<Fact>]
    let ``can convert dms to deg`` () =
        Assert.Equal(toDeg <| DMS (0, 0, 0.0), 1.0)
        Assert.Equal(toDeg <| DMS (289, 30, 0.0), 289.5)