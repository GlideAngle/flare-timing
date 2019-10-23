namespace Flight.Units

open System

// NOTE: The SI unit symbols s and m are already defined for us in
// FSharp.Data.UnitSystems.SI

[<Measure>]
type m = FSharp.Data.UnitSystems.SI.UnitSymbols.m

[<Measure>] type km
[<Measure>] type mm

/// hm is the hectometre, 100m.
[<Measure>] type hm

[<Measure>] type min
[<Measure>] type h
[<Measure>] type d

[<Measure>] type ft
[<Measure>] type mi

[<Measure>] type mph = mi / h
