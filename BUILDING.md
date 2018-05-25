# Building Flare Timing

This mono-repo contains many packages. The subset of these that are generally
useful are published to hackage and stackage.

## Building with Nix
Each library package can be built standalone. For example, the `flight-units`
package can be built with
[nix-build](https://nixos.org/nix/manual/#sec-building-simple) after having
setup the [overlay](https://github.com/BlockScope/nix-config).

    ln -s overlay.nix ~/.config/nixpkgs/overlay
    nix-build "<nixpkgs>" -A haskellPackages.flight-units

## Building with Stack

The project as a whole can be built with [stack](https://docs.haskellstack.org);

    flare-timing> stack build

Individual packages can be built by specifying either the folder or the package
name;

    flare-timing> stack build units
    flare-timing> stack build flight-units
    
## Building with Cabal

As we're depending on some git packages, draw these down using
[stack2cabal](https://github.com/brunjlar/stack2cabal);

    flare-timing> stack install stack2cabal
    flare-timing> stack exec stack2cabal -- .
    flare-timing> cabal new-build all
    
## Library Packages

For handling decimal places and significant digits;
* [`aeson-via-sci`](aeson-via-sci)
* [`aeson-via-uom`](aeson-via-uom)
* [`siggy-chardust`](siggy-chardust)

Units, latitudes, longitudes and distances on Earth;
* [`flight-units`](units)
* [`flight-earth`](earth)
* [`flight-latlng`](latlng)

Parsing input file formats;
* [`flight-fsdb`](fsdb)
* [`flight-igc`](igc)
* [`flight-kml`](kml)

Command line inputs;
* [`flight-cmd`](cmd)
* [`flight-span`](span)

The competition and scoring;
* [`flight-comp`](comp)
* [`flight-gap`](gap)
* [`flight-lookup`](lookup)
* [`flight-mask`](mask)
* [`flight-task`](task)
* [`flight-route`](route)

To do with files read from and written to during scoring;
* [`flight-scribe`](scribe)
* [`flight-track`](track)

Testing;
* [`tasty-compare`](tasty-compare)

The command line programs for scoring are in `./flare-timing`.
