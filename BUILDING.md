# Building Flare Timing

This mono-repo contains many packages. The subset of these that are generally
useful are published to hackage and stackage. Others that

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
    
The following library packages are included in this repository;

* [`aeson-via-sci`](aeson-via-sci)
* [`aeson-via-uom`](aeson-via-uom)
* [`flight-cmd`](cmd)
* [`flight-comp`](comp)
* [`flight-earth`](earth)
* [`flight-fsdb`](fsdb)
* [`flight-gap`](gap)
* [`flight-igc`](igc)
* [`flight-kml`](kml)
* [`flight-latlng`](latlng)
* [`flight-lookup`](lookup)
* [`flight-mask`](mask)
* [`flight-route`](route)
* [`flight-scribe`](scribe)
* [`siggy-chardust`](siggy-chardust)
* [`flight-span`](span)
* [`flight-task`](task)
* [`tasty-compare`](tasty-compare)
* [`flight-track`](track)
* [`flight-units`](units)

the command line programs for scoring are in `./flare-timing`.
