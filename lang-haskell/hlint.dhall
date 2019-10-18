    let Prelude/List/map =
          https://raw.githubusercontent.com/dhall-lang/Prelude/35deff0d41f2bf86c42089c6ca16665537f54d75/List/map

in  let defs = ./default-extensions.dhall

in  let f = λ(s : Text) → "-X" ++ s

in  { arguments = Prelude/List/map Text Text f defs.default-extensions }
