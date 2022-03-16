# Stale Build Methods

At some point in time these worked but are neglected.

## Building with Cabal

As we're depending on some git packages, draw these down using
[stack2cabal](https://github.com/brunjlar/stack2cabal);

    flare-timing> stack install stack2cabal
    flare-timing> stack exec stack2cabal -- .
    ...
    flare-timing> cabal new-build all
    cabal: Could not resolve dependencies:
    [__0] trying: aeson-via-sci-0.1.0 (user goal)
    [__1] trying: template-haskell-2.13.0.0/installed-2.1... (dependency of
    aeson-via-sci)
    [__2] next goal: uom-plugin (user goal)
    [__2] rejecting: uom-plugin-0.2.0.1 (conflict:
    template-haskell==2.13.0.0/installed-2.1..., uom-plugin =>
    template-haskell>=2.9 && <2.13)
    [__2] rejecting: uom-plugin-0.2.0.0, uom-plugin-0.1.1.0, uom-plugin-0.1.0.0
    (constraint from user target requires ==0.2.0.1)
    After searching the rest of the dependency tree exhaustively, these were the
    goals I've had most trouble fulfilling: template-haskell, uom-plugin,
    aeson-via-sci

If the build is working with `stack` then we might be able to get it to work
with `cabal`;

    > mv cabal.project __cabal.project
    > stack exec stack2cabal -- .
    > mv cabal.project cabal.project.local
    > mv __cabal.project cabal.project

Edit `cabal.project.local` so that it looks something like;

    with-compiler:
        /Users/.../.stack/programs/x86_64-osx/ghc-8.2.2/bin/ghc
    constraints:
        Cabal == 2.0.1.1
        , Only == 0.1
        , QuickCheck == 2.10.1
        ...
        , void == 0.7.2
        , yaml == 0.8.29

Now let's do the build again;

    > cabal new-build all
    Build profile: -w ghc-8.2.2 -O1
    In order, the following will be built (use -v for more details):
    ...
    > cabal new-build all
    Up to date

## Building with Nix
Wherever there's a `.cabal` file, there's a matching `default.nix` that enables
a [nix build](https://nixos.org/nix/manual/#sec-building-simple). There's
a build target to update the `*.nix` file for each package:

    > ./stack-shake-build.sh cabal2nix
    # cabal2nix (for cabal2nix-zone)
    # cabal2nix (for cabal2nix-units)
    # cabal2nix (for cabal2nix-track)
    # cabal2nix (for cabal2nix-time)
    # cabal2nix (for cabal2nix-task)
    # cabal2nix (for cabal2nix-span)
    # cabal2nix (for cabal2nix-scribe)
    # cabal2nix (for cabal2nix-route)
    # cabal2nix (for cabal2nix-mask)
    # cabal2nix (for cabal2nix-lookup)
    # cabal2nix (for cabal2nix-latlng)
    # cabal2nix (for cabal2nix-kml)
    # cabal2nix (for cabal2nix-igc)
    # cabal2nix (for cabal2nix-gap)
    # cabal2nix (for cabal2nix-fsdb)
    # cabal2nix (for cabal2nix-earth)
    # cabal2nix (for cabal2nix-comp)
    # cabal2nix (for cabal2nix-cmd)
    # cabal2nix (for cabal2nix-clip)
    # cabal2nix (for cabal2nix-app-serve)
    # cabal2nix (for cabal2nix-flare-timing)
    # cabal2nix (for cabal2nix-tasty-compare)
    # cabal2nix (for cabal2nix-siggy-chardust)
    # cabal2nix (for cabal2nix-detour-via-uom)
    # cabal2nix (for cabal2nix-detour-via-sci)
    Build completed in 0:01m

Building the [`flight-units`](units) library, the command line apps and web
server with nix;

    > cd units
    units> nix build
    [2 built, 0.0 MiB DL]
    > cd ../flare-timing
    flare-timing> nix build
    [17 built, 0.0 MiB DL]
    cd ../www
    www> nix build
    [1 built, 0.0 MiB DL]

The results of those builds;

    > ls units/result/lib/ghc-8.2.2/
    flight-units-0.1.0   package.conf.d       x86_64-osx-ghc-8.2.2
    > ls flare-timing/result/bin
    align-time       extract-input    land-out         task-length      test-kml-parser
    cross-zone       fs-score         mask-track       test-fsdb-parser unpack-track
    discard-further  gap-point        tag-zone         test-igc-parser
    > ls app-serve/result/bin
    comp-serve

## Building within Nix with Cabal
Each package has a `shell.nix` that picks up the pinned nixpkgs and package
overrides before opening a nix shell from which we can build with
`cabal new-build`;

    > cd units
    > nix-shell
    [nix-shell:~/.../units]$ cabal new-build
    Resolving dependencies...
    Build profile: -w ghc-8.2.2 -O1
    In order, the following will be built (use -v for more details):
     - StateVar-1.1.1.0 (lib) (requires build)
     - distributive-0.5.3 (lib:distributive) (requires build)
     - uom-plugin-0.3.0.0 (lib) (requires build)
     - scientific-0.3.6.2 (lib) (requires build)
     - siggy-chardust-1.0.0 (lib) (configuration changed)
     - contravariant-1.4.1 (lib:contravariant) (requires build)
     - formatting-6.3.6 (lib) (requires build)
     - comonad-5.0.4 (lib:comonad) (requires build)
     - bifunctors-5.5.3 (lib) (requires build)
     - flight-units-0.1.0 (lib) (first run)
     ...
     [nix-shell:~/.../units]$ cabal new-build
     Up to date

## Building with Nix from Stack

There's two ways to generate a nix build from stack's configuration with
[stackage2nix](https://github.com/typeable/stackage2nix). One method
uses an nix overlay but the alternative method is shown here;

```
> git clone https://github.com/commercialhaskell/all-cabal-hashes.git
> cd all-cabal-hashes/
> git branch
* display
> git checkout hackage
> cd ..
> git clone https://github.com/commercialhaskell/lts-haskell.git
> cd flare-timing
> stack install stackage2nix --stack-yaml=stack-stackage2nix.yaml
> cd stackage2nix
stackage2nix>../__shake-build/stackage2nix ../stack.yaml \
--all-cabal-hashes=../../all-cabal-hashes \
--lts-haskell=../../lts-haskell \
--with-stackage-closure
> nix build
```

This produces a lot of `result` links;

```
stackage2nix> ls -1 | sort --version-sort
...
result
result-1
...
result-289
result-290

stackage2nix> tree --sort=version
.
├── configuration-packages.nix
├── default.nix
├── packages.nix
├── result -> /nix/store/w10zkvrhvcpfc1f633dwlbvfyi5nhacq-Cabal-2.0.1.1
├── result-1 -> /nix/store/7x245ixigbrfh43lf35s1x3nafjknkb9-ChasingBottoms-1.3.1.4
├── result-2 -> /nix/store/vdcnjm1ccf68yqdj17yn6aaagdv4x5pn-Glob-0.9.2
├── result-3 -> /nix/store/8spnbpigby8d1l9c27q0nbv15c72x0ha-HTTP-4000.3.12
├── result-4 -> /nix/store/sk61h49f8hf85vvxgz1n6d71qv7fd9ff-HUnit-1.6.0.0
├── result-5 -> /nix/store/fk9pphl59ca7xhpq6firrzwzjn15fmzg-Only-0.1
├── result-6 -> /nix/store/69z7r2n0w29yxqjn0r2qpaniwb3vkr32-PSQueue-1.1
├── result-7 -> /nix/store/k3dv2mlw1scnc2qx6cycpbxrk2dbvs2w-QuickCheck-2.10.1
├── result-8 -> /nix/store/ns7bmij5m20nq0r03gafxddpzrm9p7il-SHA-1.6.4.4
├── result-9 -> /nix/store/7kwignpgm0g9rifv1h5yi1x0qzv7iysf-StateVar-1.1.1.0
├── result-10 -> /nix/store/bkkhgpyqdcb8n8gzprmqr2wwcnmq8iwd-abstract-deque-0.3
├── result-11 -> /nix/store/i0d3qxhramwkgbcpq2alk4r504lqsyxi-abstract-par-0.3.3
├── result-12 -> /nix/store/qygdpv7qjb7q6bk9b9ffa4qi9jwz0jhf-adjunctions-4.4
├── result-13 -> /nix/store/p67668glmr0wm9d7whvi72kazjfralsy-aeson-1.2.4.0
├── result-14 -> /nix/store/wb90hs469h7v8720h75anapmq3y5iyaf-aeson-compat-0.3.8
├── result-15 -> /nix/store/0djcnb2wz9dx21nh2mq4z402hn01fpmf-aeson-pretty-0.8.7
├── result-16 -> /nix/store/a264pqrby27qf6lrdq35g7i41a0a33cy-ansi-terminal-0.8.0.4
├── result-17 -> /nix/store/3lb4b7r1py2pk6a101ldcv6607kcvw2n-ansi-wl-pprint-0.6.8.2
├── result-18 -> /nix/store/9z0s0hcabdqvdcgs1zfxhsw05kp9xk9j-appar-0.1.4
├── result-19 -> /nix/store/0bq7cc44kslxbjag68vp8bhnlqm5yhfi-async-2.1.1.1
├── result-20 -> /nix/store/34sg6nsxr34b9fblk2drqw5hpnz7f254-attoparsec-0.13.2.2
├── result-21 -> /nix/store/1cz12z11rdmrb99cvlfhh07xgqd1cbdj-attoparsec-iso8601-1.0.0.0
├── result-22 -> /nix/store/r6yx4j62ab05jv06xc3qxvqrraf2miyk-auto-update-0.1.4
├── result-23 -> /nix/store/fc3ckiafd24v8yfxnyvk87bw1qh85x5q-base-compat-0.9.3
├── result-24 -> /nix/store/6s9594x8lknbgfrak6lqxlvg8740d7rp-base-orphans-0.7
├── result-25 -> /nix/store/37agqq8pdx2wqi4cphgwixwawysyzfz5-base-prelude-1.2.1
├── result-26 -> /nix/store/v1x7ifii8g241q0hx46mczy6m9i54lz0-base-unicode-symbols-0.2.2.4
├── result-27 -> /nix/store/dy7jsn3i0q4xy1690y04nvgkznahsaff-base64-bytestring-1.0.0.1
├── result-28 -> /nix/store/mbgirmhmcl1c8zv27lf047k29irv3qw4-basement-0.0.8
├── result-29 -> /nix/store/qrrvyhbh8yrksng67i5nlqr8sg583asp-bifunctors-5.5.3
├── result-30 -> /nix/store/3gp5h36hhaa68fzjc4l7nqhcqrjqaavq-blaze-builder-0.4.1.0
├── result-31 -> /nix/store/isvzx61jx30rakhywxp90qlrh0j1gpdi-blaze-html-0.9.1.1
├── result-32 -> /nix/store/di17wl3iqjaqrd6npp80zi4qd4wzqldr-blaze-markup-0.8.2.1
├── result-33 -> /nix/store/q04i09449aphlgr9ifa88bpbb8067326-bsb-http-chunked-0.0.0.2
├── result-34 -> /nix/store/5lhvm9grgryrll45n5ar012fiv52hcjy-build-flare-timing-0.1.0
├── result-35 -> /nix/store/iqbzkpqhd22wdhgs70zjb5b7v893pnkq-byteorder-1.0.4
├── result-36 -> /nix/store/p09hg51x8q28c8c6dyv5s7avhc8rbm2f-bytestring-builder-0.10.8.1.0
├── result-37 -> /nix/store/inb5i5jg22yj5v5bwz0m743d8ykxnsmb-cabal-doctest-1.0.6
├── result-38 -> /nix/store/n5w307hn5cirjal69v1qfm1b1x1z27c7-call-stack-0.1.0
├── result-39 -> /nix/store/61cwy96ybwh39hj425qcwcbwh2jjnsv3-case-insensitive-1.2.0.11
├── result-40 -> /nix/store/jvi16gvhizdqy41k5jf09zh4xinyp03p-cassava-0.5.1.0
├── result-41 -> /nix/store/n992izw58b16bgra41i8df01n2qzgb0f-cereal-0.5.5.0
├── result-42 -> /nix/store/qv9x76w6pxcpmb0kdj24fv11lsd7ifsm-clock-0.7.2
├── result-43 -> /nix/store/vl2b51a7sy6c66nvds654wfx8vpx1px3-cmdargs-0.10.20
├── result-44 -> /nix/store/147x5xwrid7gv71qg2mjqxv1cca0ldwh-code-page-0.1.3
├── result-45 -> /nix/store/fb5vbp7fw0vhk3id0pnsi79124i4xv88-colour-2.3.4
├── result-46 -> /nix/store/i6nrgrjrbbhxmkr9i6aqy3avnjcm12ip-comonad-5.0.4
├── result-47 -> /nix/store/pax71rcmhpqrzldwnwyi9lkyp86q8y5y-concurrent-output-1.10.6
├── result-48 -> /nix/store/bl4lj13m0wyji6rixli7msd7x8zkxb5q-conduit-1.3.0.3
├── result-49 -> /nix/store/imh48w97hnj6l976ll39fm16qd23y68a-conduit-extra-1.3.0
├── result-50 -> /nix/store/h9rhd4ilci0xs1343ypnvc7llmncka0v-constraints-0.10.1
├── result-51 -> /nix/store/aiwhvvn0yl4m9i2rh2l2khc2zk8zaj6r-contravariant-1.4.1
├── result-52 -> /nix/store/qccnyk30kkiphrhi66bqfbd0z7alwfc6-cookie-0.4.4
├── result-53 -> /nix/store/0y0jabgss0mz2c0bfcyhamvar35vizx4-cpphs-1.20.8
├── result-54 -> /nix/store/nwz9vavw9rkdadsim81m9937mbz468a2-cpu-0.1.2
├── result-55 -> /nix/store/3iswci8fqm0sr1p9amyfs6j8ymkpgds9-criterion-1.3.0.0
├── result-56 -> /nix/store/0bkq99aiag43ldpgy6i38n16jicgwr0f-cryptonite-0.25
├── result-57 -> /nix/store/1asfclm9csz9cl3fmxgzjni72x69447c-data-default-0.7.1.1
├── result-58 -> /nix/store/rdxdz5j6qcbyx1hsfil4ga2kn9cbv3jy-data-default-class-0.1.2.0
├── result-59 -> /nix/store/g9azfnykzd32jh9c99nf7bmn2pppc3b7-data-default-instances-containers-0.0.1
├── result-60 -> /nix/store/kb40sxd9nx9ccm1l76hwym9chgzva81a-data-default-instances-dlist-0.0.1
├── result-61 -> /nix/store/f7n2jgipqki04zvhcxbbmzqfb62dxyxg-data-default-instances-old-locale-0.0.1
├── result-62 -> /nix/store/aiqc4h5l9rfijbk320nvy9np8ndkdmd0-deepseq-generics-0.2.0.0
├── result-63 -> /nix/store/f94c6f0f493rxrvkljyh76ani2d7dpcw-detour-via-sci-1.0.0
├── result-64 -> /nix/store/8bsi5mr649yd4jwfmxakakmnm6kafqja-detour-via-uom-1.0.0
├── result-65 -> /nix/store/prmnz23r0yzv22r90x4v2drspb1j5p8r-distributive-0.5.3
├── result-66 -> /nix/store/11b4hadyl37zzqlzsralx6xnqdld86mr-dlist-0.8.0.4
├── result-67 -> /nix/store/n1m5ckw0mznywjrdrzcm6hrgsfbk97py-doctest-0.15.0
├── result-68 -> /nix/store/5dvlhjn1b1b8z1j6fr4vz7zqh0zikvp8-easy-file-0.2.2
├── result-69 -> /nix/store/lix3456wafdlj10pdyj6jihw8wiw7v55-entropy-0.3.8
├── result-70 -> /nix/store/sqp4iybfi090hrkwlzh1zrqmrmr4h8lm-erf-2.0.0.0
├── result-71 -> /nix/store/qpzihn376j5k4xi3gg2b1vy3f6c66w1h-exceptions-0.8.3
├── result-72 -> /nix/store/nsf4cnimy50f8ll50k8xa75ld43pd5ga-extensible-exceptions-0.1.1.4
├── result-73 -> /nix/store/qmz4yqmka42z0lw0pf291mfjgf5pcpal-extra-1.6.9
├── result-74 -> /nix/store/f2f1wc28jhgysymaaiwljzf4qbg4j7bz-fail-4.9.0.0
├── result-75 -> /nix/store/fgp8nisli11x648xd6xsxi56ivykpqyd-fast-logger-2.4.11
├── result-76 -> /nix/store/kq22pz5da90dq4sfmvggn4s33dbdbgjs-fgl-5.6.0.0
├── result-77 -> /nix/store/ajr3rr6fki04sh0fq0ncf2r0dzc8l8bs-file-embed-0.0.10.1
├── result-78 -> /nix/store/zd1fsrgwjbnaq4jr8p0jkwd0znvmvkf9-filemanip-0.3.6.3
├── result-79 -> /nix/store/h72zyy3grb7wn3f45y33a8bkbd95m4wx-fingertree-0.1.4.1
├── result-80 -> /nix/store/knbyvh8ipx3h8qdjg6afv132n614wilf-fingertree-psqueue-0.3
├── result-81 -> /nix/store/cs0ify8g197vw28vs10w31c5jih2k6xb-fixed-0.2.1.1
├── result-82 -> /nix/store/r5iw2hz1k6m78vgr08k9y2v8832n07wy-flare-timing-0.1.0
├── result-83 -> /nix/store/7qd3ipncv37xzlhfwnhfqlvzq949s3vz-flight-cmd-0.1.0
├── result-84 -> /nix/store/kzd7229sd5d2pkjwp5fmh08bb530ma2g-flight-comp-0.1.0
├── result-85 -> /nix/store/h220ix8zshqr39nsvlwbbvm1d0zi6b5n-flight-earth-0.1.0
├── result-86 -> /nix/store/9vxq9bjlxia5p1g71qpqm7bndi96r36l-flight-fsdb-0.1.0
├── result-87 -> /nix/store/c6wk3gnz1m9ci89nnkvw9r7mmfcffxgn-flight-gap-0.1.0
├── result-88 -> /nix/store/ajz2i8srgwl2nkd0vfil0k84xm22bkmw-flight-igc-1.0.0
├── result-89 -> /nix/store/lh6pp0hhls9gs01wdp33d6a5f103ja4l-flight-kml-1.0.1
├── result-90 -> /nix/store/74jpqrriawf9qhk56jlvjz0982hvc4k8-flight-latlng-0.1.0
├── result-91 -> /nix/store/alz1i7iqgqylv32fzm3zlgr0glxy4j7v-flight-lookup-0.1.0
├── result-92 -> /nix/store/896iipcw3wm6qm2m6z1jxpwask2q6fy1-flight-mask-0.1.0
├── result-93 -> /nix/store/mfc9d8qjqa2dpdc27z5f57smdq1irpnm-flight-route-0.1.0
├── result-94 -> /nix/store/qmdfwri4lilqavvq5qil7r4dyxn6akdf-flight-scribe-0.1.0
├── result-95 -> /nix/store/mkw2d3a95yn62wg8kgmnli8nbzj7q1i2-flight-span-0.1.0
├── result-96 -> /nix/store/i1sqnzjabwfxzc2dig39a7r7w4r2ka1k-flight-task-0.1.0
├── result-97 -> /nix/store/ddx6kmb8cx3xl0vgf0w0hbdb42sbgb68-flight-track-0.1.0
├── result-98 -> /nix/store/635ia6w87j1zp4krx2v9h584v0admsdr-flight-units-0.1.0
├── result-99 -> /nix/store/c59wdhsjllc2fk0dng9c44h2j6hmhjl8-flight-zone-0.1.0
├── result-100 -> /nix/store/k0sv5vcnsrvy2lq8pbczxflqvrqgbplv-foldl-1.3.7
├── result-101 -> /nix/store/lvrjgadvxgm67w21m64dmx9y3qqb30m5-formatting-6.3.6
├── result-102 -> /nix/store/nwgm7y0f7z5i9p064cspm9vpyz2rnq6s-foundation-0.0.21
├── result-103 -> /nix/store/habi7cb4i2j1lj1nr9mb6qmh3j20x8k5-free-5.0.2
├── result-104 -> /nix/store/i0bm60mlj5fylaq7arf1pkkk2688d4nd-gauge-0.2.3
├── result-105 -> /nix/store/vc5hmmqzhs6321b6hvln1bpbk93scq4x-generic-deriving-1.12.2
├── result-106 -> /nix/store/shwidlbbmhb5fk9cwz566vpznyj6gd3g-generics-sop-0.3.2.0
├── result-107 -> /nix/store/311z8b0v8jqizb10jdi4y8pkgrw3g0a9-ghc-8.2.2
├── result-108 -> /nix/store/fy7j6717k94n0fji56c1122mfqlqg900-ghc-paths-0.1.0.9
├── result-109 -> /nix/store/adfc4xz8yh6yl1i5nzd5l4v7mxkglzx5-ghc-tcplugins-extra-0.2.5
├── result-110 -> /nix/store/k6pskskq4yv0flbkqx7vxq0xwa14capn-happy-1.19.9
├── result-111 -> /nix/store/1w24ylw6jhic20rlzv1l0z4s8bmgzag3-hashable-1.2.7.0
├── result-112 -> /nix/store/bb4qw9c4221ksmjm4b6d7clhg7sxch22-hashtables-1.2.3.1
├── result-113 -> /nix/store/hj44scs2m5vm29xnvzvbaphfrqqfl44l-haskell-lexer-1.0.1
├── result-114 -> /nix/store/5b5nmhyakdwrllzxzh78i9fn6llkbnjl-haskell-src-exts-1.20.2
├── result-115 -> /nix/store/jvxrmsa6d3f8i04p49vzd5mj52p9ljqc-haskell-src-exts-util-0.2.3
├── result-116 -> /nix/store/2qdmr021l28h95nb4agl37mvik7ynpwr-haskell-src-meta-0.8.0.3
├── result-117 -> /nix/store/zx3rwhk0jsj6wz5jlnzzrznp0qai5wwk-hcoord-2.1.0
├── result-118 -> /nix/store/0nfq0abjhx99y9dymjmxqjz8ljggbimq-hcoord-utm-2.1.0
├── result-119 -> /nix/store/cqmd1p3zpyfqsqg0yaah0bm1h5ygqmy4-heaps-0.3.6
├── result-120 -> /nix/store/93mspmi7ir4ymill88lbvlfci5mhawz0-hedgehog-0.5.3
├── result-121 -> /nix/store/53d7a99fgcn4bj68l5g6sn7rjbz8jh6i-here-1.2.13
├── result-122 -> /nix/store/rap6sxizaz74rpi1ac0l4swh94r4xyg8-hex-0.1.2
├── result-123 -> /nix/store/laflfapxwlrpkjxl8b8y83z10f1dww94-hlint-2.1.8
├── result-124 -> /nix/store/sdghsq84zf0j73a30pi8xvlfliq4zl2y-hostname-1.0
├── result-125 -> /nix/store/iqxb92n62l6v42y95kdncqpd2xvn3sv9-hscolour-1.24.4
├── result-126 -> /nix/store/96v7ckxxs0k0bvs5ysrcp2088xqqyg6y-hspec-2.4.8
├── result-127 -> /nix/store/z60v7asc22q6cqxy5ck35n606vsqxywi-hspec-core-2.4.8
├── result-128 -> /nix/store/v3frkhmsf9rnf49c1vbgyxj2hhq993mh-hspec-discover-2.4.8
├── result-129 -> /nix/store/hiwzxhqnadg1kbjvpqn763fi2znn92sp-hspec-expectations-0.8.2
├── result-130 -> /nix/store/hyifvwa7n732x0y6m5qa2g3abkfvd12c-hspec-meta-2.4.6
├── result-131 -> /nix/store/dbd1m6r0nywa3vs9qs4m87xhd0cc09v5-http-api-data-0.3.7.2
├── result-132 -> /nix/store/s6jvsc5yqqm4mid96sziqdcg3rwxzmsl-http-client-0.5.13.1
├── result-133 -> /nix/store/b16b8k13bvhyqzr8v7j64bd0f11sp45k-http-date-0.0.8
├── result-134 -> /nix/store/s9476r1wyj6as6vfij0r04pa87h63lcr-http-media-0.7.1.2
├── result-135 -> /nix/store/9x3fawkm7bqz2y2yiva6kl4h07ags9qi-http-types-0.12.1
├── result-136 -> /nix/store/d40m7wm1zkag2pnbjw6afdzlrhvycmaa-http2-1.6.3
├── result-137 -> /nix/store/8yp7in2h1hrcan5791ra03d7kk1f406a-hxt-9.3.1.16
├── result-138 -> /nix/store/gsa8l0dj58416v9bh06n3nqqp7a9av5v-hxt-charproperties-9.2.0.1
├── result-139 -> /nix/store/yza4z8zjakscsj9jg0l61mwd53hmx5pj-hxt-regex-xmlschema-9.2.0.3
├── result-140 -> /nix/store/70zi0p6g69bdc95537m0imxzlpjgsywa-hxt-unicode-9.0.2.4
├── result-141 -> /nix/store/yl6dmyds80fmj7v5hdvw4glfkdn1zddr-hxt-xpath-9.1.2.2
├── result-142 -> /nix/store/hzj3ckkxcrjn24r8np5mji2chpfrdsr1-ieee754-0.8.0
├── result-143 -> /nix/store/b2713xg3wiyiwkmbyqcykqkhm0x8zfr3-integer-logarithms-1.0.2.1
├── result-144 -> /nix/store/fljxzdqml5hhwg65brwm8pqz0aslsf73-iproute-1.7.5
├── result-145 -> /nix/store/8fmpbwsciv7nsvl7x89993bxlmb9ddlx-js-flot-0.8.3
├── result-146 -> /nix/store/yg06a5cm52la4bxpqjq4kpmbmrqhrm4s-js-jquery-3.3.1
├── result-147 -> /nix/store/5vy7h1priyc418s5ksm9x71ibjiixdvj-kan-extensions-5.1
├── result-148 -> /nix/store/0ygwv4d7pi4a5aghy6p6m9qf5fx04564-language-haskell-extract-0.2.4
├── result-149 -> /nix/store/1j43sqfm66vdg0dyr3pjbq73niv09pmx-lens-4.16.1
├── result-150 -> /nix/store/inm5sq9wvcpnga9g9dv2gyh47l33df6w-lifted-async-0.9.3.3
├── result-151 -> /nix/store/7xc83028gmlz6pv7aic5hhbssxhsbdf2-lifted-base-0.2.3.12
├── result-152 -> /nix/store/jw9rfi5akrzjv8ryxz2jcbqcn94nfv0a-logging-facade-0.3.0
├── result-153 -> /nix/store/vvpz44b3vcvvijrj6x7cmk6qwx74s7hv-logict-0.6.0.2
├── result-154 -> /nix/store/w0h2ykbw0s8ibjrh4i5j9sv5kvx18iyq-math-functions-0.2.1.0
├── result-155 -> /nix/store/nkww37g4ibl9rfbj8dpi3ah9042w4r81-megaparsec-6.4.1
├── result-156 -> /nix/store/jf24dhl0jvs55c2skh42jzhiwbfydj1k-memory-0.14.16
├── result-157 -> /nix/store/i8jk6xk8p2m3hnjvqqijab7idka4ilx6-microstache-1.0.1.1
├── result-158 -> /nix/store/71hqw1jpr8b16d45djywl3i4cqnxgpi3-mime-types-0.1.0.8
├── result-159 -> /nix/store/hzz8a68m2gxrpss2m1daapvn9gffnvyy-mmorph-1.1.2
├── result-160 -> /nix/store/wqmh4wajkvd5qxlx83j4bqzzwjzc6nw1-mockery-0.3.5
├── result-161 -> /nix/store/xckprxp5mf5qiy642s17h4zj8bfy1q97-monad-control-1.0.2.3
├── result-162 -> /nix/store/h24azfc02dycnfwaj5db8hdjgl46ijhh-monad-par-0.3.4.8
├── result-163 -> /nix/store/jnf8vzc2g64b11m531laf58i9da6fsvz-monad-par-extras-0.3.3
├── result-164 -> /nix/store/x95fwypczi3rdam9cj5c9l4qyk074gws-monad-peel-0.2.1.2
├── result-165 -> /nix/store/rdpyx9w31ng7sv3akfybipwy9kfbil79-mono-traversable-1.0.9.0
├── result-166 -> /nix/store/29r5x9d2j5am8932il0833acj42flx2d-mtl-2.2.2
├── result-167 -> /nix/store/0h8j0pa3mv33w3ypmv430iwbkgqpmgh9-multimap-1.2.1
├── result-168 -> /nix/store/xb2aglici82w6qwys1wvldri532pkpym-mwc-random-0.13.6.0
├── result-169 -> /nix/store/dgqb4inrplk8rm27hcsnp60yb3cyng2q-nanospec-0.2.2
├── result-170 -> /nix/store/2dfsxlbr0q2l535pncf1ngcr9wn9svsx-nats-1.1.2
├── result-171 -> /nix/store/p6vwvakyzph49r8n23a24k7lr6zqmj5r-natural-transformation-0.4
├── result-172 -> /nix/store/v8q5j9w07ii5prmwm96f35x70vffdk0g-network-2.6.3.6
├── result-173 -> /nix/store/68gfcksbv2xsbfnhf4794lc9c2vici22-network-uri-2.6.1.0
├── result-174 -> /nix/store/vskv594q42alni4jh6cnk62jk0y31ajj-newtype-0.2
├── result-175 -> /nix/store/58djfwakj2if4j91pvb60k89zq9b14m3-numbers-3000.2.0.2
├── result-176 -> /nix/store/gs7cryr7ym53hmgwm7apq313x69kbd1c-old-locale-1.0.0.7
├── result-177 -> /nix/store/b0436kiczkwn5wrp94gj53fk1pq4bjri-old-time-1.1.0.3
├── result-178 -> /nix/store/smf0537p76indg1nqn7phnsz3i8silny-optparse-applicative-0.14.2.0
├── result-179 -> /nix/store/rbcj6ircngmzd0amb1dsm14y5xjhrq77-parallel-3.2.2.0
├── result-180 -> /nix/store/b3479v878j46v2xsr94kzhxdqjpdrkq7-parsec-3.1.13.0
├── result-181 -> /nix/store/kkvqq2x8pvrgbcmc3igm3arkjnvb3x5p-parser-combinators-0.4.0
├── result-182 -> /nix/store/b02z0a1nirnfg47bfv1zdcmwsg559zxn-path-0.6.1
├── result-183 -> /nix/store/bx3izhzxi5pfnf5x9qqyjin2h1k44dvz-pcre-light-0.4.0.4
├── result-184 -> /nix/store/062yimnahrgfpkih555s6sm1zah06nqh-polyparse-1.12
├── result-185 -> /nix/store/zsqdjf15mwvnvik4a2xfb950g83k5iza-pretty-show-1.6.16
├── result-186 -> /nix/store/vin8x1680ppdg1rfhwps1160rhlh301x-primitive-0.6.4.0
├── result-187 -> /nix/store/rvfl047idwvwg53n28b4sg98s9bh6ry6-profunctors-5.2.2
├── result-188 -> /nix/store/zmjg8iqnybd13z10d26n4cbkgg6ws45l-psqueues-0.2.7.0
├── result-189 -> /nix/store/gh9mhhzdlk6yp8nxnyswfkx8yl3bbpy3-quickcheck-instances-0.3.18
├── result-190 -> /nix/store/d0lic844ac58lys8bswcppix6pfn24xy-quickcheck-io-0.2.0
├── result-191 -> /nix/store/abyh9a1paf82ms0x53vzz29gkrmwdsg5-random-1.1
├── result-192 -> /nix/store/h694dn87843nz7cf5mwks673n8ykkfky-raw-strings-qq-1.1
├── result-193 -> /nix/store/4fshwfizsmkjsl8cvqi1ani2x5nsg9fd-refact-0.3.0.2
├── result-194 -> /nix/store/xsrg59n73w1rjjq23gp8archiinmmpw2-reflection-2.1.4
├── result-195 -> /nix/store/any94jclwz5p43m3qszklgf750lgvwf1-regex-base-0.93.2
├── result-196 -> /nix/store/vv6dvl55kiavx7ycv2gsa16g67lv5pkz-regex-posix-0.95.2
├── result-197 -> /nix/store/76nxwrbw42m0plbn8ssqk4djk02gl0kj-resourcet-1.2.1
├── result-198 -> /nix/store/y9r7igkmb85jvzpjw6b47bmpk0ggyk6a-safe-0.3.17
├── result-199 -> /nix/store/vpgvy0qcvph6z3rswrd60ii82kb29z8s-scientific-0.3.6.2
├── result-200 -> /nix/store/1w7izmqqmp9phs439s88hf9qn9bmg1wh-semigroupoids-5.2.2
├── result-201 -> /nix/store/a9pmg321waladl0qa28aqag345yg3m6m-semigroups-0.18.5
├── result-202 -> /nix/store/1hr1hz3w1670cykb06xds7l3w3la5crs-servant-0.13.0.1
├── result-203 -> /nix/store/dskpnfcrrwmf8lmbwllyjqwyykbsacps-servant-server-0.13.0.1
├── result-204 -> /nix/store/4l3srvbw0n1z0sbkjbvhw8pcwsdrvli8-setenv-0.1.1.3
├── result-205 -> /nix/store/wp0gwmcll2plds5c67i26ild6gh838yv-shake-0.16.4
├── result-206 -> /nix/store/q9f7202fb5mfslzybj7jfbp18wcy703b-siggy-chardust-1.0.0
├── result-207 -> /nix/store/1r7nqf6dsnlvirmh38im2bmv0qfda2nx-silently-1.2.5
├── result-208 -> /nix/store/a9vvkk23cls1b5vm99fvpharblzf8lfx-simple-reflect-0.3.3
├── result-209 -> /nix/store/yfiqh2l5g9irsqyl6hhw09lrybd8hka9-simple-sendfile-0.2.27
├── result-210 -> /nix/store/dkvr7mvi7iqlg9ipxifn4fzw8qs5j9hp-singleton-bool-0.1.4
├── result-211 -> /nix/store/nzsrrmkqiwzn5f0bm02x6n16zc12b4aa-siphash-1.0.3
├── result-212 -> /nix/store/hhvqip429g7fc30wd3jl2r5k4sqxvwqp-smallcheck-1.1.5
├── result-213 -> /nix/store/aalrja33a8cwrwx1mzp8zf8rwgb3qlma-split-0.2.3.3
├── result-214 -> /nix/store/br2zqlqq1icj97s5llvbjy1spmyfk3hc-stack2cabal-0.1.0.0
├── result-215 -> /nix/store/gixf7rdfxd3pavcpynpnhf7qxggqv22c-statistics-0.14.0.2
├── result-216 -> /nix/store/q3n30r6axpj41qsvzjdzjb5b5zmzv3wf-stm-2.4.5.0
├── result-217 -> /nix/store/d6pairzs2js2wbb0n1wlfwy3izg67p05-streaming-commons-0.1.19
├── result-218 -> /nix/store/3nr2xcjyzqn7dgs4fppadhwkzn0chynq-string-conversions-0.4.0.1
├── result-219 -> /nix/store/kx55q85182znp8yci0xrqq936m3cbwar-stringbuilder-0.5.1
├── result-220 -> /nix/store/xfqnhh1fyzgwfpwgz1wz1zl7g4gshlz3-stringsearch-0.3.6.6
├── result-221 -> /nix/store/zsvd38jandhcn1kz4x0lbgl13hgya3s2-syb-0.7
├── result-222 -> /nix/store/hphnzpkaksxk1zrzrrka6b0w8slzm60w-system-filepath-0.4.14
├── result-223 -> /nix/store/wncvkhld17k4jc5jajnsaawnvi9i1c2g-tagged-0.8.6
├── result-224 -> /nix/store/x9iyyl1wj5y30gj66yk7sifplbj8vpjd-tasty-1.0.1.1
├── result-225 -> /nix/store/rkrl3x16wqyr73ji8lc3ng48cr5bw6l1-tasty-compare-0.1.0
├── result-226 -> /nix/store/ddarg6c6pcg6h1s850cajp0mjnp8x8kv-tasty-discover-4.2.1
├── result-227 -> /nix/store/9lis562sjcjdn44xi7crvmh3ay6nqr0m-tasty-expected-failure-0.11.1.1
├── result-228 -> /nix/store/mmxjs5cm2xqzv9bpzc8mi8ggn3lv8vyh-tasty-golden-2.3.2
├── result-229 -> /nix/store/ra5zikrpjcnkvyg9fy6lj98yxg674qcz-tasty-hedgehog-0.1.0.2
├── result-230 -> /nix/store/0n1ij9k89llvczpvyzmy0dmkkshp4ra3-tasty-hspec-1.1.5
├── result-231 -> /nix/store/9cm49hgjv6d1bdk829kvwxxxkxgc161m-tasty-hunit-0.10.0.1
├── result-232 -> /nix/store/l7dgvzmbkhflhixhyax1nz5g4lb9b26g-tasty-kat-0.0.3
├── result-233 -> /nix/store/i39k6s94dbwhrfyvnq1dxwfd3zq0p6dm-tasty-quickcheck-0.9.2
├── result-234 -> /nix/store/f0ns0xvdz6marw4yys0y11rqyp89p9ba-tasty-smallcheck-0.8.1
├── result-235 -> /nix/store/774m9jkhdwvajirkykjbz2p2zjvi70zz-tasty-th-0.1.7
├── result-236 -> /nix/store/8ygcma77mkpfii3jjnlq72696vxl799b-temporary-1.2.1.1
├── result-237 -> /nix/store/mpzvb6yssm5ihnqxszr7yxqklb3rqfrh-temporary-rc-1.2.0.3
├── result-238 -> /nix/store/bc22x2pp24kzjscibxwjhgsghkzg2lgw-terminal-size-0.3.2.1
├── result-239 -> /nix/store/krjx42jdm52z5a3rfyy1rmivqkdvwzs3-test-framework-0.8.2.0
├── result-240 -> /nix/store/cwcw7krmgsqh166qj8a3bfa0gk97nmv5-test-framework-hunit-0.3.0.2
├── result-241 -> /nix/store/lap89l8ijc1a6xln6sd6bpvk0yjbhjrp-test-framework-quickcheck2-0.3.0.4
├── result-242 -> /nix/store/1amx4633jw2mlvj6sdi2gpwm2x0fpxp3-test-framework-th-0.2.4
├── result-243 -> /nix/store/y1z3kb5wgc4gxjxhifw3wgh4vs43qbw9-text-1.2.3.0
├── result-244 -> /nix/store/23ql72rmqnymj26gy95ayjq82r3c8648-text-short-0.1.2
├── result-245 -> /nix/store/4ziqf008xri4b236zhd3ra7s7prm6r4k-tf-random-0.5
├── result-246 -> /nix/store/2w9wi09mnni92ppqng9anwn6snw6dvbg-th-abstraction-0.2.8.0
├── result-247 -> /nix/store/sh25p1m850zl461n46hl7ana6x6sfxg5-th-expand-syns-0.4.4.0
├── result-248 -> /nix/store/40d2r5r4a7py9k0yx12swwcx3gw3ln3i-th-lift-0.7.10
├── result-249 -> /nix/store/vvb96hccmah7cqnkdrxb6l3jfn47cphn-th-lift-instances-0.1.11
├── result-250 -> /nix/store/j683jb4lcqxs9gp2sm4xlyja01rwcpyb-th-orphans-0.13.6
├── result-251 -> /nix/store/2dy4vjhwjqmxjq6j1dg5jcdhssdh442h-th-reify-many-0.1.8
├── result-252 -> /nix/store/xayl18rkm1lnvlq71698cdk08fxninm9-time-locale-compat-0.1.1.4
├── result-253 -> /nix/store/vvhs59vm9bv228sg7bjjwzqcj0kl8c3h-transformers-base-0.4.4
├── result-254 -> /nix/store/krrjn75axzkfn1hab6mgrzrq4gg86qmc-transformers-compat-0.5.1.4
├── result-255 -> /nix/store/i9002ydvqk0njr1r9vgv6s09qzbnkv6a-typed-process-0.2.2.0
├── result-256 -> /nix/store/3l4bp8w9q69yavbfyzksj7bfspnsx2d5-unbounded-delays-0.1.1.0
├── result-257 -> /nix/store/mq8m6r7sin6xylk9qslykdj3b3cvfhml-uniplate-1.6.12
├── result-258 -> /nix/store/zdpip8asrgnacy8rzc1561pqlc2d5ic6-units-parser-0.1.1.2
├── result-259 -> /nix/store/hrmj78jgxak1ln01l5d1vsf0b97qqazc-unix-compat-0.5.0.1
├── result-260 -> /nix/store/zqcanpcinxanck341r4a9z0gh5n7xaip-unix-time-0.3.8
├── result-261 -> /nix/store/y2n10m56ivwqkmxg5rb779bx1ag6i4zc-unliftio-0.2.7.0
├── result-262 -> /nix/store/85kaxp08vkfv7avp8n3y33f6wdk05wy5-unliftio-core-0.1.1.0
├── result-263 -> /nix/store/il9al7bxvar5xgcs9baz6fxr914lj2l5-unordered-containers-0.2.9.0
├── result-264 -> /nix/store/qzp6hlc1j40igskyxlrjvxwlh9cfqwhn-uom-plugin-0.3.0.0
├── result-265 -> /nix/store/0nsvafyppij53ik1nykzlif2kyyb0ax7-uri-bytestring-0.3.2.0
├── result-266 -> /nix/store/7qsfdiyhdfv421ns19dv36mwqbifjfm1-utf8-string-1.0.1.1
├── result-267 -> /nix/store/qqcajc3xskpr9rckmsxwwa5yd69008mh-uuid-types-1.0.3
├── result-268 -> /nix/store/ipbzjyisq33038g9phahyqv707ddrxjg-vault-0.3.1.2
├── result-269 -> /nix/store/6b7m2mr6458dzj0iq5csm5ragdk649ry-vector-0.12.0.1
├── result-270 -> /nix/store/pbd8bzx46ck66d6fg1dd7md803pl71y1-vector-algorithms-0.7.0.1
├── result-271 -> /nix/store/zvipb205lxc2azy59drlam34zw9ckic2-vector-binary-instances-0.2.4
├── result-272 -> /nix/store/m9a9359injs5qlcnrxlm66fpr8a9k32r-vector-builder-0.3.6
├── result-273 -> /nix/store/233kfb8kq7lh6haicr6h20d5l1n0dq1s-vector-th-unbox-0.2.1.6
├── result-274 -> /nix/store/w9ls4z4iijxiiycj6qkm2bwzb5bjyqrn-void-0.7.2
├── result-275 -> /nix/store/a5fv5dhnhs79d7xs6w7mncak1vk3jsrg-wai-3.2.1.2
├── result-276 -> /nix/store/wbx5xg0iiqvn2m7c6xfimxm0ml04jk1w-wai-app-static-3.1.6.2
├── result-277 -> /nix/store/5s85k2lrjfwja1zcpm3zzda44h9mvvpw-wai-cors-0.2.6
├── result-278 -> /nix/store/5bv8r0vshcmzdv0yzj6wz54jw8h4jarl-wai-extra-3.0.22.0
├── result-279 -> /nix/store/pbarmj7mn9vk01ypcp6hf6znavd399a7-wai-logger-2.3.2
├── result-280 -> /nix/store/7rm2wcb6pvxd6xcafdaripshb15dnib5-wai-websockets-3.0.1.2
├── result-281 -> /nix/store/4r71r639mam3hskiijy0lxivxnhwykkw-warp-3.2.23
├── result-282 -> /nix/store/br2w67gzbfmk5a1fy2zzlqvfdddvip27-websockets-0.12.5.1
├── result-283 -> /nix/store/0zcwp0h04khvyza7032fpv1y44ssbijy-weigh-0.0.12
├── result-284 -> /nix/store/zk15lfd5hx061qbsm0i0snr648bkpdan-with-location-0.1.0
├── result-285 -> /nix/store/4ssy6c3cyhc7w7nmyk5c49if7ihxci1v-wl-pprint-annotated-0.1.0.0
├── result-286 -> /nix/store/7873h1vlb0j9j4y5jmyqw8mlmnj43n2m-word8-0.1.3
├── result-287 -> /nix/store/rqlr1gcazid2gwyi2n5lsimmmw86ysm6-www-flare-timing-0.1.0
├── result-288 -> /nix/store/nxgfiaf8smviz9rm22y6g3a2jd57c1yg-xml-1.3.14
├── result-289 -> /nix/store/jgkbh73a3fn68w3q3v8vch54d97pzgsr-yaml-0.8.32
└── result-290 -> /nix/store/sb7lqx8qnh8484y4g7n3p9qns9limm2x-zlib-0.6.2

291 directories, 3 files
```

We can use naming to find the links with executable outputs;

```
stackage2nix> ls -l | egrep "\->.*flare"
... result-287 -> /nix/store/rqlr1gcazid2gwyi2n5lsimmmw86ysm6-www-flare-timing-0.1.0
... result-34 -> /nix/store/5lhvm9grgryrll45n5ar012fiv52hcjy-build-flare-timing-0.1.0
... result-82 -> /nix/store/r5iw2hz1k6m78vgr08k9y2v8832n07wy-flare-timing-0.1.0

stackage2nix> ls -1 result-287/bin
comp-serve

stackage2nix> ls -1 result-82/bin
align-time
cross-zone
discard-further
extract-input
gap-point
land-out
mask-track
tag-zone
task-length
test-fsdb-parser
test-igc-parser
test-kml-parser
```

