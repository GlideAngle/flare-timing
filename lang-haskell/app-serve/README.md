# app-serve


The backend of flare-timing, the web app, this uses
[servant](https://haskell-servant.readthedocs.io) to serve the data needed by
the [frontend](https://github.com/NoseCone/nose-cone).

To serve a comp that has already been scored, go to its root folder and start
the server like so for the [QuestAir Open
2016](https://github.com/FlareTiming/comps/tree/master/Quest/2016) data:

```
> git clone https://github.com/FlareTiming/comps.git
> cd comps/Quest/2016
> comp-serve --file=2016QuestAirOpen
Reading task length from '2016QuestAirOpen.task-length.yaml'
Reading competition & pilots DNF from '2016QuestAirOpen.comp-input.yaml'
Reading flying time range from '2016QuestAirOpen.cross-zone.yaml'
Reading zone tags from '2016QuestAirOpen.tag-zone.yaml'
Reading scored section from '2016QuestAirOpen.peg-frame.yaml'
Reading arrivals from '2016QuestAirOpen.mask-arrival.yaml'
Reading effort from '2016QuestAirOpen.mask-effort.yaml'
Reading leading from '2016QuestAirOpen.mask-lead.yaml'
Reading reach from '2016QuestAirOpen.mask-reach.yaml'
Reading speed from '2016QuestAirOpen.mask-speed.yaml'
Reading bonus reach from '2016QuestAirOpen.bonus-reach.yaml'
Reading land outs from '2016QuestAirOpen.land-out.yaml'
Reading scores from '2016QuestAirOpen.gap-point.yaml'
Reading expected or normative land outs from '2016QuestAirOpen.norm-effort.yaml'
Reading expected or normative optimal routes from '2016QuestAirOpen.norm-route.yaml'
Reading expected or normative scores from '2016QuestAirOpen.norm-score.yaml'
listening on port 3000
```
