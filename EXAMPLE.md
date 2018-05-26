# Worked Example

With this repo and [flare-timing-scores]() cloned as siblings the following
commands show how to score all tasks at once for the **2012 Hang Gliding
Pre-World Forbes** competition.

```
> ln -s ../flare-timing-scores/Forbes/2012 forbes-2012

> __shake-build/extract-input "--file=forbes-2012/Australia Forbes 2012.fsdb"
Extracted 8 tasks from "2012 Hang Gliding Pre-World Forbes"
Extracting tasks completed in 2.09 s

> __shake-build/task-length "--file=forbes-2012/Australia Forbes 2012.comp-input.yaml"
Australia Forbes 2012.comp-input.yaml
Measuring task lengths completed in 4.95 m

> __shake-build/cross-zone "--file=forbes-2012/Australia Forbes 2012.comp-input.yaml"
Reading competition from 'Australia Forbes 2012.comp-input.yaml'
Tracks crossing zones completed in 1.51 m

> __shake-build/tag-zone "--file=forbes-2012/Australia Forbes 2012.cross-zone.yaml"
Reading zone crossings from 'Australia Forbes 2012.cross-zone.yaml'
Tagging zones completed in 1.07 s

> __shake-build/align-time "--file=forbes-2012/Australia Forbes 2012.comp-input.yaml"
Reading competition from 'Australia Forbes 2012.comp-input.yaml'
Reading flying time range from 'Australia Forbes 2012.cross-zone.yaml'
Reading zone tags from 'Australia Forbes 2012.tag-zone.yaml'
Aligning times completed in 52.97 m

> __shake-build/discard-further "--file=forbes-2012/Australia Forbes 2012.comp-input.yaml"
Reading competition from 'Australia Forbes 2012.comp-input.yaml'
Reading task length from 'Australia Forbes 2012.task-length.yaml'
Reading zone tags from 'Australia Forbes 2012.tag-zone.yaml'
Filtering times completed in 1.08 m

> __shake-build/mask-track "--file=forbes-2012/Australia Forbes 2012.comp-input.yaml"
Reading competition from 'Australia Forbes 2012.comp-input.yaml'
Reading task length from 'Australia Forbes 2012.task-length.yaml'
Reading flying time range from 'Australia Forbes 2012.cross-zone.yaml'
Reading zone tags from 'Australia Forbes 2012.tag-zone.yaml'
Masking tracks completed in 1.59 m

> __shake-build/land-out "--file=forbes-2012/Australia Forbes 2012.comp-input.yaml"
Reading land outs from 'Australia Forbes 2012.mask-track.yaml'
Land outs counted for distance difficulty completed in 179.71 ms

> __shake-build/gap-point "--file=forbes-2012/Australia Forbes 2012.comp-input.yaml"
Reading pilots absent from task from 'Australia Forbes 2012.comp-input.yaml'
Reading pilots that did not fly from 'Australia Forbes 2012.cross-zone.yaml'
Reading start and end zone tagging from 'Australia Forbes 2012.tag-zone.yaml'
Reading masked tracks from 'Australia Forbes 2012.mask-track.yaml'
Reading distance difficulty from 'Australia Forbes 2012.land-out.yaml'
Tallying points completed in 1.41 s
```
