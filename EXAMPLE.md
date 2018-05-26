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
```
