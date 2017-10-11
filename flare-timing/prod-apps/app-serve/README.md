### Comp Server

    $ __shake-build/comp-serve --help
    Serve nominals, tasks and pilots from a competition YAML file.

    comp-serve [OPTIONS]
      With one competition *.comp.yaml file supplied

    Common flags:
      -f --file=ITEM
      
Run the server;

    $ __shake-build/comp-serve --file=Forbes2012.comp.yaml
    Drive {file = "Forbes2012.comp.yaml"}
    ServeOptions {file = "Forbes2012.comp.yaml"}
    listening on port 3000

The following are the web service endpoints;

    http://localhost:3000/nominals
    http://localhost:3000/tasks
    http://localhost:3000/pilots
