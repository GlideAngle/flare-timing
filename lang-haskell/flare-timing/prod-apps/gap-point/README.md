### Landing Out 

    $ ft-gap-point --help

    Group and count land outs.

    Where 'c' is the comp name and '.' is the folder with competition inputs;
        Reads  ./c.mask-track.yaml
        Writes ./c.land-out.yaml

    If a list of tasks are supplied then those tasks alone are processed, otherwise
    all tasks are processed.

    The same thing goes if a list of pilots is supplied or not.

    ft-gap-point [OPTIONS]

    Source:
      -d --dir=ITEM          Over all *.comp.yaml files in this directory
      -f --file=ITEM         With this one competition *.comp.yaml file
      -t --task=INT
      -p --pilot=ITEM
      -m --math=MATH
      -s --speedsectiononly
