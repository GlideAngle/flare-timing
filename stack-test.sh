#!/bin/sh

#set +v

stack test --no-terminal --stack-yaml=lang-haskell/stack.yaml --test-arguments "--color=always"
