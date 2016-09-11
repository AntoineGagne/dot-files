#! /bin/bash

for file in $(find . -name ".[^.]*" ! -path "./.git"); do
    ln -sf "$(pwd)/${file:2}" "${1}"
done
