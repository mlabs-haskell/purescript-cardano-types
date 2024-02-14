#!/usr/bin/env bash

for f in ./src/Cardano/Types/*.purs; do
    bash scripts/check-instances.sh "$f";
done
