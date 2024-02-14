#!/usr/bin/env bash

file="$1"

echo "Checking $file"
AsCbor="$(cat "$file" | grep -nw 'instance AsCbor')"
EncodeAeson="$(cat "$file" | grep -nw 'instance EncodeAeson')"
DecodeAeson="$(cat "$file" | grep -nw 'instance DecodeAeson')"
Show="$(cat "$file" | grep -nw 'instance Show')"
Eq="$(cat "$file" | grep -nw 'instance Eq')"
Ord="$(cat "$file" | grep -nw 'instance Ord')"
Generic="$(cat "$file" | grep -nw 'instance Generic')"

if [[ "" == "$AsCbor" ]] ; then
    echo "  No AsCbor instance!"
fi;

if [[ "" == "$EncodeAeson" ]] ; then
    echo "  No EncodeAeson instance!"
fi;

if [[ -z "$DecodeAeson" ]] ; then
    echo "  No DecodeAeson instance!"
fi;

if [[ -z "$Show" ]] ; then
    echo "  No Show instance!"
fi;

if [[ -z "$Eq" ]] ; then
    echo "  No Eq instance!"
fi;

if [[ -z "$Ord" ]] ; then
    echo "  No Ord instance!"
fi;

if [[ -z "$Generic" ]] ; then
    echo "  No Generic instance!"
fi;
