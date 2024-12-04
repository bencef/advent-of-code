#!/bin/sh

(
    sed 's/\(.\)\(.*\)/\2\1/' < ../input.txt # \1 - sign, \2 - number
    echo p
    echo q
) | dc
