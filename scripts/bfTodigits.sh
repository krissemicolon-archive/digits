#!/bin/sh

#===================================
#   Converts Brainfuck to Digits   #
#===================================

#   > = 0
#   < = 1
#   + = 2
#   - = 3
#   . = 4
#   , = 5
#   [ = 6
#   ] = 7

FN=$1
EN=$2

sed -e 's&>&0&g' -e 's&<&1&g' -e 's&+&2&g' -e 's&-&3&g' -e 's&\.&4&g' -e 's&,&5&g' -e 's&\[&6&g' -e 's&]&7&g' $FN > $EN