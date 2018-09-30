#!/bin/bash

set -euox pipefail
IFS=$'\n\t'

DIRNAME=$(dirname "$0")

${DIRNAME}/clean.sh
pdflatex ${DIRNAME}/../slides/monoids.tex
cp ${DIRNAME}/../tmp/monoids.pdf ..
