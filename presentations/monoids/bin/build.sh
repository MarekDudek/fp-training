#!/bin/bash

set -euox pipefail
IFS=$'\n\t'

DIRNAME=$(dirname "$0")


pdflatex ${DIRNAME}/../slides/monoids-slides.tex
pdflatex ${DIRNAME}/../slides/monoids-slides.tex

pdflatex ${DIRNAME}/../blog/monoids-post.tex
pdflatex ${DIRNAME}/../blog/monoids-post.tex
