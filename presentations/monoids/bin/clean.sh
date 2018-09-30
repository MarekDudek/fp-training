#!/bin/bash

set -euox pipefail
IFS=$'\n\t'

DIRNAME=$(dirname "$0")

rm -fr ${DIRNAME}/tmp/*

