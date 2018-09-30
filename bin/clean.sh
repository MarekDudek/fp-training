#!/bin/bash

set -euox pipefail
IFS=$'\n\t'

DIRNAME=$(dirname "$0")


${DIRNAME}/../presentations/monoids/bin/clean.sh
