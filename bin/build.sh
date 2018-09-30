#!/bin/bash

set -euox pipefail
IFS=$'\n\t'

DIRNAME=$(dirname "$0")


CURRENT=`pwd`

cd ${DIRNAME}/../presentations/monoids/tmp/
../bin/build.sh

cd ${CURRENT}
