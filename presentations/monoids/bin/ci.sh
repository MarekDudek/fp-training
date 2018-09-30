#!/bin/bash

set -euox pipefail
IFS=$'\n\t'

DIRNAME=$(dirname "$0")


${DIRNAME}/clean.sh
${DIRNAME}/build.sh
