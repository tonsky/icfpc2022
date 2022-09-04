#!/bin/bash
set -o errexit -o nounset -o pipefail
cd "$(dirname "$0")/.."

clj -M:opts -m icfpc2022.main runner $@