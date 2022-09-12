#!/bin/bash
set -o errexit -o nounset -o pipefail
cd "$(dirname "$0")/../zig"

zig fmt src/*.zig
zig build test $@