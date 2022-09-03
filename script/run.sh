#!/bin/bash
set -o errexit -o nounset -o pipefail
cd "$(dirname "$0")/.."

clj -J-Djdk.attach.allowAttachSelf -J-XX:+UnlockDiagnosticVMOptions -J-XX:+DebugNonSafepoints -M -m icfpc2022.main $@