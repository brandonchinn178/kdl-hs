#!/usr/bin/env bash

set -eux -o pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
tmpdir="$(mktemp -d)"

commit=a88c450d7d8d235986c89249581e40c5ba794763
url="https://github.com/kdl-org/kdl/archive/${commit}.tar.gz"

curl -fsSL "${url}" | tar xzf - -C "${tmpdir}" --strip-components=1
tar czf "${here}/tests.tar.gz" -C "${tmpdir}/tests/test_cases/" .

rm -rf $tmpdir
