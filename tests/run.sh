#!/bin/sh

# Copyright (c) 2014-2015, Natacha Porté
#
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

set -eu

TARGET_BIN=${1:+${PWD}/}${1:-../bin/simple_site}

cd "$(dirname "$0")"

if ! test -x "${TARGET_BIN}"; then
	echo "Usage: $0 target-binary [base_url]" >&2
	exit 1
fi

rm -rf data/comments/4 data/comments/5 data/comments/contact
: >|data/comments/spam.sx

touch running
${TARGET_BIN} data/site.sx running >server.log &
TARGET_PID=$!
sleep 1
sh black-box.sh "${2:-http://localhost:8888/}" \
    || echo "Black box test suite failed."
rm running
wait ${TARGET_PID}
