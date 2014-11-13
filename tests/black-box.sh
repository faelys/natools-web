#!/bin/sh

# Copyright (c) 2014, Natacha Porté
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

set -ue

if test $# -lt 1; then
	echo "Usage: $0 http://host/ [path/to/expected]" >&2
	exit 1
fi

BASE_URL=${1%/}
EXPECTED_DIR=${2:-$(dirname "$0")/expected}

# check /relative/path expected/file [curl extra flags]
#    Request the given URL and compare it with the reference output
check(){
	TARGET=$1
	EXPECTED=$2
	STOPPED=
	shift 2
	curl -s "$@" "${BASE_URL}${TARGET}" \
	    | diff -u "${EXPECTED_DIR}/${EXPECTED}" - \
	    | sed "2s|-|${BASE_URL}${TARGET}|" \
	    || STOPPED=yes
}

# chain /relative/path expected/file [curl extra flags]
#    Same as check function, but only if previous check didn't fail
chain(){
	test -z "${STOPPED}" && check "$@"
}

check /first first.html
check /second second.html
check /style.css ../data/static/style.css
check /non-existant 404.html
