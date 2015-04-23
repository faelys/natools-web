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
	    | diff -u --label "${EXPECTED_DIR}/${EXPECTED}" \
		--label "${BASE_URL}${TARGET}" \
		"${EXPECTED_DIR}/${EXPECTED}" - \
	    || STOPPED=yes
}

# chain /relative/path expected/file [curl extra flags]
#    Same as check function, but only if previous check didn't fail
chain(){
	test -z "${STOPPED}" && check "$@"
}

# chain_curl arguments
#    Wrapper around raw curl with discarded output, but only if previous check
#    didn't fail.
chain_curl(){
	test -z "${STOPPED}" && curl -s "$@" >/dev/null
}


check /first first.html
check /second second.html
check /third third.html
check /style.css ../data/static/style.css
check /non-existant 404.html
check /first 405.html -d ''
check /index index.html
check /tags/topic1 tags-topic1.html
check /tags/topic2 tags-topic2.html
check /tags/topic3 tags-topic3.html
check /tags/ tags.html
check /tags tags-redirect.html
check /comments comments.html
check /comment-feed comments.atom

check /fourth fourth.html
chain /fourth/comments 405.html
chain /fourth fourth.html
chain /fourth/comments fourth-preview.html -F 'c_mail=' \
    --form-string 'c_name=<code>nat</code>' \
    -F 'c_site=http://instinctive.eu/"' -F 'preview=Preview' \
    -F 'c_text=Preview comment that should not be written anywhere.'
chain /fourth fourth.html
chain /fourth/comments fourth-303.html -F 'c_mail=' \
    -F 'c_name=Random Stranger' \
    -F 'c_site=http://instinctive.eu/' -F 'submit=Submit' \
    -F 'c_text=Attempted spam comment text.' \
    -F 'address=http://instinctive.eu/'
chain /fourth fourth.html
chain /fourth/comments fourth-spam-1.html \
    -F 'c_name=Random Stranger' \
    -F 'c_site=http://instinctive.eu/' -F 'submit=Submit' \
    -F 'c_text=Attempted spam comment text.'
chain /fourth/comments fourth-spam-2.html -F 'c_mail=' \
    -F 'c_name=Random Spammer' \
    -F 'c_site=http://instinctive.eu/' -F 'submit=Submit' \
    -F 'c_text=Attempted spam comment text.'
chain /fourth fourth.html
chain /test base_version.txt
chain_curl -F 'sleep_update=2' "${BASE_URL}/test"
chain /fourth/comments fourth-303.html -F 'c_mail=' \
    --form-string 'c_name=<i>Nobody</i>' \
    -F 'c_site=http://instinctive.eu/"' -F 'submit=Submit' \
    -F 'c_text=Brand new comment posted during the test suite'
chain /fourth fourth.html
chain_curl -F 'wait_version=2' "${BASE_URL}/test"
chain /fourth fourth-commented.html
