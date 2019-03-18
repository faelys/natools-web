#!/bin/sh

# Copyright (c) 2014-2019, Natacha Porté
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
: ${SPAM_LOG:=${EXPECTED_DIR}/../data/comments/spam.sx}

test -e "${SPAM_LOG}" || touch "${SPAM_LOG}"

# check /relative/path expected/file [curl extra flags]
#    Request the given URL and compare it with the reference output
check(){
	TARGET=$1
	EXPECTED=$2
	STOPPED=
	shift 2
	if ! curl -s "$@" "${BASE_URL}${TARGET}" \
	    | diff -u --label "${EXPECTED_DIR}/${EXPECTED}" \
		--label "${BASE_URL}${TARGET}" \
		"${EXPECTED_DIR}/${EXPECTED}" -
	then STOPPED=yes
	fi
}

# chain /relative/path expected/file [curl extra flags]
#    Same as check function, but only if previous check didn't fail
chain(){
	if test -z "${STOPPED}"; then
		check "$@"
	fi
}

# chain_curl arguments
#    Wrapper around raw curl with discarded output, but only if previous check
#    didn't fail.
chain_curl(){
	if test -z "${STOPPED}"; then
		curl -s "$@" >/dev/null
	fi
}

# check_last_spam expected/file
#    Extract the last spam recorded and compare it to the given file.
#    Note that this function depends on site S-expression pretty printer cfg.
check_last_spam(){
	if ! sed -n '/^(/h; /^[^(]/H; $g; $p' "${SPAM_LOG}" \
	    | grep -v -e '^(' -e '(date ' \
	    | diff -u --label "${EXPECTED_DIR}/$1" \
		--label "last reported spam" \
		"${EXPECTED_DIR}/$1" -
	then STOPPED=yes
	fi
}

# chain_last_spam expected/file
#    Same as check function, but only if previous check didn't fail
chain_last_spam(){
	if test -z "${STOPPED}"; then
		check_last_spam "$@"
	fi
}

# get_version
#    Return the current version of site internal data
get_version(){
	curl -s "${BASE_URL}/test" \
	    | sed -n '/^Version:/s/[^0-9]//gp' \
	    | grep '[0-9]'
}


check /first first.html
check /second second.html -b 'foo=bar; User-Token=glaglagla'
check /third third.html
chain /third third-secure.html -H "X-Forwarded-Proto: https"
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
check /list-test list-test.html
check /expired expired.html
check /unpublished unpublished.html

check /first/comments first-comments-404.html -F 'ignore=Ignore' -F 'id=VV118'
chain /first first.html

check /first/comments first-spam.html -F 'c_mail=' \
    --form-string 'c_name=<i>Nobody</i>' \
    -F 'c_link=http://instinctive.eu/"' -F 'submit=Submit' \
    -F 'c_text=Comment text rejected solely because of bad filter'
chain /first first.html

check /second/comments second-404.html -F 'c_mail=' \
    --form-string 'c_name=<i>Nobody</i>' \
    -F 'c_link=http://instinctive.eu/"' -F 'submit=Submit' \
    -F 'c_text=Valid comment sent to a closed comment list'
chain /second second.html -b 'foo=bar; User-Token=glaglagla'

BASE_VERSION=$(get_version)
check /reload 405.html
check /reload/extra reload-extra-404.html -F 'submit=Submit'
NEW_VERSION=$(get_version)
if test "${BASE_VERSION}" != "${NEW_VERSION}"; then
	echo "Reload calls expected to be erroneous changed version" \
	    " from ${BASE_VERISON} to ${NEW_VERSION}"
fi

BASE_VERSION=$(get_version)
check /fourth fourth.html
chain /fourth/comments 405.html
chain /fourth fourth.html
chain /fourth/comments fourth-preview.html -F 'c_mail=' \
    --form-string 'c_name=  ' \
    -F 'c_link=http://instinctive.eu/"' -F 'preview=Preview' \
    -F 'c_text=Preview comment that should not be written anywhere.'
chain /fourth fourth.html
chain /fourth/comments fourth-303.html -F 'c_mail=' \
    -F 'c_name=Random Stranger' \
    -F 'c_link=http://instinctive.eu/' -F 'submit=Submit' \
    -F 'c_text=Attempted spam comment text.' \
    -F 'c_date=2015-03-03T15:10:00Z'
chain /fourth fourth.html
chain_last_spam spam-extra-fields.sx
chain /fourth/comments fourth-303.html -F 'c_mail=' \
    -F 'c_name=Random Stranger' \
    -F 'c_link=http://instinctive.eu/' -F 'submit=Submit' \
    -F 'c_text=Attempted use of disabled filter' \
    -F 'c_filter=pass-through'
chain_last_spam spam-filter-extra-field.sx
chain /fourth/comments fourth-spam-1.html \
    -F 'c_name=Random Stranger' \
    -F 'c_link=http://instinctive.eu/' -F 'submit=Submit' \
    -F 'c_text=Attempted spam comment text.'
chain_last_spam spam-missing-field.sx
chain /fourth/comments fourth-spam-2.html -F 'c_mail=' \
    -F 'c_name=Random Spammer' \
    -F 'c_link=http://instinctive.eu/' -F 'submit=Submit' \
    -F 'c_text=Attempted spam comment text.'
chain_last_spam spam-by-name.sx
chain /fourth/comments fourth-303.html -F 'address=here' \
    -F 'c_name=Random Spammer' \
    -F 'c_link=http://instinctive.eu/' -F 'submit=Submit' \
    -F 'c_text=Attempted spam comment text.'
chain_last_spam spam-all.sx
chain /fourth/comments fourth-303.html -F 'c_mail=quxx' \
    -F 'c_name=quxx' \
    -F 'c_link=http://instinctive.eu/' -F 'submit=Submit' \
    -F 'c_text=Innocent-looking spam comment'
chain_last_spam spam-fields-equal.sx
chain /fourth/comments fourth-303-newcomment.html -F 'c_mail=' \
    -F 'c_name=Random Troll' -F 'c_link=' -F 'submit=Submit' \
    -F 'c_text=Moderated comment text because of suspicious content.'
chain /fourth fourth.html
chain_curl -F 'sleep_update=2' "${BASE_URL}/test"
chain /fourth/comments fourth-303.html -F 'c_mail=' \
    --form-string 'c_name=<i>Nobody</i>' \
    -F 'c_link=instinctive.eu' -F 'submit=Submit' \
    -F 'c_text=Brand new comment posted during the test suite'
chain /fourth fourth-half-commented.html
chain_curl -F 'wait_version='$((BASE_VERSION + 2)) "${BASE_URL}/test"
chain /fourth fourth-commented.html
chain /reload reload-redirect.html -F 'submit=Submit'
chain_curl -F 'wait_version='$((BASE_VERSION + 3)) "${BASE_URL}/test"
chain /fourth fourth-commented.html

check /fifth fifth.html
chain /fifth fifth-cookie.html \
    -b 'c_info=0IiIiUmFuZG9tIFN0cmFuZ2VyIiIiaHR0cDovL2luc3RpbmN0aXZlLmV1Lw'
chain /fifth fifth-cookie.html \
    -b 'c_info=0KG5hbWUgIlJhbmRvbSBTdHJhbmdlciIpKGxpbmsgaHR0cDovL2luc3RpbmN0aXZlLmV1Lyk'

rm -f cookie-jar.txt
BASE_VERSION=$(get_version)
check /fifth fifth.html
chain "/fifth?extra=no" fifth.html
chain "/fifth?extra=yes" fifth-extra.html
chain /fifth/comments fifth-303.html -F 'c_mail=' -F 'c_name=Nobody' \
    -F 'c_link=http://instinctive.eu/"' -F 'submit=Submit' \
    -F 'c_text=Perfectly valid comment set to be ignored in the page.'
chain /fifth fifth.html
chain /fifth/comments fifth-303.html -F 'c_mail=' -F 'c_name=Administrator' \
    -F 'c_date=2015-03-03T15:10:00Z' \
    -F 'c_link=http://instinctive.eu/"' -F 'submit=Submit' --form-string \
    'c_text=<p>Administrator comments that bypasses default ignore</p>'
chain_curl -F 'wait_version='$((BASE_VERSION + 1)) "${BASE_URL}/test"
chain /fifth fifth-commented.html
chain /fifth/comments fifth-303.html -F 'c_mail=' -F 'c_name=Administrator' \
    -F 'c_date=2015-03-03T15:12:00Z' \
    -F 'c_link=http://users.example.com/admin/' -F 'submit=Submit' \
    -F 'c_filter=pass-through' --form-string \
    'c_text=<p>Administrator comment in <strong>raw</strong> HTML mode.</p>' \
    -F 'c_cookie=yes' -c cookie-jar.txt
chain_curl -F 'wait_version='$((BASE_VERSION + 2)) "${BASE_URL}/test"
if test -z "${STOPPED}"; then
	if ! test -f cookie-jar.txt; then
		echo "Cookie jar not created"
	elif ! grep -qF cGFzcy10aHJvdWdoIEFkbWluaXN0cmF0b3IgIiJodHRwOi8vdXNlcnMuZXhhbXBsZS5jb20vYWRtaW4v cookie-jar.txt; then
		echo "Expected cookie value not found"
	fi
fi
chain /fifth fifth-commented-2.html
chain /fifth/comments fifth-303.html -F 'c_mail=' -F 'c_name=Administrator' \
    -F 'c_date=2015-03-03T15:10:00Z' -F 'c_link=/dev/null' -F 'submit=Submit' \
    -F 'c_filter=html-escape' \
    -F 'c_text=Spam comment with wrong filter'
chain_last_spam spam-wrong-filter.sx
chain /fifth fifth-commented-2.html
LAST_ID=$(curl -s "${BASE_URL}/comment-ids" | tail -n 1)
chain /fifth/comments fifth-303.html -F 'ignore=Ignore' -F "id=${LAST_ID}"
chain_curl -F 'wait_version='$((BASE_VERSION + 3)) "${BASE_URL}/test"
chain /fifth fifth-commented.html
FIFTH_STOPPED="${STOPPED}"

BASE_VERSION=$(get_version)
check /contact contact-0.html
chain /contact/comments contact-303.html \
    -F 'c_mail=nat@localhost' -F 'c_name=nat' \
    -F 'c_note=Comment with all atoms' -F 'c_title=Test' \
    -F 'c_link=http://instinctive.eu/' -F 'submit=Submit' \
    -F 'c_text=Full comment in test'
chain_curl -F 'wait_version='$((BASE_VERSION + 1)) "${BASE_URL}/test"
chain /contact contact-1.html

BASE_VERSION=$(get_version)
STOPPED="${FIFTH_STOPPED}"
chain_curl -F 'next_expire=5' "${BASE_URL}/test"
chain /reload reload-redirect.html -F 'submit=Submit'
chain_curl -F 'wait_version='$((BASE_VERSION + 1)) "${BASE_URL}/test"
chain /fifth fifth-commented.html
chain_curl -F 'wait_version='$((BASE_VERSION + 2)) "${BASE_URL}/test"
