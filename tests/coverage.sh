#!/bin/sh

# Copyright (c) 2014-2017, Natacha Porté
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

: ${GNATPATH:=/usr/local/gcc6-aux/bin}
: ${LCOV_DATA:=coverage/test-info.dat}
: ${LCOV_HTML_DIR:=coverage}
: ${TEST_LOG:=coverage.log}

PATH=${GNATPATH}:${PATH} gprbuild -p -Ptests -XMODE=Coverage || exit $?
lcov --directory coverage/obj --zerocounters

sh "$(dirname "$0")/run.sh" coverage/bin/simple_site | tee tests/coverage.log || exit $?

lcov --gcov-tool ${GNATPATH}/gcov --directory coverage/obj --output "${LCOV_DATA}" --capture || exit $?
genhtml --output-dir "${LCOV_HTML_DIR}" --prefix "${PWD}" "${LCOV_DATA}"
