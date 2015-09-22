#!/bin/sh

set -xe

rootdir=$(pwd)/../../
(cd "$rootdir";tar cvzf ebill_$(cd ebill;dpkg-parsechangelog | sed -n -e 's/^Version: //p' | sed -e 's/^[[:digit:]]*://' -e 's/[-].*//').orig.tar.gz --exclude=debian --exclude=.git ebill)
