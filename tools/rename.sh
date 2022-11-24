#!/bin/bash

set -e

PREVIEW=${PREVIEW:-true}

git ls-tree -r HEAD --name-only | \
	 grep '.*[.]R\(md\)\?$' | \
	while read a; do
		echo -e -n "$a"
		d=$(mktemp); dr=$(mktemp)
		cat "$a" | perl -pe 's/(?<![A-Za-z_.])\Q'$1'\E(?![A-Za-z_.])/'$2'/g' > "$d"
                if diff "$a" "$d" > $dr; then
                   echo " ✓"
                else
                   echo -e "\n======================="
                fi
                cat $dr
		if ! $PREVIEW; then
			cat "$d" > "$a"
		fi
	done
if ! $PREVIEW; then
git commit -a -m "Rename $1 -> $2"
fi
