#!/bin/bash
sed -e 's/\([^ ]*\) \([^\.]*\).\([^ ]*\)/<td>\1<\/td><td><a href="\2.hs">\2<\/a>.<a href="\2.\3.tptp">\3<\/a><\/td>/' BigTestResults.md | \
    sed -e 's/.:\([^ ]*\) /<td>\1\<\/td>/g' | \
    sed -e 's/^/<tr>/' | sed -e 's/$/<\/tr>/' | \
    sed -e 's/   */ /' > results-table.html

