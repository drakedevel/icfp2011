wget -q -O - http://kokako.kb.ecei.tohoku.ac.jp/teams/ | grep "/teams/" | sed -r 's,[^/]+/teams/([0-9]+).+,\1,' |
while read team; do
	wget -q -O - "http://kokako.kb.ecei.tohoku.ac.jp/teams/${team}" |
	grep -E "<td>[0-9]+</td>|strong" | grep -v "<tr>" |
	sed -r 's/<strong>.+<\/strong>/3/;s/\s+//g' |
	tr -d '\n' |
	sed -r 's,<td>,,g' |
	sed -r 's,([0-9]+)</td>([0-9]+)</td>([0-9]+)</td>,\1\t\2\t\3\n,g' |
	python agg.py ${team}
done
