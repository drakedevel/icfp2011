#!/usr/bin/python
#in this file, rpearl writes bad python

import urllib
from BeautifulSoup import BeautifulSoup

def parseTeam(name,num):
	team = urllib.urlopen("http://kokako.kb.ecei.tohoku.ac.jp/teams/%s" % num)
	team_table = team.read()
	tags = BeautifulSoup(team_table)
	table = tags.find('table')
	duels=0
	wins=0
	if table != None:
		rows = table.findAll('tr')
		for tr in rows:
			cols = tr.findAll('td')
			if len(cols) == 0:
				continue
			duels += int(cols[1].find(text=True))
			wins += int(cols[2].find(text=True))
	pct = wins / float(duels) if duels != 0 else 0.0
	return (num,name,wins,duels,pct)

file="leaderboard.html"
outfile = open(file, "w")

lines = []
outfile.write("<html>\n")
outfile.write("<head>\n")
outfile.write("<title>Scoreboard</title>\n")
outfile.write("</head>\n")
outfile.write("<body>\n")
outfile.write("<table>\n")
teams = urllib.urlopen("http://kokako.kb.ecei.tohoku.ac.jp/teams/")
teams_table = teams.read()
tags = BeautifulSoup(teams_table)
table = tags.find('table')
 
rows = table.findAll('tr')
outfile.write("<tr>\n")
outfile.write("<th>Team #</th>\n")
outfile.write("<th>Team Name</th>\n")
outfile.write("<th>Wins</th>\n")
outfile.write("<th>Duels</th>\n")
outfile.write("<th>PCT</th>\n")
outfile.write("</tr>\n")

team_stats = []
for tr in rows[1:]:
	cols = tr.findAll('th')
	num = cols[0].find(text=True)
	name = cols[1].find(text=True)

	team_stats.append(parseTeam(name, num))

team_stats.sort(key = lambda stat : -stat[4])
for (num,name,wins,duels,pct) in team_stats:
	outfile.write("<tr>\n")
	outfile.write("<td>%s</td><td>%s</td><td>%d</td><td>%d</td><td>%f</td>\n" % (num,name,wins,duels,pct))
	outfile.write("</tr>\n")
outfile.write("</table></body></html>\n")

