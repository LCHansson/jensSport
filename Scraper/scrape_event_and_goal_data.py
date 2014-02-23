#!/usr/bin/env python
# -*- coding: utf-8 -*-
'''Example use of everysport.events queries'''

import os
import everysport
import urllib2
import json
import pprint
import csv
import time
import glob
import re

pp = pprint.PrettyPrinter(indent=2)
# The columns that we will store for each goal
colsGoals = [
    'gameId',
    'round',
    'homeTeam', 
    'homeTeamId',
    'homeTeamScore',
    'homePos',
    'visitingTeam',
    'visitingTeamId',
    'visitingTeamScore',
    'visitingPos',
    'date',
    'league',
    'leagueId',
    'player',
    'minute',
    'team',
    'homeGoals',
    'visitingGoals',
    'homePlayers',
    'visitingPlayers',
    'homeWarnings',
    'visitingWarnings'
]

# The columns that we will store for each game
colsEvents = [
    'gameId',
    'round',
    'homeTeam', 
    'homeTeamId',
    'homeTeamScore',
    'homePos',
    'visitingTeam',
    'visitingTeamId',
    'visitingTeamScore',
    'visitingPos',
    'date',
    'league',
    'leagueId',
    'leagueGender',
    'round',
    'spectators',
    'arenaId',
    'arenaName',
    'referees'    
]
# A list of league id's from Everysport.com
# https://docs.google.com/spreadsheet/ccc?key=0AojPWc7pGzlMdGRLZmw3d2Vybmxfa0NfaWR5alM0UkE&usp=drive_web#gid=0
#leagues = [57973,51603,44165,38686,32911,27773,21511,10581,9402,8051,6469,5307,3863,2752,44255,44254,51604,44174,38687,32912,27774,21512,10609,9401,8052,6470,5308,3864,2753,51606,44175,38689,32914,27981,21692,10607,1721,51605,44176,38688,32913,27982,21691,10608,51625,44435,38968,32895,27775,21684,10582,9347,8011,6489,5327,3883,2772,1740,55198,48300,19308,15883,14060,11329,9555,8480,6575,1244,670,610,420,294,208,19309,15884,14061,11330,9598,8483,1252,671,529,429,297,214,58850,48304,42514,36190,55197,48305,42515,36191,19313,15888,14063,11332,9721,8485,6579,693,530,421,295,210,19307,15882,14064,11333,9722,8484,6581,1243,694,533,427,299,209,19317,15892,14062,11331,9688,8482,1247,695,527,428,296,211]
# Allsvenskan, Superettan, Div 1 Norra, Södra
#sv_leagues = [57973,51603,44165,38686,32911,27773,21511,10581,9402,8051,6469,5307,3863,2752,44255,44254,51604,44174,38687,32912,27774,21512,10609,9401,8052,6470,5308,3864,2753,51606,44175,38689,32914,27981,21692,10607,1721,51605,44176,38688,32913,27982,21691,10608]

# League id's
leagues = {
    "Allsvenskan - herr": [57973,51603,44165,38686,32911,27773,21511,10581,9402,8051,6469,5307,3863,2752,1719,44254,44255]
}

# Get existing leagues to check if data is already collected
files = glob.glob("data_goals/*.csv")
existingFiles = []
for file in files:
    leagueId = int(re.findall("data_goals/(\d*).csv",file)[0])
    existingFiles.append(leagueId)

# FUNCTION: Get position in league of home and visiting team at given round
def getPositionInLeague(league, round, home, visiting):
    print "Get position in league"
    standing_url = "http://api.everysport.com/v1/leagues/{0}/standings/?apikey=e293caf28745f51e6c38e2eb30cb489d&round={1}&size=small".format(league,round)
    print standing_url
    response = urllib2.urlopen(standing_url)
    standings = json.load(response)['groups'][0]['standings']
    d = {}
    pos = 1
    for team in standings:
        if team['team']['id'] == home:
            d['home'] = pos
        if team['team']['id'] == visiting:
            d['visiting'] = pos
        pos += 1
    
    return d
    
# FUNCTION: Get all goals from event
def getGoalsFromEvent(ev, league):
    print "Scarping data from event %s" % ev["id"]
    game_events = ev["gameEvents"]
    row = {}
    
    # Get game meta data
    row['gameId'] = ev['id']
    row['homeTeam']  = ev['homeTeam']['shortName'].encode("utf-8")
    row['homeTeamId'] = ev['homeTeam']['id']
    row['homeTeamScore'] = ev['homeTeamScore']
    row['visitingTeam'] = ev['visitingTeam']['shortName'].encode("utf-8")
    row['visitingTeamId'] = ev['visitingTeam']['id']
    row['visitingTeamScore'] = ev['visitingTeamScore']
    row['date'] = ev['startDate']
    row['league'] = ev['league']['name'].encode("utf-8")
    row['leagueId'] = ev['league']['id']
    row['leagueGender'] = ev['league']['teamClass'].encode("utf-8")
    row['round'] = ev['round']
    pos = getPositionInLeague(league, row['round'], row['homeTeamId'], row['visitingTeamId'])
    row['homePos'] = pos['home']
    row['visitingPos'] = pos['visiting']
    
    # Get spectators and arena info, if defined
    if 'facts' in ev:
        if 'spectators' in ev['facts']:
            row['spectators'] = ev['facts']['spectators']
        else:
            row['spectators'] = ""
        if 'arena' in ev['facts']:
            row['arenaId'] = ev['facts']['arena']['id']
            row['arenaName'] = ev['facts']['arena']['name'].encode("utf-8")
        else:
            row['arenaId'] = ""
            row['arenaName'] = ""
    else:
        row['spectators'] = ""
        row['arenaId'] = ""
        row['arenaName'] = ""
        
    # Get referees
    if 'referees' in ev:
        row['referees'] = ",".join(ev['referees']).encode("utf-8")
    else:
        row['referees'] = ""

    # Get goal data
    dataEvents.append(row.copy())
    row['homeGoals'] = 0
    row['visitingGoals'] = 0
    dataGoals.append(row.copy())
    players = { "home": 11, "visiting": 11 }
    warnings = { "home": 0, "visiting": 0 }
    goals = { "home": 0, "visiting": 0 }

    for ge in game_events:
        # We can only get goal data from the games with detailed event data
        if ge['level'] == 'DETAILED':
            team = 'home' if ge['team']['id'] == row['homeTeamId'] else "visiting"
            row['team'] = team
        
            # Event: warning
            if ge['type'] == 'WARNING':
                warnings[team] += 1
    
            # If a player is sent off the pitch, reduce the number of players in the team
            if ge['type'] == 'PENALTY':
                players[team] -= 1
            
            # Event: Goal
            if ge['type'] == 'GOAL':
                goals[team] += 1
                if 'player' in ge: 
                    row['player'] = ge['player']['name'].encode("utf-8")
                else:
                    row['player'] = ''
                if 'minute' in ge:
                    row['minute'] = ge['minute']
                else:
                    row['minute'] = ""
                row['homeGoals'] = goals['home']
                row['visitingGoals'] = goals['visiting']
                row['homeWarnings'] = warnings['home']
                row['visitingWarnings'] = warnings['visiting']
                row['homePlayers'] = players['home']
                row['visitingPlayers'] = players['visiting']
            
                dataGoals.append(row.copy())

missing = []
errors = []
numberOfGames = 1000


# START HERE
# Iterate leagues
print "::::::::::: Start scraping leagues :::::::::::::"
for league in leagues.keys():
    for leagueId in leagues[league]:
    	print "Scraping league with id %s" % leagueId
        dataGoals = []
        dataEvents = []
        time.sleep(3)

        # Check if leagues in already scrapes
        if league not in existingFiles:
            league_url = "http://api.everysport.com/v1/events/?apikey=e293caf28745f51e6c38e2eb30cb489d&limit={0}&league={1}".format(numberOfGames, leagueId) # Last 3 games, Allsvenskan
            print "League url: %s" % league_url
            response = urllib2.urlopen(league_url)
            response_json = json.load(response)
     
            # Check if league has events
            if "events" in response_json:
                events = response_json['events']

                # Iterate all events in league
                c = 0
                for e in events:
                    id = e["id"]
                    endDate = "2014-01-01"
                    eventUrl = "http://api.everysport.com/v1/events/{0}/?apikey=e293caf28745f51e6c38e2eb30cb489d&toDate={1}&fields=all".format(id,endDate)
                    print "Event url: %s:" % (eventUrl)
                    
                    try: 
                        response = urllib2.urlopen(eventUrl)
                        ev = json.load(response)["event"]
                        if "gameEvents" in ev:
                            # Get data from event
                            getGoalsFromEvent(ev, leagueId)
            
                        # Event details missing
                        else:
                            missing.append(ev['id'])
                    except:
                        print "ERROR: Could not scrape %s" % (eventId)
                        errors.append({ 'url': eventUrl, 'league': leagueId })
                        writerErrors = csv.writer(open('error_log_live.csv'.format(leagueId), 'a'))
                        writerErrors.writerow([eventUrl, leagueId])
        
                # Write to file: Goals
                writerGoals = csv.writer(open('data_goals/{0}.csv'.format(leagueId), 'wb'))
                writerGoals.writerow(colsGoals)
                for row in dataGoals:
                    _r = []
                    for col in colsGoals:
                        if col in row:
                            _r.append(row[col])
                        else:
                            _r.append("")    
                    print _r
                    writerGoals.writerow(_r)

                # Write to file: Events                
                writerEvents = csv.writer(open('data_events/{0}.csv'.format(leagueId), 'wb'))
                writerEvents.writerow(colsEvents)
                for row in dataEvents:
                    _r = []
                    for col in colsEvents:
                        _r.append(row[col])    
                    print _r
                    writerEvents.writerow(_r)
            else:
                print "ERROR: Did not find any events in %s"  % (leagueId)

        else:
            print "League %s is already scraped" % (leagueId)


print "Done!"

# Print and store error log
print errors
try:
    # This will create a new file or **overwrite an existing file**.
    f = open("error_log.txt", "w")
    try:
        f.write(errors) # Write a string to a file
    finally:
        f.close()
except IOError:
    pass
