#!/usr/bin/env python
# -*- coding: utf-8 -*-

import everysport
import csv
import os.path
import sys

# TODO: Store the API key in an environment variable
APIKEY = "e293caf28745f51e6c38e2eb30cb489d" 

# Use the Python wrapper for the Everysport API
# Documentation: https://github.com/peterstark72/everysport
es = everysport.Everysport(APIKEY)

# Store league ids here
leagues = {
    "Allsvenskan - herr 2008-2013": [57973,51603,44165,38686,32911,27773]
}

# Store stadings for each round so that we don't have to call the standings API for every game
standings = {}

# ==========================================
#                  FUNCTIONS
# ==========================================

# Returns a dictionary of the given round with the team id as key and the standing as value
def getStandingInRound(leagueId, r):
    print "Get standing for round %s in league %s" % (r, leagueId)
    standing = {}
    standing['round'] = r
    pos = 1
    resp = es.get_standings(leagueId, r=r)
    if len(resp) > 0:
        for d in resp[0]['standings']:
            teamId = d['team']['name'].encode("utf-8")
            standing[teamId] = pos
            pos = pos + 1
            # For more detailed data (eg. goals, wins, loss, ties) we could parse the stats dict in d
        return standing
    else:
        # Return false if there is no data for the given round
        return False


# Returns a dictionary with game data
def getDataFromGame(game):
    row = {}
    row['omgang'] = game['round']
    leaugeId = game['league']['id']
    roundId = "%s-%s" % (leaugeId, row['omgang'])

    if roundId not in standings:
        standings[roundId] = getStandingInRound(leaugeId, row['omgang'])

    row['sasong'] = game['league']['startDate']
    row['matchid'] = game['id']
    row['hl_id'] = game['homeTeam']['id']
    row['hl_namn'] = game['homeTeam']['shortName'].encode('utf-8')
    row['hl_slutmal'] = game['homeTeamScore']
    row['bl_id'] = game['visitingTeam']['id']
    row['bl_namn'] = game['visitingTeam']['shortName'].encode('utf-8')
    row['bl_slutmal'] = game['visitingTeamScore']

    # Get position in league unless first round
    if row['omgang'] is not 1:
        row['hl_pos'] = standings[roundId][row['hl_name']]
        row['bl_pos'] = standings[roundId][row['bl_name']]
    else:
        row['hl_pos'] = 7
        row['bl_pos'] = 7

    print "Game: %s vs %s: %s-%s (round %s)" % (row['hl_namn'], row['bl_namn'], row['hl_slutmal'], row['bl_slutmal'], row['omgang'])
    return row


# Takes a list of leagues and returns the a list of games 
def getDataFromLeagues(leagueIds):
    data = []
    for leagueId in leagueIds:
        league = es.events.leagues(leagueId)
        print "Getting data from league %s" % leagueId
        for game in league:
            gameData = getDataFromGame(game)
            data.append(gameData)
    return data

# Takes a list of dictionaries and writes to a given csv file
def writeListToCsv(data, outputFile):
    def writeToCsv():
        cols = data[0].keys()
        f = open(outputFile, 'wb')
        dict_writer = csv.DictWriter(f, cols)

        # Write column names
        dict_writer.writer.writerow(cols)

        # Write rows
        dict_writer.writerows(data)
        print "Write to file: %s" % outputFile

    if os.path.isfile(outputFile):
        ask = raw_input("File %s already exist. Do you want to overwrite? [y/n]" % outputFile)
        if ask == "y":
            writeToCsv()
    else:
        writeToCsv()


# Takes a list of leagues and returns the positions round by round for every team
# If folder is defined it writes a csv file for every season
def getHistoricalPositions(leagueIds, folder):
    data = {}
    # Iterate leagues
    for leagueId in leagueIds:
        data[leagueId] = []
        r = 1
        # Get the standing of the next round, if no next round (= season has ended) 
        # getStandingInRound() returns False
        nextRound = getStandingInRound(leagueId, r)
        while nextRound:
            data[leagueId].append(nextRound)
            r = r + 1
            nextRound = getStandingInRound(leagueId, r)

    # Iterate collected data and write to file
    if (folder):
        for leagueId in data.keys():
            fileName = "%s/%s.csv" % (folder, leagueId)
            writeListToCsv(data[leagueId], fileName)

    return data


def getDictFromCsv(fileName):
    data = []
    for row in csv.DictReader(open(fileName)):
        data.append(row)
    return data


# Adds a form column to the dataset of games
def getTeamForm(data):
    pointsInRound = {}

    def getForm(r, team, season):
        form5 = 0
        form10 = 0
        _r = r - 1
        while (_r > r - 10 and _r > 0):
            points = pointsInRound[season][_r][team]
            if r - _r <= 5:
                form5 = form5 + points
            form10 = form10 + points
            _r = _r - 1

        return {"form5": form5, "form10": form10}

    # Iterate all games to and create a temporary object with points per game
    for game in data:
        hl = game['hl_namn']  # Home team
        bl = game['bl_namn']  # Visiting team
        r = int(float(game['omgang']))  # Round
        season = game['sasong'][0:4]  # Season
        if season not in pointsInRound:
            pointsInRound[season] = {}
        if r not in pointsInRound[season]:
            pointsInRound[season][r] = {}

        # Let tie be 0, win 1 and loss -1
        # Only positive values would distort form variable in early rounds
        if game['hl_slutmal'] == game['bl_slutmal']:
            pointsInRound[season][r][hl] = 0
            pointsInRound[season][r][bl] = 0
        elif game['hl_slutmal'] > game['bl_slutmal']:
            pointsInRound[season][r][hl] = 1
            pointsInRound[season][r][bl] = -1
        else:
            pointsInRound[season][r][hl] = -1
            pointsInRound[season][r][bl] = 0

    for i, game in enumerate(data):
        r = int(float(game['omgang']))
        print r
        season = game['sasong'][0:4]  # Season
        hl_form = getForm(r, game['hl_namn'], season)
        bl_form = getForm(r, game['bl_namn'], season)
        data[i]['hl_form5'] = hl_form['form5']
        data[i]['hl_form10'] = hl_form['form10']
        data[i]['bl_form5'] = bl_form['form5']
        data[i]['bl_form10'] = bl_form['form10']

    return data







# ==========================================
#                  DO STUFF
# ==========================================

# To get data from one or more specific leagues you need the league ids in a list
# Eg. getDataFromLeague([57973]) returns a list of all the games in Allsvenskan 2013

'''
getHistoricalPositions([57973,51603,44165,38686,32911,27773], "../Our data/Historical standings")
'''

'''
writeListToCsv(
    getDataFromLeagues(leagues["Allsvenskan - herr 2008-2013"]), 
    "../Our data/matchdata - allsvenskan 2008-2013.csv"
)
'''

data = getDictFromCsv("../Our data/matchdata - allsvenskan 2008-2013.csv")
dataWithForm = getTeamForm(data)

writeListToCsv(
    dataWithForm,
    "../Our data/matchdata - allsvenskan 2008-2013 + form.csv"
)


# ==========================================
#                  TO DO
# ==========================================

# - Check if data exist before scraper is initiated
# - Define leagues in a more accessible way for our own use
# - Improve commenting on form function