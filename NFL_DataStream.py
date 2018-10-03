import re
import os
import time
import subprocess
from datetime import datetime, timedelta
from bs4 import BeautifulSoup
import inflect 


YEAR = [2015,2016,2017,2018] 
positionHeaders = {}


def ClearSpecialCharacters(data, header):
    if header:
        data = data.replace('%', 'per')
    data = re.sub('[^a-zA-Z0-9 \n\.\s]', '', data)
    data = data.replace(' ', '')
    return data


# Extract team urls and names
def ExtractTeams():
    for year in YEAR:
        teamData = subprocess.check_output(['curl', 'https://www.pro-football-reference.com/teams/'], shell = True)
        soup = BeautifulSoup(teamData[10000:], features='html.parser')
        teamDiv = soup.find('div', {'id': 'div_teams_active' })
        teams =  teamDiv.findAll('a')
        for each in teams:
            if re.match('.*<a href="/teams/.*', str(each)):	
                print(each['href'])
                ExtractPlayersFromRoster(each['href'], each.text, year)

# Extract Players from team Roster
def ExtractPlayersFromRoster(teamURL, teamName, year):
    teamURL = 'https://www.pro-football-reference.com' + teamURL + str(year) + '_roster.htm' 
    print(teamURL)
    teamRoster = subprocess.check_output(['curl' , teamURL], shell = True)
    soup = BeautifulSoup(teamRoster[10000:], features='html.parser')
    teamDiv = soup.find('div', {'id': 'div_starters' })
    teamPlayers = teamDiv.findAll('tr', {'class': 'full_table'})
    
    for each in teamPlayers:
        position = ""
        player = ""
        playerLink = ""
        if (len(each.findAll('th')) > 0):
            position =  each.findAll('th')[0]
            position = position.text


        player = each.findAll('td')
        if (len(player) > 0):
            player = player[0]
            playerLink = player.find('a')['href']
            player = player.text
            

        if len(position) > 0 and len(player) > 0:
            player = ClearSpecialCharacters(player, False)
            ExtractPlayersData(player, position, playerLink, year)



def ExtractPlayersData(player, playerPosition, playerLink, year):
    playerLink = playerLink.split('.')[0]
    playerGameLog = 'https://www.pro-football-reference.com' + playerLink + '/gamelog/' + str(year) +'/'
    playerGameLog = subprocess.check_output(['curl' , playerGameLog], shell = True)
    soup = BeautifulSoup(playerGameLog[10000:], features='html.parser')
    playerGameLogData = soup.find('div', {'id': 'div_stats'}) 


    if (playerGameLogData == None):
        return 0
    # Header information for the tables    
    playerGameLogDataHeader = playerGameLogData.find('thead')
    header = {}
    p = inflect.engine()

    # Header Titles
    if (len(playerGameLogDataHeader.findAll('tr')) > 0):
        colSpan = 0        
        for eachHeader in (playerGameLogDataHeader.findAll('tr')[0]).findAll('th'):
                headerName = ""
                if (len(str(eachHeader.text)) > 0):
                    headerName = ClearSpecialCharacters(eachHeader.text, True)
                    
                else:
                    length = p.number_to_words(len(header) + 1)
                    headerName = length

                header[headerName] = 0
                if ('colspan' in str(eachHeader)):
                        colSpan = colSpan + int(eachHeader['colspan'])
                        header[headerName] = colSpan
    
    # Header Main
    headerMain = []
    if (len(playerGameLogDataHeader.findAll('tr')) > 1):
        for eachHeader in (playerGameLogDataHeader.findAll('tr')[1]).findAll('th'):
                headerName = ""
                if (len(str(eachHeader.text)) > 0):
                    headerName = eachHeader.text
                else:
                    length = p.number_to_words(len(header) + 1)
                    # input(length)
                    headerName = length

                ### Clear Special charcters
                headerName = ClearSpecialCharacters(headerName, True)
                headerMain.append(headerName) 

    # Find key from headerlist to append to main headers                
    def FindKey(header, index):

        for key, value in header.items(): 
            if index <= value:
                return str(key)

        return header.keys()[-1]

    # Combine headers
    for i in range(0, len(headerMain)):
        headerMain[i] = str((FindKey(header, i))) + headerMain[i]
    
    headerMain.insert(0, "PlayerPosition")
    headerMain.insert(0, "PlayerName")
    # print(','.join(headerMain))

    positionHeaders[playerPosition] = ','.join(headerMain)
    fileToWrite = open('tableInsert.csv', 'a+')
    InsertIntoTable =  'INSERT INTO NFLTABLEDATA ('  + str(','.join(headerMain)) + ') VALUES \n'
    fileToWrite.write(InsertIntoTable) 
    count = 0        
    # Body of the Gamelog
    playerGameLogDataBody = playerGameLogData.find('tbody')
    for eachTD in playerGameLogDataBody.findAll('tr'):
        if count == 0:
            playerData =  '(\'' + player + '\''  + ',' + '\'' +  playerPosition + '\'' +  ','
        else:
            playerData =  ',(\'' + player + '\''  + ',' + '\'' +  playerPosition + '\'' +  ','        
        playerData = playerData + '\'' + str(count) + '\'' 
        for each in eachTD.findAll('td'):
            playerData = playerData + ',' + '\'' + ClearSpecialCharacters(each.text, False) + '\''

        playerData = playerData + ') \n'        
        # input(playerData)
        fileToWrite.write(playerData)

        count += 1
    fileToWrite.write(';')
    fileToWrite.close()
    return 0

ExtractTeams()