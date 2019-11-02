import requests
import pandas as pd
import datetime
import json

def getdf():
    data = pd.DataFrame(columns = ['id', 'answer', 'question', 'value', 'airdate', 'created_at', 'updated_at',
                                   'category_id', 'game_id', 'invalid_count','clues_count'])
    querystring = {
        "offset": 0
    }
    resp = requests.request("GET", url='http://jservice.io/api/clues', params=querystring)
    if resp.status_code != 200: # This means something went wrong.
        raise ('GET /tasks/ {}'.format(resp.status_code))
    ind = 0
    while ind<=156800: # number of entries in database
        for dict in resp.json():
            titles = ""
            clues = ""
            if 'category' in dict:
                titles = dict['category']['title']
                clues = dict['category']['clues_count']
            # add all data to a pandas dataframe
            data = data.append({'id':dict['id'], 'answer':dict['answer'], 'question':dict['question'], 'value':dict['value'],
                         'airdate':dict['airdate'],
                        'created_at':dict['created_at'], 'updated_at':dict['updated_at'],
                        'category_id':dict['category_id'], 'game_id':dict['game_id'],
                         'invalid_count': dict['invalid_count'],
                        'title': titles, 'clues_count': clues},
                        ignore_index=True)
        ind += 100 # since API call only pulls in sets of 100
        querystring = {
            "offset": ind
        }
        print(ind)
        resp = requests.request("GET", url = 'http://jservice.io/api/clues', params=querystring)
    data.to_csv('origdata.csv')

def getcsv():
    data = pd.read_csv("origdata.csv")
    # data = data.drop('Unnamed: 0', axis=1)
    return data

def cleandates(data):
    # clean time stamps
    data['airdate2'] = data['airdate'].apply(dates) # convert to simpler datetime strings
    return data

def dates(row):
    # the following function converts the complex datetime stamps into a cleaner format
    if row=="nan" or type(row)==float:
        return row
    else:
        d1 = datetime.datetime.strptime(row, "%Y-%m-%dT%H:%M:%S.%fZ")
        new_format = "%Y-%m-%d"
        d1.strftime(new_format)
    return d1

def cleanquestions(data):
    # remove <i> from answers
    data['answer'] = data['answer'].apply(italics)
    return data

def italics(row):
    a = row
    if row!="nan" and type(row)==float:
        return a
    if "<i>" in row or "</i>" in row:
        a = map(lambda x: x.replace('<i>', '').replace('</i>', ''), row)
    return a

data = getcsv()
data = cleandates(data)
print(type(data))
data.to_csv("data.csv") # final dataframe