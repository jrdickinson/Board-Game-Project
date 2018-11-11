
# coding: utf-8

# In[1]:


# ! pip install boardgamegeek2
import pandas as pd
import csv
from boardgamegeek import BGGClient
import numpy as np
import time
import os


# In[2]:


from bs4 import BeautifulSoup

from urllib.request import urlopen

import requests


# In[25]:


#get list of boardgame names
import re
testset2 = list()
for i in range(1,"insert number of pages"):
    response = requests.get("https://boardgamegeek.com/browse/boardgame/page/" + str(i))

    # Extracting the source code of the page.
    data = response.text

    # Passing the source code to Soup to create a BeautifulSoup object for it.
    soup = BeautifulSoup(data, 'lxml')
    for j in range(1,101):
        for item in soup.findAll("div", {"id":"results_objectname%s"%(j)}):
            testset2.append(int(re.search(r'\d+', (item.find("a")["href"])).group()))


# In[2]:


#save list of names to file
import pickle
os.chdir("C:/Users/jerem.DESKTOP-GGM6Q2I/Documents/UNH Data Analytics/r programming 2/final_project")
# with open("game_names.txt", "wb") as fp:   #Pickling
#     pickle.dump(testset2, fp)
with open("game_names.txt", "rb") as fp:   # Unpickling
    full_list = pickle.load(fp)


# In[1]:


#get board game data
bgg = BGGClient()
datalists1 = list()
start = time.time()
for i in range(0,"number of games"):
    try:
        datalists1.append(bgg.game(game_id=ids[i]))
    except:
        try:
            datalists1.append(bgg.game(game_id=ids[i]))
        except:
            try:
                datalists1.append(bgg.game(game_id=ids[i]))
            except:
                pass
    if i % 50 == 0:
        print(i)
end = time.time()
print(end - start)


# In[2]:


#check size and time
print(start - end)
len(datalists1)


# In[54]:


#create feature lists
name = list()
game_id = list()
artists = list()
categories = list()
description = list()
designers = list()
expands = list()
expansion = list()
expansions = list()
families = list()
mechanics = list()
min_age = list()
publishers = list()
users_owned = list()
users_trading = list()
rating_average = list()
rating_stddev = list()
year = list()
min_players = list()
max_players = list()
min_playing_time = list()
max_playing_time = list()
users_rated = list()
rating_num_weights = list()
rating_average_weight = list()
ranks = list()
for i in datalists1:
    try:
        name.append(i.name)
    except:
        name.append("")
    try:
        game_id.append(i.id)
    except:
        game_id.append(0)
    try:
        artists.append(i.artists)
    except:
        artists.append([])
    try:
        categories.append(i.categories)
    except:
        categories.append([])
    try:
        description.append(i.description)
    except:
        description.append("")
    try:
        designers.append(i.designers)
    except:
        designers.append([])
    try:
        expands.append(i.expands)
    except:
        expands.append([])
    try:
        expansion.append(i.expansion)
    except:
        expansion.append(False)
    try:
        expansions.append(i.expansions)
    except:
        expansions.append([])
    try:
        families.append(i.families)
    except:
        families.append([])
    try:
        mechanics.append(i.mechanics)
    except:
        mechanics.append([])
    try:
        min_age.append(i.min_age)
    except:
        min_age.append(None)
    try:
        publishers.append(i.publishers)
    except:
        publishers.append([])
    try:
        users_owned.append(i.users_owned)
    except:
        users_owned.append(None)
    try:
        users_trading.append(i.users_trading)
    except:
        users_trading.append(None)
    try:
        rating_average.append(i.rating_average)
    except:
        rating_average.append(None)
    try:
        rating_stddev.append(i.rating_stddev)
    except:
        rating_stddev.append(None)
    try:
        year.append(i.year)
    except:
        year.append(None)
    try:
        min_players.append(i.min_players)
    except:
        min_players.append(None)
    try:
        max_players.append(i.max_players)
    except:
        max_players.append(None)
    try:
        min_playing_time.append(i.min_playing_time)
    except:
        min_playing_time.append(None)
    try:
        max_playing_time.append(i.max_playing_time)
    except:
        max_playing_time.append(None)
    try:
        users_rated.append(i.users_rated)
    except:
        users_rated.append(None)
    try:
        rating_num_weights.append(i.rating_num_weights)
    except:
        rating_num_weights.append(None)
    try:
        rating_average_weight.append(i.rating_average_weight)
    except:
        rating_average_weight.append(None)
    try:
        ranks.append(i.ranks)
    except:
        ranks.append([])


# In[57]:


#create dataframe 
df_list = [('name', name),
           ('id', game_id),
           ('description', description),
           ('ranks', ranks),
           ('categories', categories),
           ('families', families),
           ('mechanics', mechanics),
           ('artists', artists),
           ('designers', designers),
           ('publishers', publishers),
           ('expands', expands),
           ('expansion', expansion),
           ('expansions', expansions),
           ('year', year),
           ('users_owned', users_owned),
           ('users_trading', users_trading),
           ('complexity_weight', rating_average_weight),
           ('complexity_num_weights', rating_num_weights),
           ('rating_average', rating_average),
           ('rating_stddev', rating_stddev),
           ('users_rated', users_rated),
           ('min_players', min_players),
           ('max_players', max_players),
           ('min_playing_time', min_playing_time),
           ('max_playing_time', max_playing_time),
           ('min_age', min_age)
         ]
df = pd.DataFrame.from_items(df_list)
df.shape


# In[68]:


#create csv file
os.chdir("C:/Users/jerem.DESKTOP-GGM6Q2I/Documents/UNH Data Analytics/r programming 2/final_project")
df.to_csv("boardgames_full.csv")


# In[59]:


#combine multuple files
import glob
import pandas as pd
path =r"C:/Users/jerem.DESKTOP-GGM6Q2I/Documents/UNH Data Analytics/r programming 2/final_project/folder"
allFiles = glob.glob(path + "/*.csv")
frame = pd.DataFrame()
list_ = []
for file_ in allFiles:
    df= pd.read_csv(file_, index_col=None, header = 0)
    list_.append(df)
frame = pd.concat(list_)


# In[49]:


#check for missing ids
ids = list()
for i in range(0, "# of games"):
    if full_list[i] not in df['id'].unique():
        ids.append(full_list[i])


# In[3]:


len(ids)


# In[4]:


len(frame['id'].unique())


# In[67]:


df = frame.drop_duplicates(subset='id')
df.shape

