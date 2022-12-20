import pandas as pd 
import requests
import json 
import re
import time 
import random
from bs4 import BeautifulSoup
from tqdm.auto import tqdm
from pyproj import Transformer
import numpy as np

TRANS = Transformer.from_crs("EPSG:4326", "EPSG:2855", always_xy=True)


HEADER = {"x-requested-with": "XMLHttpRequest"}
URL = "https://www.starbucks.com/bff/locations"
r = requests.get("https://www.starbucks.com/bff/locations?mop=true&place=Chicago", headers=header)
def clean_store_json(json: dict) -> pd.DataFrame:
    ## Takes store json and cleans it returning all stores in a pandas dataframe.
    stores = json['stores']
    DEL_DICT = ['recommendation','schedule', 
                'timeZoneInfo', 'open', 'openStatusText',
                'warnings', 'mop', 'features', 
                'addressLines', 'curbside']


    data_out = []
    for store in stores:
        for key in DEL_DICT:
            _ = store.pop(key, None)
        data_out.append(store)

    data = pd.json_normalize(data_out, sep="_")
    return(data)
    

def distance(x, y, centers: list):
    dist = []
    for center in centers:
        dist.append(np.sqrt(((center[0] - x) ** 2) + ((center[1] - y) ** 2)))

    return np.mean(dist)    


class sb_city:
    def __init__(self, city):
        self.city_search = city
        self.params = {"place":city, "mop":"true",}
        self.city = None
        self.loc = None
        self.centers = None
        self.data = None

    def scrape_stores(self):
        r = requests.get(URL, params=self.params, headers=HEADER)
        
        if self.loc is None:
            self.loc = r.json()['coordinates']

        data = clean_store_json(r.json())

        data[['x', 'y']] =  data.apply(lambda x: TRANS.transform(x['coordinates_longitude'],
                 x['coordinates_latitude']), axis=1, result_type="expand")

        if self.data is None:
            self.data = data
        else: 
            print("here")
            self.data = pd.concat([self.data, data], ignore_index=True).drop_duplicates("id")

        return("Data Downloaded")

    def update_search(self):
        if self.city is None:
            self.city = self.data['address_city'].mode()[0]
        
        if self.centers is None:
            self.centers = [TRANS.transform(self.loc['lng'], self.loc['lat'])]
        else: 
            self.centers.append(TRANS.transform(self.loc['lng'], self.loc['lat']))

        local_data = self.data[self.data["address_city"]==self.city].copy()
        local_data['distance'] = local_data.apply(lambda x: distance(x['x'], x['y'], self.centers), axis=1 )
        furthest = local_data.loc[local_data['distance'].idxmax()]
        self.loc = {'lat':furthest['coordinates_latitude'], 
                    'lng':furthest['coordinates_longitude']}
        
        self.params.update(self.loc)



detroit = sb_city("Detroit, MI")    
detroit.scrape_stores()
detroit.update_search()
detroit.scrape_stores()



d = detroit.local['distances'].idxmax()
detroit.local.loc[d]['coordinates_latitude']

[TRANS.transform(detroit.loc['lng'], detroit.loc['lat'])]

def find_store_jsons(site):
    ## Takes the site and finds the json 
    soup = BeautifulSoup(site, features="lxml")
    scripts = soup.find_all("script")
    scripts = [script.string for script in scripts if script.string is not None]
    scripts = [script for script in scripts if "window.__BOOTSTRAP" in script ]
    if len(scripts)==1:
        script = scripts[0] 
    else: 
        Exception 

    jsons = re.findall("= (.*)", string=script)
    data = json.loads(jsons[0])
    stores = data['storeLocator']['locationState']['locations']

    return(stores)

URL = "https://www.starbucks.com/store-locator" 
PARS = {"place":"Oxford, Ohio", 
        "mop":"true", 
        "lat":39.5069974,
        "lng":-84.74523099999999}

## Load All city Data
census = pd.read_excel("/Users/kevinreuning//Dropbox/Projects/unionwebsite/gen/data/census_cities.xlsx",  
    skiprows=4, header=None, names=["city", "base", "est_2020", "est_2021"])
census.dropna( inplace=True)
census.sort_values(by="base",ascending=False, inplace=True)

over_50 = 0 
with tqdm(total=990) as t:
    for city in census.city[10:1000]: 
        time.sleep(random.randint(30, 60))
        #print(city)
        PARS['place'] = city
        r = requests.get(url=URL, params=PARS)

        stores = find_store_jsons(r.text)
        data = clean_store_json(stores)
        city_file = city.replace(",", "")
        file_name = f"/Users/kevinreuning//Dropbox/Projects/unionwebsite/gen/data/sb/{city_file}.csv"
        data.to_csv(file_name,index=False)
        
        over_50 += int(data.shape[0] == 50)
        t.set_postfix(city=city, over_50 = over_50)
        t.update()






## Get the city. 
## Find the furthest ones out from the center still in city. 
## Search those lat/long