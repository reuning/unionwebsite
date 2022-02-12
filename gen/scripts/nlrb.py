## Modified from https://github.com/labordata/nlrb-cases
import datetime
import scrapelib
import tqdm
from os.path import abspath
from time import sleep
import urllib.parse

from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.support.ui import Select

OPTIONS = Options()

OPTIONS.add_argument("--headless")
OPTIONS.add_argument("--no-sandbox")
OPTIONS.add_argument("--disable-dev-shm-usage")


DOWNLOAD_FOLDER = abspath("gen/data")
BASE_URL = 'https://www.nlrb.gov'
HEADER = {'user-agent':'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_2) AppleWebKit/601.3.9 (KHTML, like Gecko) Version/9.0.2 Safari/601.3.9'}

def start_data(search_url, params=None):
    browser = webdriver.Chrome(options=OPTIONS)
    browser.set_page_load_timeout(-1)
    browser.implicitly_wait(15)

    if params is None:
        browser.get(BASE_URL + search_url)
    else:
        browser.get(BASE_URL + search_url + "?" + urllib.parse.urlencode(params))

    download_link = browser.find_element_by_id('download-button')
    payload = {'cacheId': download_link.get_attribute('data-cacheid'),
               'typeOfReport': download_link.get_attribute('data-typeofreport'),
               'token': str(datetime.datetime.now())}

    scraper = scrapelib.Scraper(retry_attempts=20)
    response = scraper.post('https://www.nlrb.gov/nlrb-downloads/start-download/' +
                            payload['typeOfReport'] + '/' + payload['cacheId'] +
                            '/' + payload['token'])

    result = response.json()['data']
    return result

def get_data(file_name, search_url,
                params = None ):

    attempts = 0
    while True:
        print("Attempt " + str(attempts + 1))
        try:
            result = start_data(search_url=search_url, params=params)
            break
        except:
            attempts += 1
            sleep(10)
            if attempts > 40:
                print("Unable to download")
                raise



    previous = 0
    s = scrapelib.Scraper(retry_attempts=20)

    with tqdm.tqdm(total=result['total'], desc='NLRB.gov preparing download') as pbar:
        while not result['finished']:
            response = s.get(BASE_URL + '/nlrb-downloads/progress/' + str(result['id']))
            result = response.json()['data']

            # update progress bar
            current = result['processed']
            pbar.update(current - previous)
            previous = current
            sleep(2)


    print(BASE_URL + result['filename'])
    attempts = 0
    while True:
        file_out = s.get(BASE_URL + result['filename'])
        if file_out.content[0:6] == b'Region' or file_out.content[0:5] == b'"Case':
            break
        else:
            if attempts > 30:
                Exception("Cannot download")
            attempts +=1
            sleep(10)


    with open(DOWNLOAD_FOLDER + "/" +  file_name,'wb') as f:
          f.write(file_out.content)


get_data(search_url = '/reports/graphs-data/recent-election-results',
        file_name= "temp.csv")

sleep(5)
get_data(search_url = '/search/case',
        params={"f[0]":"case_type:R"},
        file_name= "new_data.csv")
