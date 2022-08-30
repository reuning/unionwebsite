## Modified from https://github.com/labordata/nlrb-cases
import datetime
import scrapelib
import tqdm
from os.path import abspath, getsize
from time import sleep
import urllib.parse

from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.support.ui import Select, WebDriverWait
import selenium.webdriver.support.expected_conditions
import selenium.webdriver.common.by

OPTIONS = Options()

OPTIONS.add_argument("--headless")
OPTIONS.add_argument("--no-sandbox")
OPTIONS.add_argument("--disable-dev-shm-usage")
OPTIONS.add_argument("--disable-gpu")
OPTIONS.add_argument("disable-infobars")
OPTIONS.add_argument("--disable-extensions")

DOWNLOAD_FOLDER = abspath("gen/data")
BASE_URL = 'https://www.nlrb.gov'
VERIFY = False ## turn off SSL verification

def start_data(search_url, params=None):
    browser = webdriver.Chrome(options=OPTIONS)
    browser.set_page_load_timeout(-1)
    browser.implicitly_wait(60)

    if params is None:
        browser.get(BASE_URL + search_url)
    else:
        browser.get(BASE_URL + search_url + "?" + urllib.parse.urlencode(params))

    wait = WebDriverWait(browser, 15)
    wait.until(
        selenium.webdriver.support.expected_conditions.presence_of_element_located(
            (selenium.webdriver.common.by.By.ID, "download-button")
        )
    )

    download_link = browser.find_element("xpath", "//a[@id='download-button']")
    for retry in range(4):
        try:
            download_token=browser.get_cookie("nlrb-dl-sessid")["value"]
        except TypeError:
            sleep(1)
            continue
    else:
        download_token=browser.get_cookie("nlrb-dl-sessid")["value"]


    download_link = browser.find_element_by_id('download-button')
    payload = {'cacheId': download_link.get_attribute('data-cacheid'),
               'typeOfReport': download_link.get_attribute('data-typeofreport'),
               'token': download_token}


    scraper = scrapelib.Scraper(retry_attempts=20)
    response = scraper.get('https://www.nlrb.gov/nlrb-downloads/start-download/' +
                            payload['typeOfReport'] + '/' + payload['cacheId'] +
                            '/' + payload['token'], verify=VERIFY)

    result = response.json()['data']
    return result

def get_data(file_name, search_url,
                params = None ):

    result = start_data(search_url=search_url, params=params)
    attempts = 0
    # while True:
    #     print("Attempt " + str(attempts + 1))
    #     try:
    #         result = start_data(search_url=search_url, params=params)
    #         break
    #     except:
    #         attempts += 1
    #         sleep(10)
    #         if attempts > 40:
    #             print("Unable to download")
    #             raise




    previous = 0
    s = scrapelib.Scraper(retry_attempts=20)

    with tqdm.tqdm(total=result['total'], desc='NLRB.gov preparing download') as pbar:
        while not result['finished']:
            response = s.get(BASE_URL + '/nlrb-downloads/progress/' + str(result['id']), verify=VERIFY)
            result = response.json()['data']

            # update progress bar
            current = result['processed']
            pbar.update(current - previous)
            previous = current
            #sleep(2)


    print(BASE_URL + result['filename'])
    attempts = 0
    while True:
        file_out = s.get(BASE_URL + result['filename'], verify=VERIFY)
        if file_out.content[0:6] == b'Region' or file_out.content[0:5] == b'"Case':
            break
        else:
            if attempts > 30:
                Exception("Cannot download")
            attempts +=1
            sleep(10)


    with open(DOWNLOAD_FOLDER + "/" +  file_name,'wb') as f:
          f.write(file_out.content)

tries = 0
while True:
    get_data(search_url = '/reports/graphs-data/recent-election-results?date_start=01%2F01%2F2010&date_end=',
            file_name= "temp.csv")

    if (getsize(abspath("gen/data/temp.csv")) > 1000000):
        break
    if tries > 4:
        raise RuntimeError("Unable to download election results")

    tries += 1
    sleep(5)

sleep(5)
tries = 0
while True:
    get_data(search_url = '/search/case',
            params={"f[0]":"case_type:R"},
            file_name= "new_data.csv")

    if (getsize(abspath("gen/data/new_data.csv")) > 1000000):
        break
    if tries > 4:
        raise RuntimeError("Unable to download cases")

    tries += 1
    sleep(5)
