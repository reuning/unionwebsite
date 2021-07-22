#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.support.ui import Select
#from webdriver_manager.chrome import ChromeDriverManager
from os.path import abspath
import os
import requests
import time
import traceback
import urllib.parse
import pandas as pd

download_folder = abspath("gen/data")
print(download_folder)
options = Options()
options.add_experimental_option("prefs", {
#   "download.default_directory": download_folder,
   "download.prompt_for_download": True,
#   "download.directory_upgrade": True,
#   "safebrowsing.enabled": True
})
options.add_argument("--headless")
options.add_argument("--no-sandbox")
options.add_argument("--disable-dev-shm-usage")

def open_url(browser, url, tries = 6):
    ii = 0
    while ii < tries:
        try:
            browser.get(url)
            ii = tries
        except:
            ii += 1
            pass
        if ii == (tries - 1):
            raise ValueError("Failed to open url after multiple tries")


def chunks(l, n):
    """Yield n number of striped chunks from l."""
    for i in range(0, n):
        yield i, l[i::n]

def get_split_data(chrome_options, download_text, file_out, splits = 5):
    url_start = r"https://www.nlrb.gov/reports/graphs-data/recent-election-results"
    skip = [11, 17, 23, 24, 26,30]
    districts = [i for i in range(1, 33) if i not in skip]

    for ii, district_chunk in chunks(districts, splits):
        if os.path.isfile(download_folder + "/tmp" + str(ii) + ".csv"):
            continue

        pars = {"r[" + str(i)+ "]" : str(i) for i in district_chunk}
        url = url_start + "?" + urllib.parse.urlencode(pars)
        get_data(chrome_options = chrome_options,
                url = url, download_text= download_text,
                file_out = "tmp" + str(ii) + ".csv")

    combined_csv = pd.concat([pd.read_csv(download_folder + "/tmp" + str(ii) + ".csv") for ii in range(splits) ])
    combined_csv.to_csv(download_folder + "/" +  file_out,  index=False )
    for ii in range(splits):
        os.remove(download_folder + "/tmp" + str(ii) + ".csv")


def get_data(chrome_options,
            url=r'https://www.nlrb.gov/search/case?f[0]=case_type:R&s[0]=Open',
            download_text = "Cases (All Dates)",
            file_out="new_open_data.csv"):

    browser = webdriver.Chrome(options=chrome_options)
    browser.set_page_load_timeout(-1)
    browser.implicitly_wait(30)


    print(f"Opening Page for {url}")
    open_url(browser=browser,  url=url)

    try:
        browser.find_element_by_id("download-button").click()
    except:
        print("Download button not found")
        raise

    browser.implicitly_wait(240)

    ii = 0
    while ii < 5:
        try:
            link = browser.find_element_by_link_text(download_text)
            file_url = link.get_attribute("href")
        except:
            print("Download file never prepped")

        try:
            file = requests.get(file_url)
            ii = 5
        except:
            ii += 1
            print("Downloaded Failed")

        if ii == 4:
            raise ValueError("Failed download after multiple tries")


    with open(download_folder + "/" +  file_out,'wb') as f:
      f.write(file.content)
    print("File Saved")

    return True

check = 0
while check < 5:
    try:
        print("Attempt " + str(check + 1))
        get_data(chrome_options = options)
        check = 11

    except Exception:
        print(traceback.format_exc())
        check += 1
        time.sleep(5)


check = 0
while check < 7:
    try:
        print("Attempt " + str(check + 1))
        get_split_data(chrome_options = options,
                download_text= "Recent Election Results (All Dates)",
                file_out="temp.csv")
        check = 11

    except Exception:
        print(traceback.format_exc())
        check += 1
        time.sleep(5)
