#!/usr/bin/env python3
# -*- coding: utf-8 -*-



from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.support.ui import Select
#from webdriver_manager.chrome import ChromeDriverManager
from os.path import abspath
import requests
import time
import traceback


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

def get_data(chrome_options,
            url=r'https://www.nlrb.gov/search/case?f[0]=case_type:R&s[0]=Open',
            download_text = "Cases (All Dates)",
            file_out="new_open_data.csv"):

    browser = webdriver.Chrome(options=chrome_options)
    browser.set_page_load_timeout(-1)
    browser.implicitly_wait(240)


    print(f"Opening Page for {url}")
    browser.get(url)

    try:
        browser.find_element_by_id("download-button").click()
    except:
        print("Download button not found")
        raise


    ii = 0
    while ii < 5:
        try:
            link = browser.find_element_by_link_text(download_text)
            file_url = link.get_attribute("href")
        except:
            ii += 1
            print("Download file never prepped")

        try:
            file = requests.get(file_url)
            ii = 5
        except:
            ii += 1
            print("Downloaded Failed")

        if ii == 4:
            raise


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
while check < 5:
    try:
        print("Attempt " + str(check + 1))
        get_data(chrome_options = options,
                url=r"https://www.nlrb.gov/reports/graphs-data/recent-election-results",
                download_text= "Recent Election Results (All Dates)",
                file_out="temp.csv")
        check = 11

    except Exception:
        print(traceback.format_exc())
        check += 1
        time.sleep(5)
