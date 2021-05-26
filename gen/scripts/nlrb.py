#!/usr/bin/env python3
# -*- coding: utf-8 -*-



from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.support.ui import Select
#from webdriver_manager.chrome import ChromeDriverManager
from os.path import abspath
import requests



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

browser = webdriver.Chrome(options=options)
browser.set_page_load_timeout(-1)
browser.implicitly_wait(120)


print("Opening Page")
browser.get(r'https://www.nlrb.gov/search/case?f[0]=case_type:R&s[0]=Open')
browser.find_element_by_id("download-button").click()

link = browser.find_element_by_link_text("Cases (All Dates)")

file_url = link.get_attribute("href")
file = requests.get(file_url)
print("File Downloaded")

with open(download_folder + '/new_open_data.csv','wb') as f:
  f.write(file.content)
print("File Saved")
