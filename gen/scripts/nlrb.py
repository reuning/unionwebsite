#!/usr/bin/env python3
# -*- coding: utf-8 -*-



from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.support.ui import Select
from os.path import abspath


download_folder = abspath("../data")
options = Options()
options.add_experimental_option("prefs", {
  "download.default_directory": download_folder,
  "download.prompt_for_download": False,
  "download.directory_upgrade": True,
  "safebrowsing.enabled": True
})
options.headless = True

browser = webdriver.Chrome(options=options)
browser.set_page_load_timeout(-1)
browser.implicitly_wait(30)



browser.get(r'https://www.nlrb.gov/search/case?f[0]=case_type:R&s[0]=Open')
browser.find_element_by_id("download-button").click()
