import requests
from bs4 import BeautifulSoup
import re
import zipfile, io
from tqdm import tqdm

from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.support.ui import Select

OPTIONS = Options()

OPTIONS.add_argument("--headless")
OPTIONS.add_argument("--no-sandbox")
OPTIONS.add_argument("--disable-dev-shm-usage")

browser = webdriver.Chrome(options=OPTIONS)
browser.get("https://olmsapps.dol.gov/olpdr/")


soup = BeautifulSoup(browser.page_source)

reports = soup.find_all(href=re.compile("GetYearlyFileServlet"))


request_cookies_browser = browser.get_cookies()

#making a persistent connection using the requests library
s = requests.Session()

#passing the cookies generated from the browser to the session
c = [s.cookies.set(c['name'], c['value']) for c in request_cookies_browser]

for report in tqdm(reports):
    r = s.get('https://olmsapps.dol.gov' + report.attrs['href'])
    z = zipfile.ZipFile(io.BytesIO(r.content))

    files = z.namelist()
    for file in (filter(re.compile(".*membership.*").match, files)):
        z.extract(file, "gen/data/temp")
