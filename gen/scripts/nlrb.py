## Modified from https://github.com/labordata/nlrb-cases
import datetime
import scrapelib
import lxml.html
import tqdm
from os.path import abspath

DOWNLOAD_FOLDER = abspath("gen/data")
BASE_URL = 'https://www.nlrb.gov'

def get_data(file_name, search_url,
                params = None ):
    s = scrapelib.Scraper(retry_attempts=100)

    if params is None:
        response = s.get(BASE_URL + search_url)
    else:
        response = s.get(BASE_URL + search_url, params=params)

    page = lxml.html.fromstring(response.text)
    page.make_links_absolute(search_url)
    download_folder = abspath("gen/data")

    download_link, = page.xpath("//a[@id='download-button']")
    payload = {'cacheId': download_link.get('data-cacheid'),
               'typeOfReport': download_link.get('data-typeofreport'),
               'token': str(datetime.datetime.now())}

    response = s.post('https://www.nlrb.gov/nlrb-downloads/start-download',
                         data=payload)

    result = response.json()['data']
    previous = 0

    with tqdm.tqdm(total=result['total'], desc='NLRB.gov preparing download') as pbar:
        while not result['finished']:
            response = s.get(BASE_URL + '/nlrb-downloads/progress/' + str(result['id']))
            result = response.json()['data']

            # update progress bar
            current = result['processed']
            pbar.update(current - previous)
            previous = current



    file_out = s.get(BASE_URL + result['filename'])

    with open(DOWNLOAD_FOLDER + "/" +  file_name,'wb') as f:
          f.write(file_out.content)


get_data(search_url = '/reports/graphs-data/recent-election-results',
        file_name= "temp.csv")

get_data(search_url = '/search/case',
        params={"f[0]":"case_type:R",
                "s[0]":"Open"},
        file_name= "new_open_data.csv")
