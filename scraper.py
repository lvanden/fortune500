import requests
import pandas as pd
from bs4 import BeautifulSoup

url = 'https://www.zyxware.com/articles/4344/list-of-fortune-500-companies-and-their-websites'
page = requests.get(url)

soup_object = BeautifulSoup(page.content, features="html.parser")

# Get data in within the <table> HTML tag with the class name 'data-table'
data_table = soup_object.find_all('table', 'data-table')[0]

# get all row elements
all_values = data_table.find_all('tr')

fortune_500_df = pd.DataFrame(columns = ['rank', 'company', 'website'])
index = 0

for row in all_values[1:]:
    # Extract all elements with tag <td>
    values = row.find_all('td')
    # Extract the text part from the <td> tag
    rank = values[0].text
    company = values[1].text
    website = values[2].text

    fortune_500_df.loc[index] = [rank, company, website]
    index += 1

# get DMARC records
fortune_500_df.to_csv('fortune_500_companies.csv', index=False)