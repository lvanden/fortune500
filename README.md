# DMARC progress for Fortune 500 companies as of 12/29/2020.
Track the top 100 Fortune 500 companies' DMARC progress.
Scrape 2019 top 100 list.

### To build and run locally

Execute the following commands

```bash
python scrape.py
```
Return: .csv file to analyze

Thanks to Monash Data Fluency for the HTML Web Scrapper tutorial which made this pretty simple to scrape.

Analysis is done in R. Running analysis.r will generate a png file of the Top 100 companies' DMARC policy and aggregate reporting domain(s).
