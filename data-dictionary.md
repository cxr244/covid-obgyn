All data was downloaded from source URLs on 21 December 2020.

## aamc-state-data.csv

Contains data obtained from AAMC Physician Workforce Profile.
* URL: https://www.aamc.org/data-reports/workforce/data/2019-state-profiles
* format: comma separated values
* column 'region': full name of U.S. state
* column 'state': abbreviation of U.S. state
* column 'num_ob': total number of OBGYN physicians in state
* column 'pts_per_ob': # of people per Ob/Gyn physician (2018)
* column 'Over60': proportion of practicing Ob/Gyn physicians over the age of 60
	
## covid-confirmed.csv

Contains COVID-19 data obtained from John Hopkins University
* URL: https://github.com/CSSEGISandData/COVID-19/
* format: comma separated values
* column 'Province_State': full name of U.S. state
* column `12/21/2020`: cumulative cases of SARS-CoV-2 infection

## usa_lat_long.csv

Latitude and longitude coordinates for US continental states 
* URL: https://www.latlong.net/category/states-236-14.html
* format: comma separated values
* column 'region': full name of U.S. state
* column 'Latitude': geographical longitude
* column 'Longitude': geographical latitude

## state_births.csv

2018 total births per state reported by US Census Bureau
* URL: https://www.cdc.gov/nchs/fastats/state-and-territorial-data.htm
* format: comma separated values
* column 'region': full name of U.S. state
* column 'total_births': total births per state in 2018

## state_sex.csv

2018 ratio of males to females per state reported by US Census Bureau
* URL: https://data.census.gov/cedsci/table?q=S0101&g=0100000US.04000.001,&hidePreview=true&table=S0101&tid=ACSST1Y2018.S0101&lastDisplayedRow=30&vintage=2018&moe=false
* format: comma separated values
* column 'region': full name of U.S. state
* column 'mf_ratio': ratio of males to females in state

## state_population.csv

2018 state populations reported by US Census Bureau
* URL: https://data.census.gov/cedsci/table?q=S0101&g=0100000US.04000.001,&hidePreview=true&table=S0101&tid=ACSST1Y2018.S0101&lastDisplayedRow=30&vintage=2018&moe=false
* format: comma separated values
* column 'region': full name of U.S. state
* column 'mf_ratio': ratio of males to females in state
