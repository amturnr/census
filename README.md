# census
This is an expanding resource to pull census data from their api and store as csv for further processing. 

Explination of files:
1. fips.csv - list of fips codes provided by censusapi R package, saved as csv for quick reference
2. pulling data.R - R file to pull data from the US Census API, designated important "pillar" columns from the survey
3. Variable Pull (full census API).R - questionably useful, but pulls and compiles the universe of possible variables available from the US Census

Explination of folders:
1. Census Questionaires - a holding place for pdf versions of census and ACS questionaires
2. CSV files - a storage facility for relevant census csv files 