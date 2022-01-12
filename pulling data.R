library(tidyverse)
library(tidycensus)
library(censusapi)
library(jsonlite)
library(foreach)

## Read in variable information from Census API for descriptions, codes, and labels -- only for 2019 assuming the most recent year has the largest number of variables associated for ACS
acsvars2019 <- fromJSON("https://api.census.gov/data/2019/acs/acs5/variables.json")
#Very useful to scroll through variables at the acs api via the website as well 

var_desc <- data.frame(stringsAsFactors = FALSE,matrix(ncol=3,nrow=1))
for (i in 4:length(acsvars2019$variables)){
  var_desc <- data.frame(rbind(var_desc,c(names(acsvars2019$variables)[i],acsvars2019$variables[[i]]$label,acsvars2019$variables[[i]]$concept)),stringsAsFactors = FALSE)
}

rm(acsvars2019)
names(var_desc) <- c("VariableCode","Label","Concept")

#uni is the pulled universe of variables for all surveys, instances of time, and vintages from the Census API
uni <- read.csv("universe.csv")
key <-  "YOUR KEY"

#key variables for round 1 census pull are 
  # ACS 5 yr
    #Age
    #race & hispanic
    #Educational attainment
    #Households (enumeration)
    #Housing Units (ownership)
    #commuter information
    #resident occupations (workforce)


acs5 <- uni[uni$name=="acs/acs5",]
acs5 <- left_join(acs5,var_desc,by=c("vars"="VariableCode"))

##because we use the 2019 variables, the resulting acs5 dataset has 246170 NA in the "label" category. Assuming these are for variables that we will not use right away, I'm comfortable not addressing this issue, but it should be on the radar to fix later

var_freq <- acs5 %>% group_by(vars) %>% summarize(length(vars))

## function to retrieve acs years data
yeargeog <- function(year,state,vars){
  data.frame(year = year, 
             getCensus(name="acs/acs5",
                       vintage=year,
                       vars = vars,
                       key=key,
                       region = "tract:*",
                       regionin = paste0("state:",unique(fips_codes$state_code)[state])
             )
  )
  
}

##Age
#the Age variables all sit in the family of B01001
age_vars <- grep("B01001_",acs5$vars)
acs5_age_vars <- acs5[age_vars,10]
acs5_age_vars <- as.character(acs5_age_vars)
age_var_names <- unique(acs5_age_vars)

acs5_age <- data.frame()

acs5_age <- foreach(i = rep(2009:2019,51), j = rep(1:51,10),.combine = 'rbind') %do% {
  yeargeog(year=i,state=j,vars=age_var_names)
}

fips_to_acs <- function(data){
  t <- left_join(data,fips_codes,by=c("state"="state_code","county"="county_code"))
  t
}

acs5_age <- fips_to_acs(acs5_age)

#03-09 (under 21) Male
#27-33 (under 21) Female
#20-25 (over 65) Male
#44 - 49 (over 65) Female

age_small <- acs5_age %>% 
  mutate(over65m=B01001_020E+B01001_021E+B01001_022E+B01001_023E+B01001_024E+B01001_025E,
                     over65f=B01001_044E+B01001_045E+B01001_046E+B01001_047E+B01001_048E+B01001_049E,
                     under21m=B01001_003E+B01001_004E+B01001_005E+B01001_006E+B01001_007E+B01001_008E+B01001_009E,
                     under21f=B01001_027E+B01001_028E+B01001_029E+B01001_030E+B01001_031E+B01001_032E+B01001_033E) %>%
  select("year","state_name","county.y","tract","B01001_001E","B01001_002E","B01001_026E",over65m,over65f,under21m,under21f)
names(age_small) <- c("year","state","county","tract","total population","male population","female population","over 65 male population", "over 65 female population","under 21 male population", "under 21 female population")

## Race
race_vars <- c("B01001A_001E","B01001B_001E","B01001C_001E","B01001D_001E","B01001E_001E","B01001F_001E","B01001G_001E","B01001H_001E","B01001I_001E")
acs5_race <-foreach(i = rep(2009:2019,51), j = rep(1:51,10),.combine = 'rbind') %do% {
  yeargeog(year=i,state=j,vars=race_vars)
}

acs5_race <- fips_to_acs(acs5_race)
acs5_race <- acs5_race %>% select (year,state_name,county.y,tract,"B01001A_001E","B01001B_001E","B01001C_001E","B01001D_001E","B01001E_001E","B01001F_001E","B01001G_001E","B01001H_001E","B01001I_001E" )

names(acs5_race) <- c("year","state","county", "tract",
                      "white alone",
                      "black alone",
                      "american indian or alaska native alone",
                      "asian alone",
                      "native hawaiian or pacific islander alone",
                      "some other race alone",
                      "two or more races",
                      "white alone, not hispanic or latino",
                      "hispanic or latino, any race category"
                      )

#can cross check with B02001_001:10E
#B02008:14 introduces combinations (expansion of two or more category)



##Educational attainment

# for educational attainment I initially tried to do a more reproducible approach in using text to pull variable, I found this not very useful but it could be expanded on
ed_vars <- var_desc[grep("EDUCATION",var_desc$X3),]

#Educational variable names differ in years 2009-2011 and are thus excluded, can be included later
ed_vars2016 <- c(paste0("B15003_00",2:9,"E"),paste0("B15003_0",10:25,"E"))
ed_vars2009 <- "B15001_001E"

acs5_education <- foreach(i = rep(2012:2019,51), j = rep(1:51,8),.combine = 'rbind')%do%{
  yeargeog(year=i,state=j,vars=ed_vars2016)
}

acs5_education <- fips_to_acs(acs5_race)


ed_small <- acs5_education %>% 
  mutate(noschooling=B15003_002E,
         primaryschoolnodiploma = B15003_003E+ B15003_004E+B15003_005E+B15003_006E+B15003_007E+B15003_008E+B15003_009E+B15003_010E+B15003_011E+B15003_012E+B15003_013E+B15003_014E
         + B15003_015E+B15003_016E,
         highschooldiploma= B15003_017E,
         somecollegenodegree= B15003_019E+B15003_020E
         ) %>%
  select("year","state_name","county.y","tract",noschooling,primaryschoolnodiploma,highschooldiploma,somecollegenodegree,B15003_021E,B15003_022E,B15003_023E,B15003_024E,B15003_025E)
names(ed_small) <- c("year","state","county","tract","noschooling","primaryschoolnodiploma","highschooldiploma","somecollegenodegree","associatesdegree","bachelorsdegree","masters degree","professionaldegree","doctoratedegree")


##Households (emmuneration)
## total households, family households, married with children, married without children, single parents, other, non-family households, living alone
#B09019_002E -  total (household type) 
#B09019_002E - In households


##Housing units (ownership)
## total housing units, owner occupied, renter occupied, vacant for seasonal or recreational use, 1-unit (attached or detached), 2-9 units, 10-19 units, 20 or more units, built prior to 1940


##Commuter information/Migration
## workers 16 and over, car,truc, or van --drives alone, car truck or van -- carpools, public transporation (including taxi), walk, other means, works from home

##Resident occupations
## employed population 16 and over, sectors of occupation (statsamerica shows top 6 emplyment sectors )





