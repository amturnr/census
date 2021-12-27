library(tidyverse)
library(tidycensus)
library(censusapi)
library(jsonlite)
varstuff <- fromJSON("https://api.census.gov/data/2019/acs/acs5/variables.json")

var_desc <- data.frame(stringsAsFactors = FALSE,matrix(ncol=3,nrow=1))
for (i in 4:length(varstuff$variables)){
  var_desc <- data.frame(rbind(var_desc,c(names(varstuff$variables)[i],varstuff$variables[[i]]$label,varstuff$variables[[i]]$concept)),stringsAsFactors = FALSE)
}

rm(varstuff)


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
acs5 <- left_join(acs5,var_desc,by=c("vars"="X1"))


var_freq <- acs5 %>% group_by(vars) %>% summarize(length(vars))


##Age
age_vars <- grep("B01001_",acs5$vars)
acs5_age_vars <- acs5[age_vars,10]
acs5_age_vars <- as.character(acs5_age_vars)
age_var_names <- unique(acs5_age_vars)
acs5_age <- data.frame(year= 2009, getCensus(name="acs/acs5",
                                 vintage=2009,
                                 vars = age_var_names,
                                 key=key,
                                 region = "county:*"))
for (i in 2009:2019){
  for (j in 1:51){
    acs5_age <- rbind(acs5_age, data.frame(year = i, 
                                             getCensus(name="acs/acs5",
                                                       vintage=i,
                                                       vars = age_var_names,
                                                       key=key,
                                                       region = "tract:*",
                                                       regionin = paste0("state:",unique(fips_codes$state_code)[j])
                                             )
    )
    )
  }
}

#unique(var_desc[var_desc$X1%in%age_var_names,])

#03-09 (under 21) Male
#27-33 (under 21) Female
#20-25 (over 65) Male
#44 - 49 (over 65) Female

age_small <- acs5_age %>% 
  mutate(over65m=B01001_020E+B01001_021E+B01001_022E+B01001_023E+B01001_024E+B01001_025E,
                     over65f=B01001_044E+B01001_045E+B01001_046E+B01001_047E+B01001_048E+B01001_049E,
                     under21m=B01001_003E+B01001_004E+B01001_005E+B01001_006E+B01001_007E+B01001_008E+B01001_009E,
                     under21f=B01001_027E+B01001_028E+B01001_029E+B01001_030E+B01001_031E+B01001_032E+B01001_033E) %>%
  select("year","state","county","B01001_001E","B01001_002E","B01001_026E",over65m,over65f,under21m,under21f)
names(age_small) <- c("year","state","county","total","male","female","over 65 male", "over 65 female","under 21 male", "under 21 female")

## Race
race_vars <- c("B01001A_001E","B01001B_001E","B01001C_001E","B01001D_001E","B01001E_001E","B01001F_001E","B01001G_001E","B01001H_001E","B01001I_001E")
acs5_race <- data.frame()
for (i in 1:10){
acs5_race <- rbind(acs5_race, data.frame(year = 2009, getCensus(name="acs/acs5",
                      vintage=2009,
                      vars = race_vars,
                      key=key,
                      region = "tract:*",
                      regionin = paste0("state:",unique(fips_codes$state_code)[i]))))
}

#loop doesnt work (years)
for (i in 2009:2019){
  for (j in 1:51){
  acs5_race <- rbind(acs5_race, data.frame(year = i, 
                                           getCensus(name="acs/acs5",
                                                             vintage=i,
                                                             vars = race_vars,
                                                             key=key,
                                                             region = "tract:*",
                                                             regionin = paste0("state:",unique(fips_codes$state_code)[j])
                                                     )
                                           )
                     )
  }
  }

names(acs5_race) <- c("year","state","county",
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

#cross check with B02001_001:10E
#B02008:14 introduces combinations (expansion of two or more category)

##Educational attainment
ed_vars <- var_desc[grep("EDUCATION",var_desc$X3),]
ed_vars <- c(paste0("B15003_00",2:9,"E"),paste0("B15003_0",10:25,"E"))

acs5_education <- data.frame()
for (i in 2009:2019){
  for (j in 1:51){
    acs5_education <- rbind(acs5_education, data.frame(year = i, 
                                             getCensus(name="acs/acs5",
                                                       vintage=i,
                                                       vars = ed_vars,
                                                       key=key,
                                                       region = "tract:*",
                                                       regionin = paste0("state:",unique(fips_codes$state_code)[j])
                                             )
    )
    )
  }
}

##Households (emmuneration)


##Housing units (ownership)


##Commuter information/Migration


##Resident occupations\






