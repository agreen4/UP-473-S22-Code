library(tidyverse)
library(tidycensus)

geo<- "place"
acs_year <- 2019

# Andrew's API Key - Please get your own at the following URL if you plan to download more data! 
# Request a key here: https://api.census.gov/data/key_signup.html
# census_api_key("936c96236b979ae522c6cf67edb51923cd391fb3") 

# Population
pop19<-get_acs(geography=geo, variables = c("B01003_001E"), year=2019, survey = "acs5", output="wide") %>% 
  rename("pop19" = "B01003_001E") %>% 
  select(GEOID, NAME, pop19)

pop10<-get_acs(geography=geo, variables = c("B01003_001E"), year=2010, survey = "acs5", output="wide") %>% 
  rename("pop10" = "B01003_001E") %>% 
  select(GEOID, pop10)

# Race
race19<-get_acs(geography=geo, table = "B02001", year=2019, survey = "acs5", output="wide") %>% 
  rename("race19_total" = "B02001_001E",
         "race19_white" = "B02001_002E",
         "race19_black" = "B02001_003E",
         "race19_aian" = "B02001_004E",
         "race19_asian" = "B02001_005E",
         "race19_nhpi" = "B02001_006E",
         "race19_other" = "B02001_007E",
         "race19_2more" = "B02001_008E")   %>% 
  select(GEOID, race19_total, race19_white, race19_black, race19_aian, race19_asian, race19_nhpi, race19_other, race19_2more)

race10<-get_acs(geography=geo, table = "B02001", year=2010, survey = "acs5", output="wide") %>% 
  rename("race10_total" = "B02001_001E",
         "race10_white" = "B02001_002E",
         "race10_black" = "B02001_003E",
         "race10_aian" = "B02001_004E",
         "race10_asian" = "B02001_005E",
         "race10_nhpi" = "B02001_006E",
         "race10_other" = "B02001_007E",
         "race10_2more" = "B02001_008E")   %>% 
  select(GEOID, race10_total, race10_white, race10_black, race10_aian, race10_asian, race10_nhpi, race10_other, race10_2more)

# Latino Ethnicity
ethnicity19<-get_acs(geography=geo, table = "B03002", year=2019, survey = "acs5", output="wide") %>% 
  rename("ethnicity19_total" = "B03002_001E",
         "ethnicity19_nhw" = "B03002_003E",
         "ethnicity19_latino" = "B03002_012E")   %>% 
  select(GEOID, ethnicity19_total, ethnicity19_nhw, ethnicity19_latino)

ethnicity10<-get_acs(geography=geo, table = "B03002", year=2010, survey = "acs5", output="wide") %>% 
  rename("ethnicity10_total" = "B03002_001E",
         "ethnicity10_nhw" = "B03002_003E",
         "ethnicity10_latino" = "B03002_012E")   %>% 
  select(GEOID, ethnicity10_total, ethnicity10_nhw, ethnicity10_latino)

# Age
age19<-get_acs(geography=geo, table = "B01001", year=2019, survey = "acs5", output="wide") %>% 
  mutate(
    age19_under18 =
    (B01001_003E
    +B01001_004E
    +B01001_005E
    +B01001_006E
    +B01001_027E
    +B01001_028E
    +B01001_029E
    +B01001_030E),
age19_over65 = 
  (B01001_020E
   +B01001_021E
   +B01001_022E
   +B01001_023E
   +B01001_024E
   +B01001_025E
   +B01001_044E
   +B01001_045E
   +B01001_046E
   +B01001_047E
   +B01001_048E
   +B01001_049E)) %>% 
  rename("age19_total" = "B01001_001E")  %>% 
  select(GEOID, age19_total, age19_under18, age19_over65)

age10<-get_acs(geography=geo, table = "B01001", year=2010, survey = "acs5", output="wide") %>% 
  mutate(
    age10_under18 =
      (B01001_003E
       +B01001_004E
       +B01001_005E
       +B01001_006E
       +B01001_027E
       +B01001_028E
       +B01001_029E
       +B01001_030E),
    age10_over65 = 
      (B01001_020E
       +B01001_021E
       +B01001_022E
       +B01001_023E
       +B01001_024E
       +B01001_025E
       +B01001_044E
       +B01001_045E
       +B01001_046E
       +B01001_047E
       +B01001_048E
       +B01001_049E)) %>% 
  rename("age10_total" = "B01001_001E")  %>% 
  select(GEOID, age10_total, age10_under18, age10_over65)

# Average Household Size
size19<-get_acs(geography=geo, variables = c("B25010_001E"), year=2019, survey = "acs5", output="wide") %>% rename("size19" = "B25010_001E") %>% select(GEOID, size19)
size10<-get_acs(geography=geo, variables = c("B25010_001E"), year=2010, survey = "acs5", output="wide") %>% rename("size10" = "B25010_001E") %>% select(GEOID, size10)

# Foreign Born Population

foreign19<-get_acs(geography=geo, variables = c("B05002_013E"), year=2019, survey = "acs5", output="wide") %>% rename("foreignborn19" = "B05002_013E") %>% select(GEOID, foreignborn19)
foreign10<-get_acs(geography=geo, variables = c("B05002_013E"), year=2010, survey = "acs5", output="wide") %>% rename("foreignborn10" = "B05002_013E") %>% select(GEOID, foreignborn10)

# Median Household Income
mhhi19<-get_acs(geography=geo, variables = c("B19013_001E"), year=2019, survey = "acs5", output="wide") %>% rename("mhi19" = "B19013_001E")   %>% select(GEOID, mhi19)
mhhi10<-get_acs(geography=geo, variables = c("B19013_001E"), year=2010, survey = "acs5", output="wide") %>% rename("mhi10" = "B19013_001E")   %>% select(GEOID, mhi10)

# Vacancy Rate
vac19<-get_acs(geography=geo, variables = c("B25002_003E", "B25002_001E"), year=2018, survey = "acs5", output="wide") %>% rename("hu18" = "B25002_001E", "vac18" = "B25002_003E") %>% select(GEOID, hu18, vac18)
vac10<-get_acs(geography=geo, variables = c("B25002_003E", "B25002_001E"), year=2010, survey = "acs5", output="wide") %>% rename("hu10" = "B25002_001E", "vac10" = "B25002_003E") %>% select(GEOID, hu10, vac10)

# Tenure
tenure19<-get_acs(geography=geo, variables = c("B25003_001E", "B25003_002E"), year=2019, survey = "acs5", output="wide") %>% rename("pown19" = "B25003_002E", "pownbase19" = "B25003_001E") %>% select(GEOID, pown19, pownbase19)
tenure10<-get_acs(geography=geo, variables = c("B25003_001E", "B25003_002E"), year=2010, survey = "acs5", output="wide") %>% rename("pown10" = "B25003_002E", "pownbase10" = "B25003_001E") %>% select(GEOID, pown10, pownbase10)

# Median Rent and Owner Costs
mgr19<-get_acs(geography=geo, variables = c("B25064_001E"), year=2019, survey = "acs5", output="wide") %>% rename("mgr19" = "B25064_001E")  %>% select(GEOID, mgr19)
mgr10<-get_acs(geography=geo, variables = c("B25064_001E"), year=2010, survey = "acs5", output="wide") %>% rename("mgr10" = "B25064_001E")  %>% select(GEOID, mgr10)

moc19<-get_acs(geography=geo, variables = c("B25088_001E"), year=2019, survey = "acs5", output="wide") %>% rename("moc19" = "B25088_001E")  %>% select(GEOID, moc19)
moc10<-get_acs(geography=geo, variables = c("B25088_001E"), year=2010, survey = "acs5", output="wide") %>% rename("moc10" = "B25088_001E")  %>% select(GEOID, moc10)


# Join
dataset<-list(pop19, pop10, age19, age10, race19, race10, ethnicity19, ethnicity10, foreign19, foreign10, mhhi19, mhhi10, moc19, moc10, mgr19, mgr10, tenure19, tenure10,vac19, vac10, size19, size10) %>% 
  reduce(left_join, by="GEOID")

rm(pop19, pop10, age19, age10, race19, race10, ethnicity19, ethnicity10, foreign19, foreign10, mhhi19, mhhi10, moc19, moc10, mgr19, mgr10, tenure19, tenure10,vac19, vac10, size19, size10)

# Write a file
dataset<-dataset %>% arrange(GEOID)
write_csv(dataset, "housing_data_10_19_Place.csv")
