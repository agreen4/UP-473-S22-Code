#remotes::install_github("walkerke/tidycensus")
library(tidycensus)
library(tidyverse)

# Andrew's API Key - Please get your own at the following URL if you plan to download more data! 
# Request a key here: https://api.census.gov/data/key_signup.html
# census_api_key("936c96236b979ae522c6cf67edb51923cd391fb3") 

# pums_vars_2019 <- pums_variables %>%
#   filter(year == 2019, survey == "acs1")
# 
# 
# pums_vars_2019 %>%
#   distinct(var_code, var_label, data_type, level) %>%
#   filter(level == "person") %>% View()

dataset <- get_pums(
  variables = c("PUMA", "ST", "SEX", "RAC1P", "TYPE", "BDSP","BLD", "RMSP", "GRNTP", "TEN", "VALP", "YBL", "HINCP", "SMOCP"),
  state = "all",
  survey = "acs1",
  year = 2019,
  recode = TRUE
)

# National Summary
pull1<-dataset %>%
  summarise(
    Population = sum(PWGTP),
    P_Female = sum(PWGTP[SEX_label == "Female"]/sum(PWGTP)),
    Bedrooms = weighted.mean(BDSP, WGTP),
    Type_House = sum(PWGTP[TYPE == 1]),
    Type_IGroup = sum(PWGTP[TYPE == 2]),
    Type_NIGroup = sum(PWGTP[TYPE == 3]),
    HU_SingleFamily = sum(PWGTP[BLD == "02"]),
    HU_2_4 = sum(PWGTP[BLD %in% c("04", "05")]),
    HU_5_9 = sum(PWGTP[BLD %in% c("06")]),
    HU_5_9 = sum(PWGTP[BLD %in% c("06")]),
    HU_10_19 = sum(PWGTP[BLD %in% c("07")]),
    HU_20_49 = sum(PWGTP[BLD %in% c("08")]),
    HU_50plus = sum(PWGTP[BLD %in% c("09")]),
    Rent = weighted.mean(GRNTP[GRNTP >= 0], WGTP),
    Tenure_Owned = sum(WGTP[TEN %in% c("1", "2")]),
    Tenure_Rented = sum(WGTP[TEN == "3"]),
    Unit_Built_Before1950 = sum(WGTP[YBL %in% c("01", "02")]),
    Unit_Built_1950_2000 = sum(WGTP[YBL %in% c("03", "04", "05", "06", "07")]),
    Unit_Built_After2000 = sum(WGTP[YBL %in% c("08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21")]),
  )

rent<-dataset %>% filter(TEN == 3) %>%  summarise(Rent = weighted.mean(GRNTP[GRNTP >=0], WGTP))
rent_state<-dataset %>% filter(TEN == 3) %>% group_by(State = ST_label) %>%   summarise(Rent = weighted.mean(GRNTP[GRNTP >=0], WGTP))

rent_race<-dataset %>% filter(TEN == 3) %>% group_by(Race = RAC1P_label) %>%   summarise(Rent = weighted.mean(GRNTP[GRNTP >=0], WGTP))

pull2<-pums %>% filter(SMOCP >= 1)%>%
  summarise(
    Owner_Costs = weighted.mean(SMOCP, WGTP),
  )

pull3<-pums %>% filter(VALP >= 0)%>%
  summarise(
    Property_Value_Mean = weighted.mean(VALP, WGTP),
  )

pull4<-pums %>% filter(HINCP >= 1)%>%
  summarise(
    Household_Income = weighted.mean(HINCP, WGTP)
  )

dataset_us<-bind_cols(pull1, pull2, pull3, pull4)
rm(pull1, pull2, pull3, pull4)

write_csv(dataset_us, "PUMS_us.csv")

# State Summary
pull1<-pums %>% group_by(State = ST_label) %>% 
  summarise(
  Population = sum(PWGTP),
  P_Female = sum(PWGTP[SEX_label == "Female"]/sum(PWGTP)),
  Bedrooms = weighted.mean(BDSP, WGTP),
  Type_House = sum(PWGTP[TYPE == 1]),
  Type_IGroup = sum(PWGTP[TYPE == 2]),
  Type_NIGroup = sum(PWGTP[TYPE == 3]),
  HU_SingleFamily = sum(PWGTP[BLD == "02"]),
  HU_2_4 = sum(PWGTP[BLD %in% c("04", "05")]),
  HU_5_9 = sum(PWGTP[BLD %in% c("06")]),
  HU_5_9 = sum(PWGTP[BLD %in% c("06")]),
  HU_10_19 = sum(PWGTP[BLD %in% c("07")]),
  HU_20_49 = sum(PWGTP[BLD %in% c("08")]),
  HU_50plus = sum(PWGTP[BLD %in% c("09")]),
  Rent = weighted.mean(GRNTP[GRNTP >= 0], WGTP),
  Tenure_Owned = sum(WGTP[TEN %in% c("1", "2")]),
  Tenure_Rented = sum(WGTP[TEN == "3"]),
  Unit_Built_Before1950 = sum(WGTP[YBL %in% c("01", "02")]),
  Unit_Built_1950_2000 = sum(WGTP[YBL %in% c("03", "04", "05", "06", "07")]),
  Unit_Built_After2000 = sum(WGTP[YBL %in% c("08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21")]),
)

pull2<-pums %>% filter(SMOCP >= 1)%>% group_by(State = ST_label) %>% 
  summarise(
    Owner_Costs = weighted.mean(SMOCP, WGTP),
  )

pull3<-pums %>% filter(VALP >= 0)%>% group_by(State = ST_label) %>% 
  summarise(
    Property_Value_Mean = weighted.mean(VALP, WGTP),
  )

pull4<-pums %>% filter(HINCP >= 1)%>% group_by(State = ST_label) %>% 
  summarise(
    Household_Income = weighted.mean(HINCP, WGTP)
  )

dataset_state<-left_join(pull1, pull2, by="State")
dataset_state<-left_join(dataset_state, pull3, by="State")
dataset_state<-left_join(dataset_state, pull4, by="State")

write_csv(dataset_state, "PUMS_state.csv")
rm(pull1, pull2, pull3, pull4)

#US Summary by Race
pull1<-pums %>% group_by(Race = RAC1P_label) %>% 
  summarise(
    Population = sum(PWGTP),
    P_Female = sum(PWGTP[SEX_label == "Female"]/sum(PWGTP)),
    Bedrooms = weighted.mean(BDSP, WGTP),
    Type_House = sum(PWGTP[TYPE == 1]),
    Type_IGroup = sum(PWGTP[TYPE == 2]),
    Type_NIGroup = sum(PWGTP[TYPE == 3]),
    HU_SingleFamily = sum(PWGTP[BLD == "02"]),
    HU_2_4 = sum(PWGTP[BLD %in% c("04", "05")]),
    HU_5_9 = sum(PWGTP[BLD %in% c("06")]),
    HU_5_9 = sum(PWGTP[BLD %in% c("06")]),
    HU_10_19 = sum(PWGTP[BLD %in% c("07")]),
    HU_20_49 = sum(PWGTP[BLD %in% c("08")]),
    HU_50plus = sum(PWGTP[BLD %in% c("09")]),
    Rent = weighted.mean(GRNTP[GRNTP >= 0], WGTP),
    Tenure_Owned = sum(WGTP[TEN %in% c("1", "2")]),
    Tenure_Rented = sum(WGTP[TEN == "3"]),
    Unit_Built_Before1950 = sum(WGTP[YBL %in% c("01", "02")]),
    Unit_Built_1950_2000 = sum(WGTP[YBL %in% c("03", "04", "05", "06", "07")]),
    Unit_Built_After2000 = sum(WGTP[YBL %in% c("08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21")]),
  )

pull2<-pums %>% filter(SMOCP >= 1)%>% group_by(Race = RAC1P_label) %>% 
  summarise(
    Owner_Costs = weighted.mean(SMOCP, WGTP),
  )

pull3<-pums %>% filter(VALP >= 0)%>% group_by(Race = RAC1P_label) %>% 
  summarise(
    Property_Value_Mean = weighted.mean(VALP, WGTP),
  )

pull4<-pums %>% filter(HINCP >= 1)%>% group_by(Race = RAC1P_label) %>% 
  summarise(
    Household_Income = weighted.mean(HINCP, WGTP)
  )

dataset_us_race<-left_join(pull1, pull2, by="Race")
dataset_us_race<-left_join(dataset_us_race, pull3, by="Race")
dataset_us_race<-left_join(dataset_us_race, pull4, by="Race")

write_csv(dataset_us_race, "PUMS_race_US.csv")
rm(pull1, pull2, pull3, pull4)

# Unit age by Race of Householder
dataset %>% group_by(State = ST_label,Race = RAC1P_label) %>% 
  summarise(
    Built_Before_1939 = sum(WGTP[YBL == "01"]),
    Built_1940_1949 = sum(WGTP[YBL == "02"]),
    Built_1950_1959 = sum(WGTP[YBL == "03"]),
    Built_1960_1969 = sum(WGTP[YBL == "04"]),
    Built_1970_1979 = sum(WGTP[YBL == "05"]),
    Built_1980_1989 = sum(WGTP[YBL == "06"]),
    Built_1990_1999 = sum(WGTP[YBL == "07"]),
    Built_2000_2009 = sum(WGTP[YBL %in% c("08", "09", "10", "11", "12", "13")]),
    Unit_Built_After2010 = sum(WGTP[YBL %in% c("14", "15", "16", "17", "18", "19", "20", "21", "22")])
  ) %>% write_csv(., "PUMS_UnitAgeRace_State.csv")
