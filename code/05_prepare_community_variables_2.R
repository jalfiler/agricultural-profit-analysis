# Objective: Determine local community factors by household.


library(tidyverse)
library(haven)



# --------- Community Surveys (only rural) ------------------------------------------------


community_eco_infra <- read_dta("raw_data/cs2.dta") # Economy and Infrastructure
community_edu <- read_dta("raw_data/cs3.dta") # Education
community_ag <- read_dta("raw_data/cs5b.dta") # Agriculture


# ----------- Tidy Community Health Data ----------------------------------------------------

# see G4QComm for enums (page 18)

# renames community agriculture
community_ag <- community_ag %>% rename(
  Use_Chem_Fert = "s5bq15",
  Use_Insecticides = "s5bq16",
  Irrigated_Fields = "s5bq17",
  Ag_extension = "s5bq5",
  Kilometers = "s5bq6",
  Agent_visit = "s5bq7",
  Agencies = "s5bq8",
  Service1 = "s5bq9a",
  Service2 = "s5bq9b",
  Service3 = "s5bq9c",
  Service4 = "s5bq9d",
  Cooperative = "s5bq10",
  Coop_service1 = "s5bq11a",
  Coop_service2 = "s5bq11b",
  Coop_service3 = "s5bq11c",
  Coop_participation = "s5bq12"
)


# renames for community economic and infrastructure
community_eco_infra <- community_eco_infra %>% rename(
  Comm_Activities1 = "s2q1a",
  Comm_Activities2 = "s2q1b",
  Comm_Activities3 = "s2q1c",
  Comm_Activities4 = "s2q1d",
  Motorable_Road = "s2q4",
  Water_source = "s2q12",
  Dist_Motorable = "s2q5",
  Impassable_Road = "s2q6",
  Dist_Impassable = "s2q7",
  Perm_Market_in_Comm = "s2q19",
  Dist_to_Market = "s2q22",
  Bank_in_Comm = "s2q17",
  Dist_to_Bank = "s2q18"
)

# renames for community education
community_edu <- community_edu %>% rename(
  Primary_School = "s3q1",
  PS_Distance = "s3q2",
  School1 = "s3q4a",
  School2 = "s3q4b",
  School3 = "s3q4c",
  Boys_Enrolled = "s3q8",
  Girls_Enrolled = "s3q9"
)



# Drop region, district
community_ag <- community_ag %>%
  select(-c(region, district))

community_edu <- community_edu %>%
  select(-c(region, district))

community_eco_infra <- community_eco_infra %>%
  select(-c(region, district))


# select necessary variables in df
keeps <- c(
  "eanum",
  "Use_Chem_Fert",
  "Use_Insecticides",
  "Irrigated_Fields"
)
community_ag <- community_ag[keeps]


keeps <- c(
  "eanum",
  "Primary_School",
  "PS_Distance",
  "School1",
  "School2",
  "School3",
  "Boys_Enrolled",
  "Girls_Enrolled"
)
community_edu <- community_edu[keeps]


keeps <- c(
  "eanum",
  "Motorable_Road",
  "Dist_Motorable",
  "Impassable_Road",
  "Dist_Impassable"
)
community_eco_infra <- community_eco_infra[keeps]

# If primary school in the community, then update distance to 0.
c_edu <- community_edu %>%
  mutate(Dis_School = case_when(
    Primary_School == 1 ~ 0,
    Primary_School == 2 ~ PS_Distance,
    TRUE ~ NA_real_
  )) %>%
  select(eanum, Dis_School)

# check for null values
c_edu %>%
  filter(is.na(Dis_School) == TRUE) %>%
  summarise()

# check for duplicates rows
edu_dupes <- c_edu %>%
  group_by(eanum) %>%
  filter(n() > 1)

c_edu <- c_edu %>%
  distinct()

# check for skew
summary(c_edu)

c_edu <- c_edu %>%
  group_by(eanum) %>%
  summarise(Dis_School = median(Dis_School))

# The presence of a motorable road near the household influences
# agricultural profit per acre.
c_road <- community_eco_infra %>%
  select(eanum, Motorable_Road, Dist_Motorable) %>%
  mutate(Dis_Road = case_when(
    Motorable_Road == 1 ~ 0,
    Motorable_Road == 2 ~ Dist_Motorable,
    TRUE ~ NA_real_
  )) %>%
  select(eanum, Dis_Road)

# check for null values
c_road %>%
  filter(is.na(Dis_Road) == TRUE) %>%
  summarise()

# check for duplicates rows
road_dupes <- c_road %>%
  group_by(eanum) %>%
  filter(n() > 1)

c_road <- c_road %>%
  distinct()

# check for skew
summary(c_road)

c_road <- c_road %>%
  group_by(eanum) %>%
  summarise(Dis_Road = median(Dis_Road))

# The presence of fertilizer/pesticides influences agricultural profit per acre.

# check for null values
community_ag %>%
  filter(if_any(everything(), is.na))

# check for duplicates rows
c_ag_dupes <- community_ag %>%
  group_by(eanum) %>%
  filter(n() > 1)

community_ag <- community_ag %>%
  distinct()

community_ag <- community_ag %>%
  distinct(eanum, .keep_all = TRUE)

# Hypothesis --------------------------------------------------------------------
# The presence of primary school near the household influences agricultural profit per acre.
# The presence of a motor able road near the household influences agricultural profit per acre.
# The presence of fertilizer/pesticides influences agricultural profit per acre.

# Clean Up Environment ----------------------------------------------------

# Remove environment variables no longer needed.

rm(list = c(
  "c_ag_dupes",
  "community_ag",
  "community_eco_infra",
  "community_edu",
  "edu_dupes",
  "road_dupes",
  "keeps"
))
