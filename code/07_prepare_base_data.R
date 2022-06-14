# Objective: Merge response and explanatory variables into single dataframe to
# use as base data for developing models. 


# Load Libraries ----------------------------------------------------------

library(tidyverse)


# Load and Clean Variables ------------------------------------------------

# Loading and cleaning all variables allows for running only a single script
# to prepare base model data. 

# Agricultural Profit

ag_profit <- read_dta("raw_data/agg2.dta") %>%
  select(clust,
    hh = nh,
    profit = agri1c
  )

profit <- read_dta("raw_data/sec8b.dta") %>%
  select(
    clust = clust,
    hh = nh,
    farm_id = farmcd,
    size = s8bq4a,
    unit = s8bq4b
  ) %>%
  drop_na() %>%
  filter(unit %in% c(1, 2, 3)) %>%
  mutate(acres = case_when(
    unit %in% c(1, 2) ~ size,
    unit == 3 ~ size / 9,
    TRUE ~ NA_real_
  )) %>%
  group_by(clust, hh) %>%
  summarize(acres = sum(acres)) %>%
  left_join(ag_profit, by = c("clust", "hh")) %>%
  transmute(clust,
    hh,
    profit = profit / acres
  )

# Regional Variables

region <- read_dta("raw_data/sec0a.dta") %>%
  transmute(
    clust,
    eanum,
    hh = nh,
    ez = case_when(
      ez == 1 ~ "Coastal",
      ez == 2 ~ "Forest",
      ez == 3 ~ "Savannah",
      TRUE ~ NA_character_
    ),
    u_r = case_when(
      loc2 == 1 ~ "Urban",
      loc2 == 2 ~ "Rural",
      TRUE ~ NA_character_
    ),
    ez_u_r = case_when(
      loc5 == 1 ~ "Accra",
      loc5 == 2 ~ "Other Urban",
      loc5 == 3 ~ "Rural Coastal",
      loc5 == 4 ~ "Rural Forest",
      loc5 == 5 ~ "Rural Savannah",
      TRUE ~ NA_character_
    ),
    u_r_c = case_when(
      loc3 == 1 ~ "Accra",
      loc3 == 2 ~ "Other Urban",
      loc3 == 3 ~ "Rural",
      TRUE ~ NA_character_
    )
  )

# Educational Attainment

edu <- read_dta("raw_data/sec2a.dta") %>%
  select(clust,
         hh = nh,
         pid,
         edu_attend = s2aq1,
         edu_qual = s2aq3,
         edu_lvl = s2aq2)
         
edu <- read_dta("raw_data/sec1.dta") %>%
  select(clust,
         hh = nh,
         pid,
         agey) %>%
  filter(agey >= 15) %>%
  left_join(edu, by = c(
    "clust",
    "hh",
    "pid"
  )) %>%
  select(
    clust,
    hh,
    pid,
    agey,
    edu_attend,
    edu_qual,
    edu_lvl
  ) %>%
  transmute(clust,
            hh,
            pid,
            agey,
            edu_attend,
            edu_qual = case_when(
              edu_attend == 1 ~ edu_qual,
              edu_attend == 2 ~ 1,
              TRUE ~ edu_qual
            ),
            edu_lvl = case_when(
              edu_attend == 1 ~ edu_lvl,
              edu_attend == 2 ~ 1,
              TRUE ~ edu_lvl
            )
  ) %>%
  mutate(
    edu_qual = case_when(
      edu_qual == 1 ~ "none",
      edu_qual == 2 ~ "basic",
      edu_qual %in% 3:14 ~ "more_basic",
      edu_qual == 96 ~ "unknown",
      TRUE ~ NA_character_
    ),
    edu_lvl = case_when(
      edu_lvl == 1 ~ "none",
      edu_lvl %in% 2:5 ~ "less_basic",
      edu_lvl %in% 6:16 ~ "more_basic",
      edu_lvl == 17 ~ "less_basic",
      edu_lvl == 96 ~ "unknown",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(edu = case_when(
    edu_qual == "none" & edu_lvl == "none" ~ "none",
    edu_qual == "none" & edu_lvl != "none" ~ "less_basic",
    edu_qual == "unknown" & edu_lvl %in% c("none", "unknown") ~ "none",
    edu_qual == "unknown" & edu_lvl %in% c("less_basic", "more_basic") ~ "less_basic",
    edu_qual == "basic" ~ "basic",
    edu_qual == "more_basic" ~ "more_basic",
    TRUE ~ NA_character_
  ), 
  rank = case_when(
    is.na(edu) == TRUE ~ 0,
    edu == "none" ~ 1,
    edu == "less_basic" ~ 2,
    edu == "basic" ~ 3,
    edu == "more_basic" ~ 4,
    TRUE ~ NA_real_)) %>%
  group_by(clust, hh) %>%
  slice(which.max(rank)) %>%
  select(
    clust,
    hh,
    edu
  ) %>%
  mutate(edu = replace(edu, is.na(edu) == TRUE, "none"))

# Community Size

c_size <- read_dta("raw_data/cs1.dta") %>%
  select(eanum,
         size = s1q1
  ) %>%
  distinct() %>%
  filter(!(eanum %in% c(419, 485) & is.na(size) == TRUE)) %>%
  group_by(eanum) %>%
  summarize(size = sum(size)) %>%
  mutate(size = replace_na(size, median(size, na.rm = TRUE)))

# Community Distance to Nearest Primary School

c_edu <- read_dta("raw_data/cs3.dta") %>%
  select(eanum,
         school = s3q1,
         school_dis = s3q2
         ) %>%
  mutate(school_dis = case_when(
    school == 1 ~ 0,
    school == 2 ~ school_dis,
    TRUE ~ NA_real_)) %>%
  select(eanum,
         school_dis) %>%
  distinct() %>%
  group_by(eanum) %>%
  summarise(school_dis = median(school_dis))

# Community Distance to Nearest Motorable Road

c_road <- read_dta("raw_data/cs2.dta") %>%
  select(eanum,
         road = s2q4, 
         road_dis = s2q5) %>%
  mutate(road_dis = case_when(
    road == 1 ~ 0,
    road == 2 ~ road_dis,
    TRUE ~ NA_real_
  )) %>%
  select(eanum, road_dis) %>%
  distinct() %>%
  group_by(eanum) %>%
  summarise(road_dis = median(road_dis))

# Community Agriculture: chemical fertilizer, insecticide, irrigation

c_ag <- read_dta("raw_data/cs5b.dta") %>%
  select(eanum,
         chem_fert = s5bq15,
         insecticide = s5bq16,
         irrigation = s5bq17) %>%
  distinct() %>%
  distinct(eanum, .keep_all = TRUE)

# Average Household Age

age <- read_dta("raw_data/sec1.dta") %>%
  select(clust,
         hh = nh,
         pid,
         age = agey) %>%
  group_by(clust, hh) %>%
  summarize(age = median(age))
  

# Merge Dataframes -----------------------------------------------------

# Merge all dataframes together using left join on profit. 

# Merge 1: Left join with agricultural profit and region.

base <- profit %>%
  left_join(region, by = c("clust", "hh")) %>%
  select(clust,
         eanum,
         hh,
         profit,
         ez,
         ez_u_r,
         u_r,
         u_r_c)

# Check for missing values.

base %>%
  filter(if_any(everything(), is.na))
  
# Merge 2: Left join with base and educational attainment.

base <- base %>%
  left_join(edu, by = c("clust", "hh")) %>%
  select(clust,
         eanum,
         hh,
         profit,
         ez,
         ez_u_r,
         u_r,
         u_r_c,
         edu)

# Check for missing values.

base %>%
  filter(if_any(everything(), is.na))

# Merge 3: Left join with base and community characteristics. 

# Left join with community size

base <- base %>%
  left_join(c_size, by = c("eanum")) %>%
  select(clust,
         eanum,
         hh,
         profit,
         ez,
         ez_u_r,
         u_r,
         u_r_c,
         edu,
         size)

# Check for missing values.

base %>%
  filter(if_any(everything(), is.na)) %>%
  count()

# There are 72 rows without a community size. This makes sense as the community
# survey was only conducted in rural areas so not all of the eanum areas will
# have information. It makes the most sense to remove these rows from the data.

base <- base %>%
  drop_na()

base  %>%
  filter(if_any(everything(), is.na)) %>%
  count()

# Left join with community primary school distance

base <- base %>%
  left_join(c_edu, by = c("eanum")) %>%
  select(clust,
         eanum,
         hh,
         profit,
         ez,
         ez_u_r,
         u_r,
         u_r_c,
         edu,
         size,
         school_dis)

# Left join with community nearest motorable road distance.

base <- base %>%
  left_join(c_road, by = c("eanum")) %>%
  select(clust,
         eanum,
         hh,
         profit,
         ez,
         ez_u_r,
         u_r,
         u_r_c,
         edu,
         size,
         school_dis,
         road_dis)

# Left join with community variables on use of chemical fertilizers, use of
# insecticides, and presence of irrigation.

base <- base %>%
  left_join(c_ag, by = c("eanum")) %>%
  select(clust,
         eanum,
         hh,
         profit,
         ez,
         ez_u_r,
         u_r,
         u_r_c,
         edu,
         size,
         school_dis,
         road_dis,
         chem_fert,
         insecticide,
         irrigation)

# Add average household age.

base <- base %>%
  left_join(age, by = c("clust", "hh")) %>%
  select(clust,
         eanum,
         hh,
         profit,
         ez,
         ez_u_r,
         u_r,
         u_r_c,
         edu,
         age,
         size,
         school_dis,
         road_dis,
         chem_fert,
         insecticide,
         irrigation)

# Check for missing values.

base %>%
  filter(if_any(everything(), is.na)) %>%
  count()

# Missing values are because there is an enumeration area from an urban locality
# and there is likely is no information regarding chemical fertilizers, insecticides, and 
# irrigation because it is urban. Deciding to update missing values for all three
# variables to no (2).

base <- base %>%
  mutate(chem_fert = replace_na(chem_fert, 2),
         insecticide = replace_na(insecticide, 2),
         irrigation = replace_na(irrigation, 2))



# Reorder Factor Levels for Categorical Data ------------------------------

# Regional Data:
# Determine region with greatest number of households to use as default.

base %>%
  group_by(ez) %>%
  summarize(households = n()) %>%
  mutate(percent = round(households / sum(households) * 100, 3)) %>%
  arrange(desc(households))

base %>%
  group_by(ez_u_r) %>%
  summarize(households = n()) %>%
  mutate(percent = round(households / sum(households) * 100, 3)) %>%
  arrange(desc(households))

base  %>%
  group_by(u_r) %>%
  summarize(households = n()) %>%
  mutate(percent = round(households / sum(households) * 100, 3)) %>%
  arrange(desc(households))

base  %>%
  group_by(u_r_c) %>%
  summarize(households = n()) %>%
  mutate(percent = round(households / sum(households) * 100, 3)) %>%
  arrange(desc(households))

# There is no longer any households from the capital region. Removing the 
# urban/rural/capital variable. Setting categorical factor levels for locality 
# by largest value. With ecological region: forest, savannah, coastal. With 
# density: rural, urban.

base <- base %>%
  select(-u_r_c) %>%
  mutate(ez = factor(ez, levels = c('Forest', 'Savannah', 'Coastal')),
         ez_u_r = factor(ez_u_r, levels = c('Rural Forest', 'Rural Savannah', 'Rural Coastal', 'Other Urban')),
         u_r = factor(u_r, levels = c('Rural', 'Urban')))

# Education Data:
# Ordering by least to greatest level of educational attainment.  

base <- base %>%
  mutate(edu = factor(edu, levels = c('none', 'less_basic', 'basic', 'more_basic')))

# Community Agriculture Data: 
# Ordering by no impact as the default. Change all values to yes or no.

base <- base %>%
  mutate(chem_fert = case_when(chem_fert == 1 ~ 'yes',
                               chem_fert == 2 ~ 'no',
                               TRUE ~ NA_character_),
         chem_fert = factor(chem_fert, levels = c('no', 'yes')),
         insecticide = case_when(insecticide == 1 ~ 'yes',
                                 insecticide == 2 ~ 'no',
                                 TRUE ~ NA_character_),
         insecticide = factor(insecticide, levels = c('no', 'yes')),
         irrigation = case_when(irrigation == 1 ~ 'yes',
                                irrigation == 2 ~ 'no',
                                TRUE ~ NA_character_),
         irrigation = factor(irrigation, levels = c('no', 'yes')))


# Save Data ---------------------------------------------------------------

saveRDS(base, file = "data/base.RData")

# Clean Up Environment ----------------------------------------------------

# Remove environmental variables no longer needed.

rm(list = c(
  "ag_profit",
  "age",
  "c_ag",
  "c_edu",
  "c_road",
  "c_size",
  "edu",
  "profit",
  "region"
))

