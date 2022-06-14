# Objective: Prepare response variable of agricultural profit per acre at 
# the household level.


# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(haven)


# Load Data ---------------------------------------------------------------

# After reviewing the documentation, agricultural profit will be sourced from
# the agricultural income per household aggregate variable in the agg2.dta file.

# The "Estimation of Components of Household Incomes and Expenditures: A
# Methodological Guide base on the Ghana Living Standards Survey, 1991/1992 and
# 1998/1999" (Aggregate.pdf) defines agricultural income as income derived
# explicitly from the sale of cash crops or livestock products, or implicitly
# from the consumption of homegrown produce. The recommend using the estimate
# that takes the gross agricultural output and then deducting the total input
# costs of agricultural activity. The agricultural income estimate is provided
# as uncorrected and corrected. The corrected agricultural income estimate
# (agri1c) will be used because that is standardized, and accounts for outliers,
# missing values, and specific challenges with aggregation.

# The enumeration area number (clust) and household ID (nh) will be used as a
# unique key for each household.

# Agricultural profit
# Data file: agg2
# Columns: clust, nh, agri1c

# Select enumeration area number (clust), household ID (nh), and corrected
# agricultural income (agri1c) from the agg2.dta file. Rename nh to hh and
# agri1c to profit so the variable names are more intuitive.

ag_profit <- read_dta("raw_data/agg2.dta") %>%
  select(clust,
    hh = nh,
    profit = agri1c
  )

# Agricultural profit will be divided by acre to allow for regional comparison.
# Agricultural land information is in the sec8b.dta file.

# Agricultural land details
# Data file used: sec8b
# Columns: clust, nh, farmcd, s8bq4a, s8bq4b

# Select enumeration area number (clust), household ID (hh), farm ID (farmcd),
# farm size (s8bq4a), and size unit (s8bq4b). Rename nh to hh, farmcd to
# farm_id, s8bq4a to size and s8bq4b to unit so variable names are more
# intuitive.

ag_area <- read_dta("raw_data/sec8b.dta") %>% select(
  clust = clust,
  hh = nh,
  farm_id = farmcd,
  size = s8bq4a,
  unit = s8bq4b
)


# Clean Data --------------------------------------------------------------

# Check agricultural profit for missing values.

ag_profit %>%
  filter(if_any(everything(), is.na))

# Check agricultural profit for duplicate values.

ag_profit %>%
  group_by(clust, hh) %>%
  filter(n() > 1)

# Check agricultural area  for missing values.

ag_area %>%
  filter(if_any(everything(), is.na))

# There is one observation with missing values for both farm size and size unit.
# Removing observation instead of filling with an estimate as it is only a
# single observation and it is unclear what would be a good estimate as both
# size and unit are missing.

ag_area <- ag_area %>%
  drop_na()

# Check agricultural area for duplicate values.

ag_area %>%
  group_by(clust, hh, farm_id) %>%
  filter(n() > 1)

# Determine best unit to use for all farms by looking at household breakdown.

ag_area %>%
  group_by(unit) %>%
  summarize(total = n(), mean = mean(size)) %>%
  mutate(percent = round(total / sum(total) * 100, 3))

# Units Key:
# 1 = acres
# 2 = poles
# 3 = ropes
# 4 = other

# Most farms are in acres, so acreage will be used as the standard unit. From
# the documentation provided, one pole is equal to one acre, and nine ropes are
# equal to one acre. The 13 observations with other units will be removed as
# units are unknown. They make up 0.1% of the data, so it is unlikely to have an
# large impact. After calculating acreage, grouping by household.

ag_area <- ag_area %>%
  filter(unit %in% c(1, 2, 3)) %>%
  mutate(acres = case_when(
    unit %in% c(1, 2) ~ size,
    unit == 3 ~ size / 9,
    TRUE ~ NA_real_
  )) %>%
  group_by(clust, hh) %>%
  summarize(acres = sum(acres))

# There are only 3,936 households with agricultural land. Based on observations
# removed, I would expect to see 5,984 (5,998 - 14) households.

ag_profit %>%
  filter(profit <= 0) %>%
  count()

# Agricultural profit was provided for all households, but not all households
# make a living from agricultural activities, and so would not have any
# agricultural land. I would expect agricultural profit to be zero for these
# households.

no_ag_area <- ag_profit %>%
  left_join(ag_area, by = c("clust", "hh")) %>%
  filter(is.na(acres))

summary(no_ag_area$profit)

no_ag_area %>%
  mutate(profit_cat = case_when(
    profit < 0 ~ "< 0",
    profit == 0 ~ "0",
    profit > 0 ~ "> 0",
    TRUE ~ NA_character_
  )) %>%
  group_by(profit_cat) %>%
  summarize(households = n()) %>%
  mutate(percent = round(households / sum(households) * 100, 3)) %>%
  arrange(desc(households))

# Most households without agricultural land (79%) have an agricultural profit
# of zero. It is unclear why there are some households without agricultural
# land that make an agricultural profit. This may be due to data error or
# perhaps there are agricultural activities that do not involve owning or
# operating farmland. Either way, as agricultural profit needs to be per acre,
# these observations are being removed even though it is ~ 34% (22048 / 5984)
# of the data.

# TODO: Investigate why there are households with non-zero agricultural profits
# and no agricultural land.


# Determine Agricultural Profit by Area -----------------------------------

# Use left join to merge agricultural profit for households that only have
# agricultural land. Transform acres and profit into profit by acre.

profit <- left_join(ag_area, ag_profit, by = c("clust", "hh")) %>%
  transmute(clust,
    hh,
    profit = profit / acres
  )

# Check for missing values and duplicate values as a quality check.

profit %>%
  filter(if_any(everything(), is.na))

profit %>%
  group_by(clust, hh) %>%
  filter(n() > 1)

# No missing or duplicate values, agricultural profit by acre is ready to be
# merged with the other data.


# Clean Up Environment ----------------------------------------------------

# Clear plots.

dev.off()

# Remove environment variables no longer needed.

rm(list = c(
  "ag_area",
  "ag_profit",
  "no_ag_area"
))
