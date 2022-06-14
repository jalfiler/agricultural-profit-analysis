# Objective: Prepare explanatory variable of educational attainment at the
# household level.


# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(haven)


# Data Understanding and Plan ---------------------------------------------

# Education variables are categorical and based on type of education instead
# of years of education, a ranking system is needed to organize the categories
# from least to greatest amount of education. The documentation (G4report.pdf)
# provided groups educational attainment by the following: never been to school,
# less than MSLC/BECE, MSLC/BECE, and secondary or higher.

# MSLC / BECE stands for Middle School Leaving Certificate / Basic Education
# Certificate Examination and seems to be taken after middle school around 9th
# grade. It is considered a basic level of education.

# For clarity, educational attainment categories will be: none, less than basic,
# basic, more than basic.

# It is unclear if all individuals that completed JSS and Middle education
# levels achieved their MSLC/BECE qualification. Both columns will be used
# to build an educational attainment grouping similar to the documentation. The
# education qualification column will be used as the best source of truth for
# the educational attainment categories as this matches to the documentation
# and it is likely that field is more stable. Following are specific assumptions
# for highest educational attainment.

# Highest Educational Attainment
# If qual = none and lvl = none, then none
# If qual = none and lvl != none, then less_basic
# If qual = unknown and lvl in (unknown, none), then none
# If qual = unknown and lvl in (< basic, > basic), then less_basic
# If qual = basic, then basic
# If qual = > basic, then more_basic

# As a person must be at least 15 years old to have had the opportunity to obtain
# a basic level of education, educational attainment will be limited to people
# that are at least 15 years old. Once educational attainment by individual is
# determined, the highest education attained per household will be added as an
# explanatory variable.

# Using the documentation provided (G4report.pdf) and following websites:
# Sources: https://www.aacrao.org/edge/country/ghana
# https://files.eric.ed.gov/fulltext/EJ1248146.pdf
# https://en.wikipedia.org/wiki/Education_in_Ghana
# https://wenr.wes.org/2019/04/education-in-ghana

# Assigned educational attaiment categories to educational qualification and
# level codes. Assigned other value wih unknown.

# Highest Educational Qualification
# 01 None - none
# 02 MSLC / BECE - basic
# 03 Voc / Comm - more than basic
# 04 'O' Level - more than basic
# 05 SSS - more than basic
# 06 'A' Level - more than basic
# 07 T / T Cert. B - more than basic
# 08 T / T Cert. A - more than basic
# 09 Nursing - more than basic
# 10 Tech / Prof Cert. - more than basic
# 11 Tech / Prof Dip. - more than basic
# 12 Bachelor - more than basic
# 13 Masters - more than basic
# 14 Doctorate - more than basic
# 96 Other - unknown

# Highest Education Level Completed
# 01 None - none
# 02 Kindergarten - less than basic
# 03 Primary - less than basic
# 04 Middle - less than basic
# 05 JSS - less than basic
# 06 SSS - more than basic
# 07 Voc / Comm - more than basic
# 08 Sec. (O Level) - more than basic
# 09 Sixth Form - more than basic
# 10 Teach. Train. - more than basic
# 11 Technical - more than basic
# 12 P/Sec. T/T - more than basic
# 13 Nursing - more than basic
# 14 P/Sec. Nursing - more than basic
# 15 Polytechnic - more than basic
# 16 University - more than basic
# 17 Koranic stage - less than basic
# 96 Other - unknown

# Load Educational Attainment Data ----------------------------------------

# Select enumeration area number (clust), household ID (nh), person ID (pid),
# and age in years (agey) from sec1.dta. Change nh to hh to make more
# intuitive.

# Age
# Data file used: sec1
# Columns: clust, nh, pid, agey

age <- read_dta("raw_data/sec1.dta") %>%
  select(clust,
    hh = nh,
    pid,
    agey
  )

# Select enumeration area number (clust), household ID (nh), person ID (pid),
# if the person ever attended school (s2aq1), highest education level
# completed (s2aq2), and the highest education qualification (s2aq3).

# Educational level and attainment
# Data file used: sec2a
# Columns: clust, nh, pid, s2aq3, s2aq2, s2aq1

edu <- read_dta("raw_data/sec2a.dta") %>%
  select(clust,
    hh = nh,
    pid,
    edu_attend = s2aq1,
    edu_qual = s2aq3,
    edu_lvl = s2aq2
  )


# Clean Data --------------------------------------------------------------

# Select individuals that are 15+ years old. Use a left join to merge with
# table containing highest education qualification and level.

edu <- age %>%
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
  )

# Check for duplicate values.

edu %>%
  group_by(clust, hh, pid) %>%
  filter(n() > 1)

# Check for missing values.

edu %>%
  filter(if_any(everything(), is.na)) %>%
  count()

# 5,148 observations with missing values. Missing values are found
# in edu_attend, mostly in edu_qual and edu_lvl. Some missing
# values in edu_qual and edu_lvl are because individual did not
# attend school. Update those missing values to 1 (none).

edu <- edu %>%
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
  )

edu %>%
  filter(if_any(everything(), is.na)) %>%
  count()

# There are now only 550 observations with missing values. All
# observations are missing values for the three education variables. These
# missing values may not matter at the greatest educational attainment at the
# household level so will address later.

# Assign education categories to qualification and level.

edu <- edu %>%
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
  )

# Confirm no new missing values were created.

edu %>%
  filter(if_any(everything(), is.na)) %>%
  count()

# Create educational attainment and rank.

edu <- edu %>%
  mutate(
    edu = case_when(
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
      TRUE ~ NA_real_
    )
  )

# Check percentages again
edu %>%
  group_by(edu) %>%
  summarize(people = n()) %>%
  mutate(percent = round(people / sum(people) * 100, 3)) %>%
  arrange(desc(people))

# The percentages are similar but not exact to the documentation (G4report.pdf,
# p. 8) Appendix 3 does not contain details of calculation so it is unclear
# where calculations differ.Proceeding with estimates as it seems like they are
# in the same ballpark. Next step, take highest education attainment per
# household.

edu <- edu %>%
  group_by(clust, hh) %>%
  slice(which.max(rank)) %>%
  select(
    clust,
    hh,
    edu
  )

# Check again for missing values.

edu %>%
  filter(if_any(everything(), is.na)) %>%
  count()

# There are 2 rows where educational attainment is missing. Choosing to replace
# with no education (none).

edu <- edu %>%
  mutate(edu = replace(edu, is.na(edu) == TRUE, "none"))

edu %>%
  filter(if_any(everything(), is.na)) %>%
  count()

# Confirmed no more missing values. Check for duplicates.

edu %>%
  group_by(clust, hh) %>%
  filter(n() > 1)

# Confirmed no duplicates. Educational attainment variable is ready to be
# merged with other data.


# Clean Up Environment ----------------------------------------------------

# Remove environment variables no longer needed.

rm(list = c(
  "age"
))
