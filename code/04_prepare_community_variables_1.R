# Objective: Prepare explanatory community variables at the enumeration area
# level.


# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(haven)


# Load Data ---------------------------------------------------------------

# Use the community survey to list all community factors that might be relevant
# to include for analysis. If these factors are significant, may need review if
# documentation again to see if there is a more detailed factor that could
# provide additional information.

# Basic information about community survey.

cs0 <- read_dta("raw_data/cs0.dta")

# There can be multiple communities in an enumeration area, but looking at the
# data there is no way to determine what households belong to which community,
# so these data will likely not be useful for analysis.

# Basic Information about community

cs1 <- read_dta("raw_data/cs1.dta")

# Important Factors:
# s1q1 - community size by number of people

# Economy / Infrastructure

cs2 <- read_dta("raw_data/cs2.dta")

# Important Factors:
# s2q1a - most important economic activity, includes farming
# s2q4 and s2q5 - distance to motorable road
# s2q8 - any electricty/generators in community
# s2q10 - piped water
# s2q12 - drinking water dry season
# s2q13 - drinking water rainy season
# s2q14 - restaurant
# s2q15 and s2q16 - post office/telephone
# s2q17 and s2q18 - bank
# s2q19 and s2q22 - periodic market
# s2q21 - market frequency or daily
# s2q23 and s2q25 - nearest public transport
# s2q26 - public transport frequency
# s2q29 - people leave for agricultural  work
# s2q34a - people come for agricultural work
# s2q38a-c - if major community development project is a market or farm

# Education

cs3 <- read_dta("raw_data/cs3.dta")

# Important Factors:
# s3q1 and 2 - primary school distance


# Health

cs4a <- read_dta("raw_data/cs4a.dta")
cs4b <- read_dta("raw_data/cs4b.dta")
cs4c <- read_dta("raw_data/cs4c.dta")

# Important Factors:
# s4q1-2 - nearest health professional
# s4q5-6 - nearest health business

# Agriculture

cs5a <- read_dta("raw_data/cs5a.dta")
cs5b <- read_dta("raw_data/cs5b.dta")

# Important Factors:
# s5q1a - most important crop grown
# s5q2 - planting frequency for most important crop
# 5q4 - how is most important crop generally sold
# s5q5 and 6 - agriculture extension center distance
# s5q7 - agriculture extension officer
# s5q10 and 12 - agriculture co-op
# s5q13 - number of tractors
# s5q14 - rice husking machine
# s5q15 - chemical fertilizer
# s5q16 - insecticides / herbicides
# s5q17 - irrigated fields
# s5q18 - more or less rain this year than last year, might help indicate natural
# disaster impact?
# s5q20 - sharecroppers present
# s5q22 - agriculture  wage, maybe take max wage?
# s5q24 - mutual aid system with farmers for field work

# There are a lot of community factors to choose from. One community factor will
# be cleaned for an initial model and more community factors will be added as
# time allows.

# TODO: Add more community factors.


# Clean Data --------------------------------------------------------------

# Community Size
# Start with size of the community as it may be an important factor. The number
# of people may be a proxy for developmental pressures that could increase the
# costs of agriculture, support available for agricultural activities such as
# co-ops that may decrease the costs of agriculture, or something else.

c_size <- cs1 %>%
  select(eanum,
    size = s1q1
  )

# Check for duplicate values.

c_size %>%
  group_by(eanum, size) %>%
  filter(n() > 1)

# There are 39 duplicate rows. Remove duplicates.

c_size <- c_size %>%
  distinct()

# Check for multiple communities in eanum.

c_size_dups <- c_size %>%
  group_by(eanum) %>%
  filter(n() > 1)

# There are 27 eanum duplicates, possibly indicating multiple communities. Two
# of the duplicates have missing values, so I can select the duplicate with a
# community size value.

c_size <- c_size %>%
  filter(!(eanum %in% c(419, 485) & is.na(size) == TRUE))

c_size_dups <- c_size %>%
  group_by(eanum) %>%
  filter(n() > 1)

# For the others, assuming they represent separate community sizes within the
# eanum area, I think it makes the most sense to sum community sizes.

c_size <- c_size %>%
  group_by(eanum) %>%
  summarize(size = sum(size))

# Check for missing values.

c_size %>%
  filter(if_any(everything(), is.na)) %>%
  count()

# There are 33 eanum rows missing the community size. I'm not sure what the best
# to handle is, remove or add average. To keep in more data, using average.

summary(c_size$size)

ggplot(data = c_size, mapping = aes(x = size)) +
  geom_histogram(fill = "black", bins = 50) +
  xlab("Community Size") +
  ylab("Count") +
  theme_minimal()

# The data is skewed right, so I think it makes the most sense to fill the
# missing values with the median.

c_size <- c_size %>%
  mutate(size = replace_na(size, median(size, na.rm = TRUE)))

c_size %>%
  filter(if_any(everything(), is.na)) %>%
  count()

# Community size is ready to add to base model data.

# Clean Up Environment ----------------------------------------------------

# Clear plots.

dev.off()

# Remove environment variables no longer needed.

rm(list = c(
  "c_size_dups",
  "cs0",
  "cs1",
  "cs2",
  "cs3",
  "cs4a",
  "cs4b",
  "cs4c",
  "cs5a",
  "cs5b"
))
