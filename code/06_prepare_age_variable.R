# Objective: Prepare explanatory variable of average household age.

# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(haven)


# Load Data ---------------------------------------------------------------

# From initial model development, age at the household level was included and
# seemed to explain slightly more variation in the response variable. The
# original age variable was a random age from the household. Preparing average
# household age variable to see if that improves the model. This variable likely 
# does not indicate the actual age of someone in the household, but more helps 
# to explain if the household consists of younger or older individuals.

# Ages of household members
# Data file used: sec1
# Columns: clust, nh, pid, agey

age <- read_dta("raw_data/sec1.dta") %>%
  select(clust,
         hh = nh,
         pid,
         age = agey)


# Clean Data --------------------------------------------------------------

# Check for missing values.

age %>%
  filter(if_any(everything(), is.na)) %>%
  count()

# No missing values. Check for duplicates.

age %>%
  group_by(clust, hh, pid) %>%
  filter(n() > 1)

# There are no duplicate rows. Check age distribution to see if makes more 
# sense to take median or mean as the average age per household.

summary(age$age)

ggplot(data = age, mapping = aes(x = age)) +
  geom_histogram(fill = "black", bins = 50) +
  xlab("Age (years)") +
  ylab("Count") +
  theme_minimal()

# Age distribution has a strong right skew, so using the median age of each
# household.

age <- age %>%
  group_by(clust, hh) %>%
  summarize(age = median(age))

# Average household age variable is ready to be merged with other data.
