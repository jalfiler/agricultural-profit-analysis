# Objective: Complete descriptive statistics of base model data.


# Load Libraries ----------------------------------------------------------

library(tidyverse)


# Load Data ---------------------------------------------------------------

base <- readRDS("data/base.RData") %>%
  select(
    clust,
    profit,
    ez,
    ez_u_r,
    u_r,
    edu,
    age,
    size,
    school_dis,
    road_dis,
    chem_fert,
    insecticide,
    irrigation
  )


# Check Correlation -------------------------------------------------------

# Create correlation matrix to check for perfect multicollinearity between
# continuous variables. Perfect correlation will cause a model error and one of
# the perfectly correlated variables will need to be removed.

base %>%
  select(profit, age, size, school_dis, road_dis) %>%
  cor()

# There are low correlations between all continuous variables.

# TODO: Determine how to quickly check all categorical variables for perfect
# correlation. It it likely several are perfectly correlated as ez_u_r
# (ecological zone urban vs. rural) is practically a combination of ez
# (ecological zone) and u_r (urban vs. rural).


# Agricultural Profit per Acre --------------------------------------------

# Summarize profit.

summary(base$profit)

# Graph profit distribution.

ggplot(data = base, mapping = aes(x = profit)) +
  geom_histogram(fill = "black", bins = 100) +
  xlab("Agricultural Profit per Acre (cedis)") +
  ylab("Household Count") +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal()

# Save agricultural profit per acre distribution graph.

ggsave("figures/ds1_profit_dist.jpg", height = 6, width = 8)

# The agricultural profit distribution is strongly skewed right so the median
# is a better metric of the average than the mean, and there appears to be
# outliers present. The average agricultural profit per household is 133,690
# cedis per acre, with 75% of profits ranging between 42,835 and 334,029 cedis
# per acre.


# Ecological Zone ---------------------------------------------------------

# Summarize households by ecological zone.

base %>%
  group_by(ez) %>%
  summarize(households = n()) %>%
  mutate(percent = round(households / sum(households) * 100, 3)) %>%
  arrange(desc(households))

# Most of the households are in forest areas (52.8%), over a quarter are in
# savannah areas (27.8%), and a fifth are in coastal areas (19.5%).

# Summarize agricultural profit by ecological zone.

base %>%
  group_by(ez) %>%
  summarize(
    min_profit = min(profit),
    q1_profit = quantile(profit, 0.25),
    median_profit = median(profit),
    mean_profit = mean(profit),
    q3_profit = quantile(profit, 0.75),
    max_profit = max(profit)
  )

# Coastal areas have the most extreme agricultural profits. All areas have
# negative and positive profits. All distributions appear to be skewed right as
# the median is less than the mean.


# Ecological Zone Urban vs. Rural -----------------------------------------

# Summarize households by ecological urban vs. rural zone.

base %>%
  group_by(ez_u_r) %>%
  summarize(households = n()) %>%
  mutate(percent = round(households / sum(households) * 100, 3)) %>%
  arrange(desc(households))

# Most of the households are in rural forest areas (51.3%), over a quarter are
# in rural savannah areas (27.3%), a fifth are in rural coastal areas (19.5%),
# and 1.98% are in other urban areas.

# Rural Coastal and Coastal have the same number of households, meaning they
# are perfectly correlated. A model cannot have both variables otherwise there
# will be an error.

# Summarize agricultural profit by ecological zone.

base %>%
  group_by(ez_u_r) %>%
  summarize(
    min_profit = min(profit),
    q1_profit = quantile(profit, 0.25),
    median_profit = median(profit),
    mean_profit = mean(profit),
    q3_profit = quantile(profit, 0.75),
    max_profit = max(profit)
  )

# The agricultural profit breakdown by ecological zone urban vs. rural is
# similar to the breakdown by ecological zone. Other urban has the smallest
# minimum and maximum profits.


# Urban vs. Rural ---------------------------------------------------------

# Summarize households by urban vs. rural.

base %>%
  group_by(u_r) %>%
  summarize(households = n()) %>%
  mutate(percent = round(households / sum(households) * 100, 3)) %>%
  arrange(desc(households))

# Most of the households are in rural areas (98%), 1.98% are in urban areas.

# Urban and Other Urban have the same number of households, meaning they
# are perfectly correlated. A model cannot have both variables otherwise there
# will be an error.

# Summarize agricultural profit by urban vs. rural.

base %>%
  group_by(u_r) %>%
  summarize(
    min_profit = min(profit),
    q1_profit = quantile(profit, 0.25),
    median_profit = median(profit),
    mean_profit = mean(profit),
    q3_profit = quantile(profit, 0.75),
    max_profit = max(profit)
  )

# The agricultural profit breakdown by urban vs. rural is the same as the
# ecological zones urban vs. rural except all rural ecological zones are
# summarized as rural.


# Educational Attainment --------------------------------------------------

# Summarize households by greatest household educational attainment.

base %>%
  group_by(edu) %>%
  summarize(households = n()) %>%
  mutate(percent = round(households / sum(households) * 100, 3)) %>%
  arrange(desc(households))

# Almost half of households have a basic education (40.8%), over a quarter have
# none (27.9%), a little over a fifth have less than a basic education (21%),
# and about 10% have attained higher than a basic educational qualification.

# Summarize agricultural profit by educational attainment.

base %>%
  group_by(edu) %>%
  summarize(
    min_profit = min(profit),
    q1_profit = quantile(profit, 0.25),
    median_profit = median(profit),
    mean_profit = mean(profit),
    q3_profit = quantile(profit, 0.75),
    max_profit = max(profit)
  )

# All educational attainment groups have positive and negative agricultural
# profits and distributions that are skewed right. There does not seem to be as
# much extreme variation as the regional variables.


# Average Household Age ----------------------------------------------------

# Summarize households by average household age.

summary(base$age)

# Graph average household age distribution.

ggplot(data = base, mapping = aes(x = age)) +
  geom_histogram(fill = "black", bins = 100) +
  xlab("Median Household Age") +
  ylab("Household Count") +
  theme_minimal()

# Save average household age distribution graph.

ggsave("figures/ds1_med_hh_age_dist.jpg", height = 6, width = 8)

# The median household age is strongly skewed right so the median is a better
# metric of the average than the mean, and there appears to be outliers present.
# The overall shape of the distribution seems to make sense as there are fewer
# households where the median age is older.The average median household age is
# 18 years with 75% of households with ages between 12 and 29 years. This could
# indicate many households where most people tend to be younger or households
# with lots of children.

# Graph agricultural profit by average household age.

ggplot(base, aes(age, profit)) +
  geom_point() +
  geom_smooth() +
  xlab("Median Household Age (years)") +
  ylab("Agricultural Profit per Acre (cedis)") +
  scale_y_continuous(labels = scales::comma) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black")
  )

# Save agricultural profit per acre by median household age graph.

ggsave("figures/ds1_profit_med_hh_age.jpg", height = 6, width = 8)

# There appear to be outliers. The average agricultural profit does not seem to
# change much based on median household age.


# Community Size ----------------------------------------------------------

# Summarize households by community size.
summary(base$size)

# Graph community size distribution.

ggplot(data = base, mapping = aes(x = size)) +
  geom_histogram(fill = "black", bins = 100) +
  xlab("Community Size (people)") +
  ylab("Household Count") +
  theme_minimal()

# Save community size distribution graph.

ggsave("figures/ds1_com_size_dist.jpg", height = 6, width = 8)

# The community size is strongly skewed right so the median is a better metric
# of the average than the mean, and there appears to be outliers present. The
# average community size is 649 people, with 75% of households in communities
# with 341 to 1,051 people.

# Graph agricultural profit by community size.

ggplot(base, aes(size, profit)) +
  geom_point() +
  geom_smooth() +
  xlab("Community Size (people)") +
  ylab("Agricultural Profit per Acre (cedis)") +
  scale_y_continuous(labels = scales::comma) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black")
  )

# Save agricultural profit per acre by community size graph.

ggsave("figures/ds1_profit_com_size.jpg", height = 6, width = 8)

# There appear to be outliers. The average agricultural profit does not seem to
# change much based on community size.


# Distance to Nearest Primary School --------------------------------------

# Summarize households by distance to nearest primary school.

summary(base$school_dis)

# Graph distance to nearest primary school distribution.

ggplot(data = base, mapping = aes(x = school_dis)) +
  geom_histogram(fill = "black", bins = 100) +
  xlab("Distance to Nearest Primary School (kilometers)") +
  ylab("Household Count") +
  theme_minimal()

# Save distance to primary school distribution.

ggsave("figures/ds1_school_dis_dist.jpg", height = 6, width = 8)

# The distance to nearest primary school (kilometers) is strongly skewed right
# so the median is a better metric of the average than the mean, and there
# appears to be outliers present. The average distance is 0 kilometers, with
# 75% of households with a distance of 0 kilometers. It seems like most
# households have a primary school in their community, so this may not be an
# impactful variable in our model.

# Graph agricultural profit by distance to primary school.

ggplot(base, aes(school_dis, profit)) +
  geom_point() +
  geom_smooth() +
  xlab("Distance to Primary School (kilometers)") +
  ylab("Agricultural Profit per Acre (cedis)") +
  scale_y_continuous(labels = scales::comma) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black")
  )

# Save agricultural profit per acre by distance to primary school graph.

ggsave("figures/ds1_profit_school_dis.jpg", height = 6, width = 8)

# There appear to be outliers.The distance is split into two groups, with most
# households clustered around zero kilometers and a few households clustered
# at greater than 300 kilometers. The average agricultural profit does not seem
# to change much by distance, however there is a large amount of error and
# convex upwards curve, likely due to the split in households.


# Distance to Nearest Motorable Road --------------------------------------

# Summarize households by distance to nearest motorable road.

summary(base$road_dis)

# Graph distance to nearest motorable road distribution.

ggplot(data = base, mapping = aes(x = road_dis)) +
  geom_histogram(fill = "black", bins = 30) +
  xlab("Distance to Nearest Motorable Road (kilometers)") +
  ylab("Household Count") +
  theme_minimal()

# Save distance to motorable road distribution.

ggsave("figures/ds1_road_dis_dist.jpg", height = 6, width = 8)

# The distance to nearest motorable road (kilometers) is strongly skewed right
# so the median is a better metric of the average than the mean, and there
# appears to be outliers present. Also of interest, the distances appear to be
# in discrete groups. The average distance is 0 kilometers, with
# 75% of households with a distance of 0 kilometers. It seems like most
# households have a motorable road in their community, so this may not be an
# impactful variable in our model.

# Graph agricultural profit by distance to motorable road.

ggplot(base, aes(road_dis, profit)) +
  geom_point() +
  geom_smooth() +
  xlab("Distance to Motorable Road (kilometers)") +
  ylab("Agricultural Profit per Acre (cedis)") +
  scale_y_continuous(labels = scales::comma) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black")
  )

# Save agricultural profit per acre by distance to nearest road graph.

ggsave("figures/ds1_profit_road_dis.jpg", height = 6, width = 8)

# There appear to be outliers.The distance is split into discrete distances
# with more households clustered around zero, five, and 14 kilometers. The
# average agricultural profit is a squiggly line ranging mostly between 0 and
# 10 million cedis per acre, with the largest peak at around 5 kilometers. It
# is likely the average variation is due to the distances being in discrete
# groups.


# Use of Chemical Fertilizer ----------------------------------------------

# Summarize households by use of chemical fertilizer in community.

base %>%
  group_by(chem_fert) %>%
  summarize(households = n()) %>%
  mutate(percent = round(households / sum(households) * 100, 3)) %>%
  arrange(desc(households))

# Most households (58.7%) live in communities where chemical fertilizer is used.

# Summarize agricultural profit by use of chemical fertilizer.

base %>%
  group_by(chem_fert) %>%
  summarize(
    min_profit = min(profit),
    q1_profit = quantile(profit, 0.25),
    median_profit = median(profit),
    mean_profit = mean(profit),
    q3_profit = quantile(profit, 0.75),
    max_profit = max(profit)
  )

# The agricultural profit breakdown by use of chemical fertilizer appears to
# have slightly different but still skewed right distributions.


# Use of Insecticide ------------------------------------------------------

# Summarize households by use of insecticide in community.

base %>%
  group_by(insecticide) %>%
  summarize(households = n()) %>%
  mutate(percent = round(households / sum(households) * 100, 3)) %>%
  arrange(desc(households))

# Most households (68.3%) live in communities where insecticide is used.

# Summarize agricultural profit by use of insecticide.

base %>%
  group_by(insecticide) %>%
  summarize(
    min_profit = min(profit),
    q1_profit = quantile(profit, 0.25),
    median_profit = median(profit),
    mean_profit = mean(profit),
    q3_profit = quantile(profit, 0.75),
    max_profit = max(profit)
  )

# The agricultural profit breakdown by use of insecticide in the community
# appears to have slightly different but still skewed right distributions.


# Presence of Irrigation --------------------------------------------------

# Summarize households by use of irrigation for agriculture in the community.

base %>%
  group_by(irrigation) %>%
  summarize(households = n()) %>%
  mutate(percent = round(households / sum(households) * 100, 3)) %>%
  arrange(desc(households))

# Most households (92.5%) live in communities where there is no agricultural
# irrigation.

# Summarize agricultural profit by use of agricultural irrigation.

base %>%
  group_by(irrigation) %>%
  summarize(
    min_profit = min(profit),
    q1_profit = quantile(profit, 0.25),
    median_profit = median(profit),
    mean_profit = mean(profit),
    q3_profit = quantile(profit, 0.75),
    max_profit = max(profit)
  )

# The agricultural profit breakdown by use of insecticide in the community
# appears to have slightly different but still skewed right distributions.
