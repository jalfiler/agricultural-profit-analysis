# Objective: Develop linear regression model for agricultural profit by area.


# Load Libraries ----------------------------------------------------------

library(tidyverse)
library(jtools)
library(ggstance)
library(broom.mixed)


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


# Model 1: All Variables --------------------------------------------------

# Model 1 includes all variables. There will be two variable pairs that have
# coefficient errors due to perfect correlation, ezCoastal / ez_u_rCoastal and
# u_rUrban / ez_u_rUrban.

lm_1 <- lm(profit ~ ez + ez_u_r + u_r + edu + age + size + school_dis +
  road_dis + chem_fert + insecticide + irrigation, data = base)

summary(lm_1)

# Coefficient Interpretation (all):

# ezSavannah: Profit increases by 737,471.26 cedis in savannah areas relative
# to forest areas, not significant.

# ezCoastal: Profit increases by 340,103.64 cedis in coastal areas relative to
# forest areas, significant at less than the 0.1% level.

# ez_u_rRural Savannah: Profit decreases by 827,556.37 cedis in rural savannah
# areas relative to rural forest areas, significant at at the 10% level.

# ez_u_rOther Urban: Profit increases by 145,586.60 cedis in urban areas
# relative to rural forest areas, not significant.

# eduLess than MSLC/BECE: Profit increases by 106,896.13 cedis if the household
# educational attainment is less than basic relative to if the household
# educational attainment is none, not significant.

# eduMSLC/BECE: Profit decreases by 106,896.13 cedis if the household
# educational attainment is basic relative to if the household educational
# attainment is none, not significant.

# eduSecondary or higher: Profit decreases by 38,768.01 cedis if the household
# educational attainment is secondary or higher relative to if the household
# educational attainment is none, not significant.

# age: Profit decreases by 3,887.54 cedis as the median household age increases
# by one year, significant at the 5% level.

# size: Profit decreases by 27.24 cedis as the community size increases by one
# person, not significant.

# school_dis: Profit increases by 769.60 cedis as the distance to closest
# primary school increases by one kilometer, not significant.

# road_dis: Profit increases by 60,755.57 cedis as the distance to closest
# motorable road increases by one kilometer, significant at the 0.1% level.

# chem_fertyes: Profit decreases by 135,354.41 cedis in areas that use chemical
# fertilizers relative to areas that do not use chemical fertilizers,
# significant at the 5% level.

# insecticideyes: Profit increases by 250,604.22 cedis in areas that use
# insecticide on agriculture relative to areas that do not use insecticide,
# significant at the 0.1% level.

# irrigationyes: Profit decreases by 85,306.87 cedis in areas that have
# irrigation relative to areas that do not have agricultural irrigation, not
# significant.


# Coefficient Error Resolution:

# Either ez and u_r, or ez_u_r need to be removed from the model due to perfect
# correlation between placeholder variables leading to model coefficient error.
# Does dropping ez and u_r, or ez_u_r make a better model?

# Model 1a: all non-regional variables and combined ecological zone urban vs.
# rural variable.

lm_1a <- lm(profit ~ ez_u_r + edu + age + size + school_dis + road_dis +
  chem_fert + insecticide + irrigation, data = base)

summary(lm_1a)

# Model 1b: all non-regional variables, urban vs. rural variable, ecological
# zone variable.

lm_1b <- lm(profit ~ ez + u_r + edu + age + size + school_dis + road_dis +
  chem_fert + insecticide + irrigation, data = base)

summary(lm_1b)

# There is not a strong difference between Model 1a and Model 1b. Proceeding
# with Model 1a as it seems to have a slightly higher adjusted R-squared value,
# (0.01893 instead of 0.01876), and it gets the savannah placeholder variable
# closer to being significant with a p-value going from 0.361112 to 0.240258.


# Coefficient Interpretation:

# Graph all coefficient estimates with 95% confidence interval.

plot_summs(lm_1a, scale = TRUE, colors = "black", coefs = c(
  "Rural Savannah" = "ez_u_rRural Savannah",
  "Rural Coastal" = "ez_u_rRural Coastal",
  "Urban" = "ez_u_rOther Urban",
  "Less Than Basic Education" = "eduless_basic",
  "Basic Education" = "edubasic",
  "Higher Than Basic Education" = "edumore_basic",
  "Average Household Age" = "age",
  "Community Size" = "size",
  "Distance to Nearest Primary School" = "school_dis",
  "Distance to Nearest Motorable Road" = "road_dis",
  "Chemical Fertilizer" = "chem_fert",
  "Insecticide" = "insecticide",
  "Irrigation" = "irrigation"
))

ggsave("figures/lm_1a_coef.jpg", height = 6, width = 8)

# Interpret significant coefficients.

# ez_u_rRural Coastal: Profit increases by 343,001.23 cedis in rural coastal
# areas relative to rural forest areas, significant at less than the 0.01% level.

# age: Profit decreases by 3,482.89 cedis as the median household age increases
# by one year, significant at the 5% level.

# road_dis: Profit increases by 60,905.38  cedis as the distance to closest
# motorable road increases by one kilometer, significant at less than the 0.1%
# level.

# chem_fertyes: Profit decreases by 146,989.99 cedis in areas that use chemical
# fertilizers relative to areas that do not use chemical fertilizers,
# significant at the 5% level.

# insecticideyes: Profit increases by 261,406.78 cedis in areas that use
# insecticide on agriculture relative to areas that do not use insecticide,
# significant at the 0.1% level.

# Model Validation:

# The adjusted R-squared value is 0.01893, so the model explains only about
# 1.9% of the variation seen in the response variable. The F-statistic is 5.875
# and statistically significant at less than the 0.1% level.

# Check the normality assumption with the distribution of the standardized
# residuals.

ggplot(lm_1a, aes(x = rstandard(lm_1a))) +
  geom_histogram(
    bins = 50,
    color = "black",
    fill = "black"
  ) +
  xlab("Standardized Residuals") +
  ylab("Frequency") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black")
  )

ggsave("figures/lm_1a_resid_dist.jpg", height = 6, width = 8)

# The distribution is skewed right, so the normality assumption fails. The
# estimates are unbiased but the errors are biased. To resolve this issue, more
# date would be helpful.

# Check the constant variance assumption with a scatter plot of the fitted
# values and residuals.

ggplot(lm_1a, aes(x = fitted(lm_1a), y = resid(lm_1a))) +
  geom_point(color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth() +
  xlab("Fitted Values") +
  ylab("Residuals") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black")
  )

ggsave("figures/lm_1a_resid_fitted.jpg", height = 6, width = 8)

# The mean has a slight bend upwards and these data may be slightly
# heteroskedastic. It is difficult to interpret visually with potential
# outliers. As the mean stays close to zero and the overall spread seems to be
# balanced, the model seems to be valid.


# Model 2: All Significant Coefficients -----------------------------------

# Model 2 only includes variables with significant coefficients from Model 1.
# Some categorical variables have insignificant coefficients on placeholder
# variables. The categorical variable was kept if at least one placeholder
# variable coefficient was significant.

lm_2 <- lm(profit ~ ez_u_r + age + road_dis + chem_fert + insecticide,
  data = base
)

summary(lm_2)


# Coefficient Interpretation:

# Graph all coefficient estimates with 95% confidence interval.

plot_summs(lm_2, scale = TRUE, colors = "black", coefs = c(
  "Rural Savannah" = "ez_u_rRural Savannah",
  "Rural Coastal" = "ez_u_rRural Coastal",
  "Urban" = "ez_u_rOther Urban",
  "Average Household Age" = "age",
  "Distance to Nearest Motorable Road" = "road_dis",
  "Chemical Fertilizer" = "chem_fert",
  "Insecticide" = "insecticide"
))

ggsave("figures/lm_2_coef.jpg", height = 6, width = 8)

# Interpret significant coefficients.

# ez_u_rRural Coastal: Profit increases 353,835 cedis in rural coastal areas
# relative to rural forest areas, significant at less than the 0.01% level.

# age: Profit decreases by 3,896 cedis as the median household age increases
# by one year, significant at the 5% level.

# road_dis: Profit increases by 65,016 cedis as the distance to closest
# motorable road increases by one kilometer, significant at less than the 0.1%
# level.

# chem_fertyes: Profit decreases by 143,130 cedis in areas that use chemical
# fertilizers relative to areas that do not use chemical fertilizers,
# significant at the 5% level.

# insecticideyes: Profit increases by 247,928 cedis in areas that use
# insecticide on agriculture relative to areas that do not use insecticide,
# significant at the 0.1% level.


# Model Validation:

# The adjusted R-squared value is 0.01885, so the model explains only about
# 1.9% of the variation seen in the response variable. The F-statistic is 10.02
# and statistically significant at less than the 0.1% level.

# Check the normality assumption with the distribution of the standardized
# residuals.

ggplot(lm_2, aes(x = rstandard(lm_2))) +
  geom_histogram(
    bins = 50,
    color = "black",
    fill = "black"
  ) +
  xlab("Standardized Residuals") +
  ylab("Frequency") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black")
  )

ggsave("figures/lm_2_resid_dist.jpg", height = 6, width = 8)

# The distribution is skewed right, so the normality assumption fails. The
# estimates are unbiased but the errors are biased. To resolve this issue, more
# date would be helpful.

# Check the constant variance assumption with a scatter plot of the fitted
# values and residuals.

ggplot(lm_2, aes(x = fitted(lm_2), y = resid(lm_2))) +
  geom_point(color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth() +
  xlab("Fitted Values") +
  ylab("Residuals") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black")
  )

ggsave("figures/lm_2_resid_fitted.jpg", height = 6, width = 8)

# There are a couple of slight curves in the mean, and these data may be
# slightly heteroskedastic. It is difficult to interpret visually with potential
# outliers. As the mean stays close to zero and the overall spread seems to be
# mostly balanced, the model seems to be valid.

# Removing the variables with all coefficients that were not significant seemed
# to slightly improve the model.



# Model 3: Education Interaction ------------------------------------------

# Model 3 adds in an interaction between educational attainment and
# ecological zone urban vs. rural. Educational attainment was flagged as a
# variable to focus on and perhaps there is a interaction effect that
# between region and education that improves the model.

lm_3 <- lm(profit ~ ez_u_r * edu + age + road_dis + chem_fert + insecticide,
  data = base
)

summary(lm_3)


# Coefficient Interpretation:

# Graph all coefficient estimates with 95% confidence interval.

plot_summs(lm_3, scale = TRUE, colors = "black", coefs = c(
  "Rural Savannah" = "ez_u_rRural Savannah",
  "Rural Coastal" = "ez_u_rRural Coastal",
  "Urban" = "ez_u_rOther Urban",
  "Less Than Basic Education" = "eduless_basic",
  "Basic Education" = "edubasic",
  "Higher Than Basic Education" = "edumore_basic",
  "Average Household Age" = "age",
  "Distance to Nearest Motorable Road" = "road_dis",
  "Chemical Fertilizer" = "chem_fert",
  "Insecticide" = "insecticide",
  "Rural Savannah & Less Than Basic Education" = "ez_u_rRural Savannah:eduless_basic",
  "Rural Coastal & Less Than Basic Education" = "ez_u_rRural Coastal:eduless_basic",
  "Urban & Less Than Basic Education" = "ez_u_rOther Urban:eduless_basic",
  "Rural Savannah & Basic Education" = "ez_u_rRural Savannah:edubasic",
  "Rural Coastal & Basic Education" = "ez_u_rRural Coastal:edubasic",
  "Urban & Basic Education" = "ez_u_rOther Urban:edubasic",
  "Rural Savannah & Higher Than Basic Education" = "ez_u_rRural Savannah:edumore_basic",
  "Rural Coastal & Higher Than Basic Education" = "ez_u_rRural Coastal:edumore_basic",
  "Urban & Higher Than Basic Education" = "ez_u_rOther Urban:edumore_basic"
))

ggsave("figures/lm_3_coef.jpg", height = 6, width = 8)

# Interpret significant coefficients.

# ez_u_rRural Savannah: Profit decreases by 283,486 cedis in rural savannah
# areas relative to rural forest areas, significant at at the 5% level.

# eduLess than MSLC/BECE: Profit decreases by 260,240 cedis if the household
# educational attainment is less than basic relative to if the household
# educational attainment is none, significant at the 5% level.

# age: Profit decreases by 4,032 cedis as the median household age increases
# by one year, significant at the 5% level.

# road_dis: Profit increases by 63,611 cedis as the distance to closest
# motorable road increases by one kilometer, significant at less than the
# 0.1% level.

# chem_fertyes: Profit decreases by 145,742 cedis in areas that use chemical
# fertilizers relative to areas that do not use chemical fertilizers,
# significant at the 5% level.

# insecticideyes: Profit increases by 248,279 cedis in areas that use
# insecticide on agriculture relative to areas that do not use insecticide,
# significant at less than the 0.1% level.

# ez_u_rRural Savannah:eduLess than MSLC/BECE: Profit increases by 44,3691 cedis
# in rural savannah areas relative to forest areas where the highest household
# educational attainment is less than basic relative to none, significant at
# the 5% level.

# ez_u_rRural Coastal:eduLess than MSLC/BECE: Profit increases by 95,9419 cedis
# in rural coastal areas relative to forest areas where the highest household
# educational attainment is less than basic relative to none, significant at
# less than the 0.1% level.


# Model Validation:

# The adjusted R-squared value is 0.02481, so the model explains only about
# 2.5% of the variation seen in the response variable. The F-statistic is 5.398
# and statistically significant at less than the 0.1% level.

# Check the normality assumption with the distribution of the standardized
# residuals.

ggplot(lm_3, aes(x = rstandard(lm_3))) +
  geom_histogram(
    bins = 50,
    color = "black",
    fill = "black"
  ) +
  xlab("Standardized Residuals") +
  ylab("Frequency") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black")
  )

ggsave("figures/lm_3_resid_dist.jpg", height = 6, width = 8)

# The distribution is skewed right, so the normality assumption fails. The
# estimates are unbiased but the errors are biased. To resolve this issue, more
# date would be helpful.

# Check the constant variance assumption with a scatter plot of the fitted
# values and residuals.

ggplot(lm_3, aes(x = fitted(lm_3), y = resid(lm_3))) +
  geom_point(color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth() +
  xlab("Fitted Values") +
  ylab("Residuals") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black")
  )

ggsave("figures/lm_3_resid_fitted.jpg", height = 6, width = 8)

# The curves are more pronounced in the mean, especially at the high end of the
# fitted values. These data still have the slightly heteroskedastic issue as
# well. It is difficult to interpret visually with potential outliers. As the
# mean stays close to zero except for the tail, and the overall spread is more
# on the side of balanced than not, the model seems to be valid.

# Adding in educational as an attainment seemed to slightly decrease the model
# validity but improved the model. There are now 8 significant coefficients and
# more of the response variable variation is explained.


# Model 4: Community Size Polynomial --------------------------------------

# Model 4 adds back in community size and community size squared to see if that
# improves the model.

lm_4 <- lm(profit ~ ez_u_r * edu + age + road_dis + chem_fert + insecticide +
  size + I(size^2), data = base)

summary(lm_4)

# Graph profit against size and size squared.

ggplot(base, aes(size, profit)) +
  geom_point() +
  geom_smooth(
    method = "lm", formula = y ~ x, aes(color = "community size"),
    se = FALSE
  ) +
  geom_smooth(
    method = "lm", formula = y ~ x + I(x^2),
    aes(color = "community size squared"), se = FALSE
  ) +
  xlab("Community Size (people)") +
  ylab("Agricultural Profit per Acre (cedis)") +
  scale_color_manual("Linear Model",
    values = c("blue", "red")
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    legend.key = element_rect(
      fill = "transparent",
      colour = "transparent"
    ),
    legend.position = c(0.2, 0.9)
  )

ggsave("figures/lm_4_proft_size.jpg", height = 6, width = 8)

# Determine turning point of squared model.
# x* = |Bhat1 / (2 * Bhat2)|
turning_point <- abs(coef(lm_4)["size"] /
  (2 * coef(lm_4)["I(size^2)"]))

turning_point

# Percentage of agricultural profit above turning point.

turning_point_percent <- base %>%
  group_by(size > turning_point) %>%
  summarize(total = n()) %>%
  mutate(percent = round(total / sum(total) * 100, 3))

turning_point_percent

# Coefficient Interpretation:

# Graph all coefficient estimates with 95% confidence interval.

plot_summs(lm_4, scale = TRUE, colors = "black", coefs = c(
  "Rural Savannah" = "ez_u_rRural Savannah",
  "Rural Coastal" = "ez_u_rRural Coastal",
  "Urban" = "ez_u_rOther Urban",
  "Less Than Basic Education" = "eduless_basic",
  "Basic Education" = "edubasic",
  "Higher Than Basic Education" = "edumore_basic",
  "Average Household Age" = "age",
  "Distance to Nearest Motorable Road" = "road_dis",
  "Chemical Fertilizer" = "chem_fert",
  "Insecticide" = "insecticide",
  "Community Size" = "size",
  "Community Size Squared" = "I(size^2)",
  "Rural Savannah & Less Than Basic Education" = "ez_u_rRural Savannah:eduless_basic",
  "Rural Coastal & Less Than Basic Education" = "ez_u_rRural Coastal:eduless_basic",
  "Urban & Less Than Basic Education" = "ez_u_rOther Urban:eduless_basic",
  "Rural Savannah & Basic Education" = "ez_u_rRural Savannah:edubasic",
  "Rural Coastal & Basic Education" = "ez_u_rRural Coastal:edubasic",
  "Urban & Basic Education" = "ez_u_rOther Urban:edubasic",
  "Rural Savannah & Higher Than Basic Education" = "ez_u_rRural Savannah:edumore_basic",
  "Rural Coastal & Higher Than Basic Education" = "ez_u_rRural Coastal:edumore_basic",
  "Urban & Higher Than Basic Education" = "ez_u_rOther Urban:edumore_basic"
))

ggsave("figures/lm_4_coef.jpg", height = 6, width = 8)

# Interpret significant coefficients.

# ez_u_rRural Savannah: Profit decreases by 293,100 cedis in rural savannah
# areas relative to rural forest areas, significant at at the 5% level.

# eduLess than MSLC/BECE: Profit decreases by 261,000 cedis if the household
# educational attainment is less than basic relative to if the household
# educational attainment is none, significant at the 5% level.

# age: Profit decreases by 3,852 cedis as the median household age increases
# by one year, significant at the 5% level.

# road_dis: Profit increases by 60,950 cedis as the distance to closest
# motorable road increases by one kilometer, significant at less than the
# 0.1% level.

# chem_fertyes: Profit decreases by 141,000 cedis in areas that use chemical
# fertilizers relative to areas that do not use chemical fertilizers,
# significant at the 5% level.

# insecticideyes: Profit increases by 255,100 cedis in areas that use
# insecticide on agriculture relative to areas that do not use insecticide,
# significant at less than the 0.1% level.

# size and I(size^2): The coefficient for size is -132.9 with significance at
# the 10% level and the coefficient for size squared is 226.4 with significance
# at the 10% level. As the community size coefficient is negative and the
# community size squared coefficient is positive, the relationship with profit
# follows an upwards concave curve. Holding all other variables constant, as
# community size increases the profit decreases with diminishing returns until
# community size reaches ~2,933 people and then the profit starts to increase.
# Most households (94.6%) are before the turning point.

# ez_u_rRural Savannah:eduLess than MSLC/BECE: Profit increases by 45,660 cedis
# in rural savannah areas relative to forest areas where the highest household
# educational attainment is less than basic relative to none, significant at
# the 5% level.

# ez_u_rRural Coastal:eduLess than MSLC/BECE: Profit increases by 95,040 cedis
# in rural coastal areas relative to forest areas where the highest household
# educational attainment is less than basic relative to none, significant at
# less than the 0.1% level.


# Model Validation:

# The adjusted R-squared value is 0.02535, so the model explains only about
# 2.5% of the variation seen in the response variable. The F-statistic is 5.068
# and statistically significant at less than the 0.1% level.

# Check the normality assumption with the distribution of the standardized
# residuals.

ggplot(lm_4, aes(x = rstandard(lm_4))) +
  geom_histogram(
    bins = 50,
    color = "black",
    fill = "black"
  ) +
  xlab("Standardized Residuals") +
  ylab("Frequency") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black")
  )

ggsave("figures/lm_4_resid_dist.jpg", height = 6, width = 8)

# The distribution is skewed right, so the normality assumption fails. The
# estimates are unbiased but the errors are biased. To resolve this issue, more
# date would be helpful.

# Check the constant variance assumption with a scatter plot of the fitted
# values and residuals.

ggplot(lm_4, aes(x = fitted(lm_4), y = resid(lm_4))) +
  geom_point(color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth() +
  xlab("Fitted Values") +
  ylab("Residuals") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black")
  )

ggsave("figures/lm_4_resid_fitted.jpg", height = 6, width = 8)

# There is still a lift in the mean at the high end of the fitted values but
# the curves seem to be slightly less pronounced in the mean, These data still
# might be slightly heteroskedastic. It is difficult to interpret visually with
# potential outliers. As the mean stays close to zero except for the tail, and
# the overall spread is more on the side of balanced than not, the model seems
# to be valid.

# Adding in educational as an attainment seemed to slightly improved the model
# validity and the model. There are now 10 significant coefficients and
# more of the response variable variation is explained. It is also plausible
# that as the community size increases the agricultural profit decreases,
# perhaps land is more expensive, cost of living is higher, etc. And either
# the 5.45% of communities past the turning point are outliers, or there is
# some economic factor,large communal support, farming resources, etc. where
# agricultural profit per acre increase in larger communities.

# I think ecological zone urban vs. rural and educational attainment are the
# most interesting results. Creating a graph for memo.

memo_plot <- plot_summs(
  "Model 4" = lm_4, scale = TRUE, colors = "black", coefs = c(
    "Rural Savannah" = "ez_u_rRural Savannah",
    "Rural Coastal" = "ez_u_rRural Coastal",
    "Urban" = "ez_u_rOther Urban",
    "Less Than Basic Education" = "eduless_basic",
    "Basic Education" = "edubasicE",
    "Higher Than Basic Education" = "edumore_basic",
    "Rural Savannah & Less Than Basic Education" = "ez_u_rRural Savannah:eduless_basic",
    "Rural Coastal & Less Than Basic Education" = "ez_u_rRural Coastal:eduless_basic",
    "Urban & Less Than Basic Education" = "ez_u_rOther Urban:eduless_basic",
    "Rural Savannah & Basic Education" = "ez_u_rRural Savannah:edubasic",
    "Rural Coastal & Basic Education" = "ez_u_rRural Coastal:edubasic",
    "Urban & Basic Education" = "ez_u_rOther Urban:edubasic",
    "Rural Savannah & Higher Than Basic Education" = "ez_u_rRural Savannah:edumore_basic",
    "Rural Coastal & Higher Than Basic Education" = "ez_u_rRural Coastal:edumore_basic",
    "Urban & Higher Than Basic Education" = "ez_u_rOther Urban:edumore_basic"
  )
)

memo_plot + labs(x = "\n Effect on Agricultural Profit (cedis) \n ") + scale_x_continuous(labels = scales::comma)

ggsave("figures/lm_4_coef_memo.jpg", height = 6, width = 8)


# Model 5: Log Median Household Age ---------------------------------------

# Model 5 takes the log of median household age to see if that improves the
# model by using a different functional form and age is the only other
# continuous variable to use except for community size.

lm_5 <- lm(profit ~ ez_u_r * edu + log(age) + road_dis + chem_fert + insecticide +
  size + I(size^2), data = base)

summary(lm_5)


# Coefficient Interpretation:

# Graph all coefficient estimates with 95% confidence interval.

plot_summs(lm_5, scale = TRUE, colors = "black", coefs = c(
  "Rural Savannah" = "ez_u_rRural Savannah",
  "Rural Coastal" = "ez_u_rRural Coastal",
  "Urban" = "ez_u_rOther Urban",
  "Less Than Basic Education" = "eduless_basic",
  "Basic Education" = "edubasic",
  "Higher Than Basic Education" = "edumore_basic",
  "Log of Average Household Age" = "log(age)",
  "Distance to Nearest Motorable Road" = "road_dis",
  "Chemical Fertilizer" = "chem_fert",
  "Insecticide" = "insecticide",
  "Community Size" = "size",
  "Community Size Squared" = "I(size^2)",
  "Rural Savannah & Less Than Basic Education" = "ez_u_rRural Savannah:eduless_basic",
  "Rural Coastal & Less Than Basic Education" = "ez_u_rRural Coastal:eduless_basic",
  "Urban & Less Than Basic Education" = "ez_u_rOther Urban:eduless_basic",
  "Rural Savannah & Basic Education" = "ez_u_rRural Savannah:edubasic",
  "Rural Coastal & Basic Education" = "ez_u_rRural Coastal:edubasic",
  "Urban & Basic Education" = "ez_u_rOther Urban:edubasic",
  "Rural Savannah & Higher Than Basic Education" = "ez_u_rRural Savannah:edumore_basic",
  "Rural Coastal & Higher Than Basic Education" = "ez_u_rRural Coastal:edumore_basic",
  "Urban & Higher Than Basic Education" = "ez_u_rOther Urban:edumore_basic"
))

ggsave("figures/lm_5_coef.jpg", height = 6, width = 8)

# Interpret significant coefficients.

# ez_u_rRural Savannah: Profit decreases by 278,900 cedis in rural savannah
# areas relative to rural forest areas, significant at at the 5% level.

# eduLess than MSLC/BECE: Profit decreases by 245,200 cedis if the household
# educational attainment is less than basic relative to if the household
# educational attainment is none, significant at the 10% level.

# age: Profit decreases by 88,980 cedis as the median household age increases
# by one percentage point, significant at the 5% level.

# road_dis: Profit increases by 60,810 cedis as the distance to closest
# motorable road increases by one kilometer, significant at less than the
# 0.1% level.

# chem_fertyes: Profit decreases by 140,900 cedis in areas that use chemical
# fertilizers relative to areas that do not use chemical fertilizers,
# significant at the 5% level.

# insecticideyes: Profit increases by 255,800 cedis in areas that use
# insecticide on agriculture relative to areas that do not use insecticide,
# significant at less than the 0.1% level.

# size and I(size^2): The coefficient for size is -136.6 with significance at
# the 5% level and the coefficient for size squared is 232.7 with significance
# at the 10% level. As the community size coefficient is negative and the
# community size squared coefficient is positive, the relationship with profit
# follows an upwards concave curve. Holding all other variables constant, as
# community size increases the profit decreases with diminishing returns until
# community size reaches ~2,933 people and then the profit starts to increase.
# Most households (94.6%) are before the turning point.

# ez_u_rRural Savannah:eduLess than MSLC/BECE: Profit increases by 448,400 cedis
# in rural savannah areas relative to forest areas where the highest household
# educational attainment is less than basic relative to none, significant at
# the 5% level.

# ez_u_rRural Coastal:eduLess than MSLC/BECE: Profit increases by 958,800 cedis
# in rural coastal areas relative to forest areas where the highest household
# educational attainment is less than basic relative to none, significant at
# less than the 0.1% level.


# Model Validation:

# The adjusted R-squared value is 0.02515, so the model explains only about
# 2.5% of the variation seen in the response variable. The F-statistic is 5.035
# and statistically significant at less than the 0.1% level.

# Check the normality assumption with the distribution of the standardized
# residuals.

ggplot(lm_5, aes(x = rstandard(lm_5))) +
  geom_histogram(
    bins = 50,
    color = "black",
    fill = "black"
  ) +
  xlab("Standardized Residuals") +
  ylab("Frequency") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black")
  )

ggsave("figures/lm_5_resid_dist.jpg", height = 6, width = 8)

# The distribution is skewed right, so the normality assumption fails. The
# estimates are unbiased but the errors are biased. To resolve this issue, more
# date would be helpful.

# Check the constant variance assumption with a scatter plot of the fitted
# values and residuals.

ggplot(lm_5, aes(x = fitted(lm_5), y = resid(lm_5))) +
  geom_point(color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth() +
  xlab("Fitted Values") +
  ylab("Residuals") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black")
  )

ggsave("figures/lm_5_resid_fitted.jpg", height = 6, width = 8)

# There seems to be no change in the constant variance between Model 4 and
# Model 5, however Model 4 makes the model slightly worse with the F-statistic
# p-value and adjusted R-squared. It also shifts significance from eduLess than
# MSLC/BECE to size squared. Based on this rationale, log of median household
# age will be left out of the model.


# Model 6: Outlier Sensitivity --------------------------------------------

# Out of concern that outliers are influencing the model, dropping the top and
# bottom 10 values to see how that changes the model.

# Remove top ten and top bottom rows.

outlier_base <- base %>%
  arrange(profit) %>%
  head(n = 3276)

outlier_base <- outlier_base %>%
  arrange(profit) %>%
  tail(n = 3266)

# Check how that might have changed the profit distribution.
# Summarize profit.

summary(outlier_base$profit)

# Graph profit distribution.

ggplot(data = outlier_base, mapping = aes(x = profit)) +
  geom_histogram(fill = "black", bins = 100) +
  xlab("Agricultural Profit per Acre (cedis)") +
  ylab("Household Count") +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal()

# Save agricultural profit per acre distribution graph.

ggsave("figures/ds2_profit_dist.jpg", height = 6, width = 8)

# The distribution is still skewed right but there are less extreme values.

# Run the best model so far (Model 4: Community Size Polynomial) with the data
# that has top and bottom 10 agricultural profit observations removed.

lm_6 <- lm(profit ~ ez_u_r * edu + age + road_dis + chem_fert + insecticide +
  size + I(size^2), data = outlier_base)

summary(lm_6)

# Graph profit against size and size squared as underlying data has changed.

ggplot(outlier_base, aes(size, profit)) +
  geom_point() +
  geom_smooth(
    method = "lm", formula = y ~ x, aes(color = "community size"),
    se = FALSE
  ) +
  geom_smooth(
    method = "lm", formula = y ~ x + I(x^2),
    aes(color = "community size squared"), se = FALSE
  ) +
  xlab("Community Size (people)") +
  ylab("Agricultural Profit per Acre (cedis)") +
  scale_color_manual("Linear Model",
    values = c("blue", "red")
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    legend.key = element_rect(
      fill = "transparent",
      colour = "transparent"
    ),
    legend.position = c(0.2, 0.9)
  )

ggsave("figures/lm_6_proft_size.jpg", height = 6, width = 8)

# Determine turning point of squared model.
# x* = |Bhat1 / (2 * Bhat2)|
turning_point <- abs(coef(lm_6)["size"] /
  (2 * coef(lm_4)["I(size^2)"]))

turning_point

# Percentage of agricultural profit above turning point.

turning_point_percent <- base %>%
  group_by(size > turning_point) %>%
  summarize(total = n()) %>%
  mutate(percent = round(total / sum(total) * 100, 3))

turning_point_percent

# Coefficient Interpretation:

# Graph all coefficient estimates with 95% confidence interval.

plot_summs("Model 4" = lm_4, "Model 6" = lm_6, scale = TRUE, coefs = c(
  "Rural Savannah" = "ez_u_rRural Savannah",
  "Rural Coastal" = "ez_u_rRural Coastal",
  "Urban" = "ez_u_rOther Urban",
  "Less Than Basic Education" = "eduLess than MSLC/BECE",
  "Basic Education" = "eduMSLC/BECE",
  "Higher Than Basic Education" = "eduSecondary or higher",
  "Average Household Age" = "age",
  "Distance to Motorable Road" = "road_dis",
  "Use of Chemical Fertilizer" = "chem_fertyes",
  "Use of Insecticides" = "insecticideyes",
  "Community Size" = "size",
  "Community Size Squared" = "I(size^2)",
  "Rural Savannah & Less Than Basic Education" = "ez_u_rRural Savannah:eduLess than MSLC/BECE",
  "Rural Coastal & Less Than Basic Education" = "ez_u_rRural Coastal:eduLess than MSLC/BECE0",
  "Urban & Less Than Basic Education" = "ez_u_rOther Urban:eduLess than MSLC/BECE",
  "Rural Savannah & Basic Education" = "ez_u_rRural Savannah:eduMSLC/BECE",
  "Rural Coastal & Basic Education" = "ez_u_rRural Coastal:eduMSLC/BECE ",
  "Urban & Basic Education" = "ez_u_rOther Urban:eduMSLC/BECE",
  "Rural Savannah & Higher Than Basic Education" = "ez_u_rRural Savannah:eduSecondary or higher",
  "Rural Coastal & Higher Than Basic Education" = "ez_u_rRural Coastal:eduSecondary or higher",
  "Urban & Higher Than Basic Education" = "ez_u_rOther Urban:eduSecondary or higher "
))

ggsave("figures/lm_6_coef.jpg", height = 6, width = 8)

# Interpret significant coefficients.

# ez_u_rRural Savannah: Profit decreases by 202,500 cedis in rural savannah
# areas relative to rural forest areas, significant at at the 1% level.

# age: Profit decreases by 2,433 cedis as the median household age increases
# by one year, significant at the 5% level.

# road_dis: Profit increases by 28,660 cedis as the distance to closest
# motorable road increases by one kilometer, significant at less than the
# 0.1% level.

# chem_fertyes: Profit decreases by 171,300 cedis in areas that use chemical
# fertilizers relative to areas that do not use chemical fertilizers,
# significant at the 5% level.

# size and I(size^2): The coefficient for size is -79.06 with significance at
# the 5% level and the coefficient for size squared is 0.0166 with significance
# at the 5% level. As the community size coefficient is negative and the
# community size squared coefficient is positive, the relationship with profit
# follows an upwards concave curve. Holding all other variables constant, as
# community size increases the profit decreases with diminishing returns until
# community size reaches ~1,746 people and then the profit starts to increase.
# Most households (88%) are before the turning point, however this is less than
# Model 4.

# ez_u_rRural Savannah:eduLess than MSLC/BECE: Profit increases by 357,500 cedis
# in rural savannah areas relative to forest areas where the highest household
# educational attainment is less than basic relative to none, significant at
# the 1% level.

# ez_u_rRural Coastal:eduLess than MSLC/BECE: Profit increases by 211,110 cedis
# in rural coastal areas relative to forest areas where the highest household
# educational attainment is less than basic relative to none, significant at
# the 10% level.

# ez_u_rRural Coastal:eduSecondary or higher: Profit increases by 375,600 cedis
# in rural coastal areas relative to forest areas where the highest household
# educational attainment is less than basic relative to none, significant at
# the 5% level.


# Model Validation:

# The adjusted R-squared value is 0.02202, so the model explains only about
# 2.2% of the variation seen in the response variable. The F-statistic is 4.5
# and statistically significant at less than the 0.1% level.

# Check the normality assumption with the distribution of the standardized
# residuals.

ggplot(lm_6, aes(x = rstandard(lm_6))) +
  geom_histogram(
    bins = 50,
    color = "black",
    fill = "black"
  ) +
  xlab("Standardized Residuals") +
  ylab("Frequency") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black")
  )

ggsave("figures/lm_6_resid_dist.jpg", height = 6, width = 8)

# The distribution is skewed right, so the normality assumption fails. The
# estimates are unbiased but the errors are biased. To resolve this issue, more
# date would be helpful. The skewness is not as extreme as Model 4.

# Check the constant variance assumption with a scatter plot of the fitted
# values and residuals.

ggplot(lm_6, aes(x = fitted(lm_6), y = resid(lm_6))) +
  geom_point(color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth() +
  xlab("Fitted Values") +
  ylab("Residuals") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black")
  )

ggsave("figures/lm_6_resid_fitted.jpg", height = 6, width = 8)

# The mean is stable around zero for all observations and the spread seems to
# be balanced. Compared to the residuals vs. fitted values graphed for Model 4,
# Model 6 has better constant variance. The model is valid.

# The model seems to be sensitive to outliers, so reporting results of best
# model with and without the top and bottom ten profits.

# TODO: Investigate if any variables dropped from the original model are now
# significant. Further investigate outliers and how best to handle.






