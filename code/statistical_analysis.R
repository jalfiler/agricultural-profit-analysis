# Objective: building model for Statistical Analysis report. 

# This part will concentrate on the model analysis in order to learn more about 
# how agricultural earnings are calculated in Ghana. We focused our research on 
# how variables obtained during the GLSS 4 survey affect agricultural profit in Ghana.

library(tidyverse)
library(haven)
library(corrplot)

base <- readRDS("data/base.RData")


# Prepare Data ------------------------------------------------------------

# Initial model will use profit_by_acre as the response variable. The 
# explanatory variables are edu (categorical), ez (categorical), and 
# community size (continuous).

base <- base %>%
  select(clust,
         ez,
         edu,
         size,
         school_dis = p_school_dis,
         profit = profit_by_acre,
         age_group = age,
         r_distance =  road_dis,
         c_fertilizer = chem_fert,
         insect = insecticide,
         irri = irrigation
         )

summary(base)


# first regression
# dependent variable: profit
# explanatory variables: ez + size + school_dis + age_group + r_distance + c_fertilizer + insect + irri
model <- lm(profit ~ ez + size + school_dis + age_group + r_distance + c_fertilizer + insect + irri, data = base)
summary(model)

plot(model)

ggplot(data = base, aes(x = model$residuals)) +
  geom_histogram(bins = 200, fill = 'steelblue', color = 'black') +
  labs(title = 'First Regression', x = 'Residuals', y = 'Frequency')


# second regression
# dependent variable: profit
# Explanatory variable: age_group + r_distance + c_fertilizer + insect
model1 <- lm(profit ~ age_group + r_distance + c_fertilizer + insect, data = base)
summary(model1)

plot(model1)


ggplot(model1, aes(x = fitted(model1), y = resid(model1))) + 
  geom_point(color = 'black') + 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_smooth() + 
  xlab("Fitted Values") +
  ylab("Residuals") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color ='black')
  )

#CONCLUSION: adjusted R-squared is 1.12% for all four variables with a p-value of 0.
# There is a new outlier value, however the mean and spread seems to be about the same.
# Histogram is also skewed right. 


# third regression
# dependent variable: profit
# Explanatory variable: r_distance
model2 <- lm(profit ~ I(r_distance^2), data = base)
summary(model2)

plot(model2)

ggplot(data = base, aes(x = model2$residuals)) +
  geom_histogram(bins = 100, fill = 'steelblue', color = 'black') +
  labs(title = 'Third Regression', x = 'Residuals', y = 'Frequency')



# fourth regression
# dependent variable: profit
# Explanatory variable: (log) r_distance
model3 <- lm(profit ~ I(r_distance^2), data = base)
summary(model3)


model4 <- lm(profit ~ I(r_distance^2) + size, data = base)
summary(model4)

ggplot(data = base, aes(x = model4$residuals)) +
  geom_histogram(bins = 100, fill = 'steelblue', color = 'black') +
  labs(title = 'Third Regression', x = 'Residuals', y = 'Frequency')
         