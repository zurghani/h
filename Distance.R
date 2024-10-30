library(stringr)
library(tidyverse)
library("pacman")
pacman::p_load(tidyverse, broom, emmeans,lme4, lmerTest,
               afex, MASS, pscl, knitr, psycho, ordinal,
               tidytext, xtable)

# This step reads in the data file and assigns a name to it. I chose datafamily,
# datapolice, and datagovern randomly
datafamily <- read_csv('FamilyDistance.csv')
datagovern <- read_csv('GovernmentDistance.csv')
datapolice <- read_csv('PoliceDistance.csv')


# convert the data frame to tibble
datafamily <- as_tibble(datafamily)
datagovern <- as_tibble(datagovern)
datapolice <- as_tibble(datapolice)
# to see the tibble:
datafamily
datagovern
datapolice
# to see the number of rows and number of columns respectively:
nrow(datafamily)
ncol(datafamily)

# to see the levels of the vector Concord for example:
levels(datafamily$Concord)
levels(datagovern$Concord)
levels(datapolice$Concord)


# The above command comes back NULL because it is a character vector. Below I 
# convert it to a factor vector:
datafamily$Concord <- as.factor(datafamily$Concord)
levels(datafamily$Concord)

datagovern$Concord <- as.factor(datagovern$Concord)
levels(datagovern$Concord)

datapolice$Concord <- as.factor(datapolice$Concord)
levels(datapolice$Concord)


# to confirm the class of the vector Concord:
class(datafamily$Concord)

class(datagovern$Concord)

class(datapolice$Concord)

# this step is the actual statistical model
mdlfamily <- glm(Concord ~ Distance, data = datafamily, family = 'binomial')
summary(mdlfamily)

mdlgovern <- glm(Concord ~ Distance, data = datagovern, family = 'binomial')
summary(mdlgovern)

mdlpolice <- glm(Concord ~ Distance, data = datapolice, family = 'binomial')
summary(mdlpolice)

tidy(mdlfamily)
tidy(mdlgovern)
tidy(mdlpolice)
# 
kable(summary(mdlfamily)$coef, digits=4)
tidy(mdlfamily) %>%
  mutate(estimate = round(estimate, 2),
         std.error = round(std.error, 2),
         statistic = round(statistic, 2),
         p.value = format.pval(p.value, 4)) %>% print(n = 45)

# calculating predicted probabilities for singular pronominal for family

family_preds <-- tibble(Distance = 1:10)
predict(mdlfamily, family_preds)
plogis(predict(mdlfamily, family_preds))
# another way instead of the plogis function is the following:
# predict(mdlfamily, family_preds, type = 'response')

# compute the 95% confidence interval for plotting (Winter, 2020: 215)
family_preds <- as_tibble(predict(mdlfamily,
                                  family_preds,
                                  se.fit = TRUE) [1:2]) %>% 
  mutate(prob = plogis(fit),
         LB = plogis(fit - 1.96 * se.fit),
         UB = plogis(fit + 1.96 * se.fit)) %>% 
  bind_cols(family_preds)
family_preds

# plotting predicted probabilities around each fitted value
# using geom_errorbar()
family_preds %>% ggplot(aes(x = Distance, y = prob)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = LB, ymax = UB), width = 0.5) +
  scale_x_continuous(breaks = 1:10) +
  xlab('Distance') +
  ylab('p(y = singular)') +
  theme_minimal()

# calculating predicted probabilities for singular pronominal for government

govern_preds <-- tibble(Distance = 1:10)
predict(mdlgovern, govern_preds)
plogis(predict(mdlgovern, govern_preds))
# another way instead of the plogis function is the following:
# predict(mdlgovern, govern_preds, type = 'response')

# compute the 95% confidence interval for plotting (Winter, 2020: 215)
govern_preds <- as_tibble(predict(mdlgovern,
                                  govern_preds,
                                  se.fit = TRUE) [1:2]) %>% 
  mutate(prob = plogis(fit),
         LB = plogis(fit - 1.96 * se.fit),
         UB = plogis(fit + 1.96 * se.fit)) %>% 
  bind_cols(govern_preds)
govern_preds

# plotting predicted probabilities around each fitted value
# using geom_errorbar()
govern_preds %>% ggplot(aes(x = Distance, y = prob)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = LB, ymax = UB), width = 0.5) +
  scale_x_continuous(breaks = 1:10) +
  xlab('Distance') +
  ylab('p(y = singular)') +
  theme_minimal()


# calculating predicted probabilities for singular pronominal for police

police_preds <-- tibble(Distance = 1:10)
predict(mdlpolice, police_preds)
plogis(predict(mdlpolice, police_preds))
# another way instead of the plogis function is the following:
# predict(mdlpolice, police_preds, type = 'response')

# compute the 95% confidence interval for plotting (Winter, 2020: 215)
police_preds <- as_tibble(predict(mdlpolice,
                                  police_preds,
                                  se.fit = TRUE) [1:2]) %>% 
  mutate(prob = plogis(fit),
         LB = plogis(fit - 1.96 * se.fit),
         UB = plogis(fit + 1.96 * se.fit)) %>% 
  bind_cols(police_preds)
police_preds

# plotting predicted probabilities around each fitted value
# using geom_errorbar()
police_preds %>% ggplot(aes(x = Distance, y = prob)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = LB, ymax = UB), width = 0.5) +
  scale_x_continuous(breaks = 1:10) +
  xlab('Distance') +
  ylab('p(y = singular)') +
  theme_minimal()
citation("lme4")
