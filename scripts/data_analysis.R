rm(list = ls())

library(ggstatsplot)
library(lme4)

source("scripts/data_preparation.R")

data <- load("data/analysis_data.Rdta")

###################### descriptive #########################
data %>% gghistostats(agediff)
data %>% ggscatterstats(agediff:wifenum)
data %>% ggcorrmat(agediff:underweight)

##################### agediff models ######################
summary(fevtreat_agediff <- glmer(fevtreat~agediff5+(1|respondent)+(1|country), 
                                  data=data, weights=perweight,
                                  family=binomial(link="logit")))


summary(measles_agediff <- glmer(measles~agediff5+(1|respondent)+(1|country), 
                                 data=data %>% filter(kidcurage!="less than 1 year"), 
                                 weights=perweight,
                                 family=binomial(link="logit")))

summary(underweight_agediff <- glmer(underweight~agediff5+(1|respondent)+(1|country), 
                                     data=data, weights=perweight,
                                     family=binomial(link="logit")))

##################### full models ##########################
summary(fevtreat_model <- glmer(fevtreat~agediff5+kidsex+kidcurage+kidbord+poly+
                                  mage+educlvl+husedlvl+wealthq+urban+
                                  (1|respondent)+(1|country), data=data,
                                family=binomial(link="logit")))

summary(measles_model <- glmer(measles~agediff5+kidsex+kidcurage+kidbord+poly+
                                 mage+educlvl+husedlvl+wealthq+urban+
                                 (1|respondent)+(1|country), 
                               data=data %>% filter(kidcurage!="less than 1 year"),
                               family=binomial(link="logit")))

summary(underweight_model <- glmer(underweight~agediff5+kidsex+kidcurage+kidbord+poly+
                                     mage+educlvl+husedlvl+wealthq+urban+
                                     (1|respondent)+(1|country), data=data,
                                   family=binomial(link="logit")))