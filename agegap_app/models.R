library(lme4)
library(tidyverse)
library(haven)
library(broom)

PATH="C:/R/age_gap/agegap_app"
setwd(PATH)

source("prepare_data.R")

data <- data %>% 
  mutate(agediff5=as_factor(agediff5) %>% relevel(ref="5-9"))
data_fevtreat <- data_fevtreat %>% 
  mutate(agediff5=as_factor(agediff5) %>% relevel(ref="5-9"))
data_measles <- data_measles %>% 
  mutate(agediff5=as_factor(agediff5) %>% relevel(ref="5-9"))
data_underweight <- data_underweight %>% 
  mutate(agediff5=as_factor(agediff5) %>% relevel(ref="5-9"))

##################### only age gap models #########################
## fever treatment
fevtreat_model <- glmer(fevtreat~agediff5+
                          (1|respondent)+(1|country), data=data_fevtreat, nAGQ=0,
                        family=binomial(link="logit"), weights=perweight)
# fevtreat_icc <- performance::icc(fevtreat_model,by_group=TRUE)

## measles vaccination
measles_model <- glmer(measles~agediff5+
                         (1|respondent)+(1|country), nAGQ=0, data=data_measles,
                       family=binomial(link="logit"), weights=perweight)
# measles_icc <- performance::icc(measles_model,by_group=TRUE)

## underweight
underweight_model <- glmer(underweight~agediff5+
                             (1|respondent)+(1|country), data=data_underweight, nAGQ=0,
                           family=binomial(link="logit"), weights=perweight)
# underweight_icc <- performance::icc(underweight_model,by_group=TRUE)

## save as odds ratios
or <- left_join(as_tibble(odds_ratios(fevtreat_model),rownames="term"),
                as_tibble(odds_ratios(measles_model),rownames="term"), 
                by="term", suffix=c("fevtreat","measles")) %>% 
  left_join(as_tibble(odds_ratios(underweight_model),rownames="term") %>% 
              rename("Estunderweight"="Est", "LLunderweight"="LL",
                     "ULunderweight"="UL"), by="term") %>% 
  mutate_at(vars(-term), round,2) %>% 
  mutate(ci_fevtreat=paste(Estfevtreat, " ", "(", LLfevtreat, "-", ULfevtreat, ")",sep=""),
         ci_measles=paste(Estmeasles, " ", "(", LLmeasles, "-", ULmeasles, ")",sep=""),
         ci_underweight=paste(Estunderweight, " ", "(", LLunderweight, "-", ULunderweight, ")",sep="")) %>% 
  select(term,ci_fevtreat,ci_measles,ci_underweight)

# icc <- left_join(as_tibble(as_tibble(fevtreat_icc)),
#           as_tibble(as_tibble(measles_icc)), 
#           by="Group", suffix=c("_fevtreat","_measles")) %>% 
#   left_join(as_tibble(as_tibble(underweight_icc)) %>% 
#               rename("ICC_underweight"="ICC"), by="Group") %>% 
#   mutate_at(vars(-Group),round,3)

#################### children models #########################
## fever treatment
fevtreat_model <- glmer(fevtreat~agediff5+kidsex+kidcurage+kidbord+
                          (1|respondent)+(1|country), data=data_fevtreat, nAGQ=0,
                        family=binomial(link="logit"), weights=perweight)
# fevtreat_icc <- performance::icc(fevtreat_model,by_group=TRUE)

## measeles vaccination
measles_model <- glmer(measles~agediff5+kidsex+kidcurage+kidbord+
                         (1|respondent)+(1|country), nAGQ=0,
                       data=data_measles,
                       family=binomial(link="logit"), weights=perweight)
# measles_icc <- performance::icc(measles_model,by_group=TRUE)


## underweight
underweight_model <- glmer(underweight~agediff5+kidsex+kidcurage+kidbord+
                             (1|respondent)+(1|country), data=data_underweight, nAGQ=0,
                           family=binomial(link="logit"), weights=perweight)
# underweight_icc <- performance::icc(underweight_model,by_group=TRUE)

## save as odds ratios
or <- left_join(as_tibble(odds_ratios(fevtreat_model),rownames="term"),
                as_tibble(odds_ratios(measles_model),rownames="term"), 
                by="term", suffix=c("fevtreat","measles")) %>% 
  left_join(as_tibble(odds_ratios(underweight_model),rownames="term") %>% 
              rename("Estunderweight"="Est", "LLunderweight"="LL",
                     "ULunderweight"="UL"), by="term") %>% 
  mutate_at(vars(-term), round,2) %>% 
  mutate(ci_fevtreat=paste(Estfevtreat, " ", "(", LLfevtreat, "-", ULfevtreat, ")",sep=""),
         ci_measles=paste(Estmeasles, " ", "(", LLmeasles, "-", ULmeasles, ")",sep=""),
         ci_underweight=paste(Estunderweight, " ", "(", LLunderweight, "-", ULunderweight, ")",sep="")) %>% 
  select(term,ci_fevtreat,ci_measles,ci_underweight)

# icc <- left_join(as_tibble(as_tibble(fevtreat_icc)),
#                  as_tibble(as_tibble(measles_icc)), 
#                  by="Group", suffix=c("_fevtreat","_measles")) %>% 
#   left_join(as_tibble(as_tibble(underweight_icc)) %>% 
#               rename("ICC_underweight"="ICC"), by="Group") %>% 
#   mutate_at(vars(-Group),round,3)


##################### main models ##########################
## fever treatment
fevtreat_model <- glmer(fevtreat~agediff5+kidsex+kidcurage+kidbord+poly+
                          mage+educlvl+husedlvl+wealthq+urban+
                          (1|respondent)+(1|country), data=data_fevtreat, nAGQ=0,
                        family=binomial(link="logit"), weights=perweight)
# fevtreat_icc <- performance::icc(fevtreat_model,by_group=TRUE)
# fevtreat_res <- ggcoefstats(fevtreat_model,
#             exclude.intercept=T,stats.labels=F)

## measeles vaccination
measles_model <- glmer(measles~agediff5+kidsex+kidcurage+kidbord+poly+
                         mage+educlvl+husedlvl+wealthq+urban+
                         (1|respondent)+(1|country), nAGQ=0,
                       data=data_measles,
                       family=binomial(link="logit"), weights=perweight)
# measles_icc <- performance::icc(measles_model,by_group=TRUE)
# measles_res <- ggcoefstats(measles_model,
#             exclude.intercept=T,stats.labels=F)

## underweight
underweight_model <- glmer(underweight~agediff5+kidsex+kidcurage+kidbord+poly+
                             mage+educlvl+husedlvl+wealthq+urban+
                             (1|respondent)+(1|country), data=data_underweight, nAGQ=0,
                           family=binomial(link="logit"), weights=perweight)
# underweight_icc <- performance::icc(underweight_model,by_group=TRUE)
# underweight_res <- ggcoefstats(underweight_model,
#             exclude.intercept=T,stats.labels=F)

## save as odds ratios
or <- left_join(as_tibble(odds_ratios(fevtreat_model),rownames="term"),
                as_tibble(odds_ratios(measles_model),rownames="term"), 
                by="term", suffix=c("fevtreat","measles")) %>% 
  left_join(as_tibble(odds_ratios(underweight_model),rownames="term") %>% 
              rename("Estunderweight"="Est", "LLunderweight"="LL",
                     "ULunderweight"="UL"), by="term") %>% 
  mutate_at(vars(-term), round,2) %>% 
  mutate(ci_fevtreat=paste(Estfevtreat, " ", "(", LLfevtreat, "-", ULfevtreat, ")",sep=""),
         ci_measles=paste(Estmeasles, " ", "(", LLmeasles, "-", ULmeasles, ")",sep=""),
         ci_underweight=paste(Estunderweight, " ", "(", LLunderweight, "-", ULunderweight, ")",sep="")) %>% 
  select(term,ci_fevtreat,ci_measles,ci_underweight)

# icc <- left_join(as_tibble(as_tibble(fevtreat_icc)),
#                  as_tibble(as_tibble(measles_icc)), 
#                  by="Group", suffix=c("_fevtreat","_measles")) %>% 
#   left_join(as_tibble(as_tibble(underweight_icc)) %>% 
#               rename("ICC_underweight"="ICC"), by="Group") %>% 
#   mutate_at(vars(-Group),round,3)
