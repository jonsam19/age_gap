rm(list = ls())

library(tidyverse)
library(haven)
library(lme4)
library(ggstatsplot)
library(sjstats)

setwd("C:/R/Master thesis")
 
data <- read_dta("data/thesis_data.dta") %>% 
  janitor::clean_names()

# select variables & agediff
data <- data %>% select(country, year, wifenum, kidalive, perweight,
                        kidsex, kidbord, kidcurage, educlvl,
                        husedlvl, wealthq, urban, poly, fevtreat, measles, 
                        underweight, agediff5, agediff, respondent, mage,
                        healthcardkid, vacev, worm, kidalive,lowbw) %>% 
  mutate(underweight=as_factor(underweight),
         urban=as_factor(urban),
         country=as_factor(country),
         kidalive=as_factor(kidalive),
         kidsex=as_factor(kidsex),
         educlvl=as_factor(educlvl),
         husedlvl=as_factor(husedlvl),
         wealthq=as_factor(wealthq),
         poly=as_factor(poly),
         fevtreat=as_factor(fevtreat),
         measles=as_factor(measles),
         agediff5=as_factor(agediff5),
         kidcurage=as_factor(kidcurage),
         kidbord=as_factor(kidbord),
         mage=as_factor(mage) %>% relevel(ref="25-29"),
         kidbord=as_factor(kidbord),
         healthcardkid=as_factor(healthcardkid),
         vacev=as_factor(vacev),
         worm=as_factor(worm),
         kidalive=as_factor(kidalive),
         lowbw=as_factor(lowbw)) %>% 
  zap_labels()

data_fevtreat <- data %>% filter(!is.na(fevtreat) & !is.na(agediff5) & !is.na(kidsex) & !is.na(kidcurage) &
                                   !is.na(kidbord) & !is.na(poly) & !is.na(mage) & !is.na(educlvl) &
                                   !is.na(husedlvl) & !is.na(wealthq) & !is.na(urban) & !is.na(kidcurage))
data_measles <- data %>% filter(!is.na(measles) & !is.na(agediff5) & !is.na(kidsex) & !is.na(kidcurage) &
                                  !is.na(kidbord) & !is.na(poly) & !is.na(mage) & !is.na(educlvl) &
                                  !is.na(husedlvl) & !is.na(wealthq) & !is.na(urban) & !is.na(kidcurage) &
                                  kidcurage!="less than 1 year")
data_underweight <- data %>% filter(!is.na(underweight) & !is.na(agediff5) & !is.na(kidsex) & !is.na(kidcurage) &
                                      !is.na(kidbord) & !is.na(poly) & !is.na(mage) & !is.na(educlvl) &
                                      !is.na(husedlvl) & !is.na(wealthq) & !is.na(urban) & !is.na(kidcurage))

################################### functions #################################
odds_ratios <-function(model){
  exp(tab <- cbind(Est = fixef(model), 
                   LL = fixef(model) - 1.96 * sqrt(diag(vcov(model))), 
                   UL = fixef(model) + 1.96 * sqrt(diag(vcov(model)))))
}

###################### descriptive #########################
chisq.test(data$fevtreat,data$agediff5)
chisq.test(data$measles,data$agediff5)
chisq.test(data$underweight,data$agediff5)

data_fevtreat %>% group_by(agediff5) %>% 
  count(fevtreat,wt=perweight) %>% pivot_wider(values_from=n,names_from=fevtreat) %>% 
  mutate(percent=`Received treatment`/(`No treatment`+`Received treatment`)*100)

data_measles %>% group_by(agediff5) %>% 
  count(measles,wt=perweight) %>% pivot_wider(values_from=n,names_from=measles) %>% 
  mutate(percent=Vaccinated/(`Not vaccinated`+Vaccinated)*100)

data_underweight %>% group_by(agediff5) %>% 
  count(underweight,wt=perweight) %>% pivot_wider(values_from=n,names_from=underweight) %>% 
  mutate(percent=Underweight/(Underweight+`Normal weight`)*100)

data %>% gghistostats(agediff)
ggsave("output/agediff_dist.png",width=12,height=7)

data_fevtreat %>% 
  ggbarstats(x=fevtreat,y=agediff5, 
                                 xlab="Parental age gap")
ggsave("output/fevtreat_desc.png",width=12,height=7)

data_measles %>% 
  ggbarstats(x=measles,y=agediff5, 
                                 xlab="Parental age gap")
ggsave("output/measles_desc.png",width=12,height=7)

data_underweight %>% 
  ggbarstats(x=underweight,y=agediff5, 
                                 xlab="Parental age gap")
ggsave("output/underweight_desc.png",width=12,height=7)

###################### change ref cat. ###########################
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
fevtreat_icc <- performance::icc(fevtreat_model,by_group=TRUE)

## measles vaccination
measles_model <- glmer(measles~agediff5+
                         (1|respondent)+(1|country), nAGQ=0, data=data_measles,
                       family=binomial(link="logit"), weights=perweight)
measles_icc <- performance::icc(measles_model,by_group=TRUE)

## underweight
underweight_model <- glmer(underweight~agediff5+
                             (1|respondent)+(1|country), data=data_underweight, nAGQ=0,
                           family=binomial(link="logit"), weights=perweight)
underweight_icc <- performance::icc(underweight_model,by_group=TRUE)

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

icc <- left_join(as_tibble(as_tibble(fevtreat_icc)),
          as_tibble(as_tibble(measles_icc)), 
          by="Group", suffix=c("_fevtreat","_measles")) %>% 
  left_join(as_tibble(as_tibble(underweight_icc)) %>% 
              rename("ICC_underweight"="ICC"), by="Group") %>% 
  mutate_at(vars(-Group),round,3)

write_csv2(as.data.frame(or),"output/reg_results_onlyagegap.csv")
write_csv2(as.data.frame(icc),"output/icc_onlyagegap.csv")

#################### children models #########################
## fever treatment
fevtreat_model <- glmer(fevtreat~agediff5+kidsex+kidcurage+kidbord+
                          (1|respondent)+(1|country), data=data_fevtreat, nAGQ=0,
                        family=binomial(link="logit"), weights=perweight)
fevtreat_icc <- performance::icc(fevtreat_model,by_group=TRUE)

## measeles vaccination
measles_model <- glmer(measles~agediff5+kidsex+kidcurage+kidbord+
                         (1|respondent)+(1|country), nAGQ=0,
                       data=data_measles,
                       family=binomial(link="logit"), weights=perweight)
measles_icc <- performance::icc(measles_model,by_group=TRUE)


## underweight
underweight_model <- glmer(underweight~agediff5+kidsex+kidcurage+kidbord+
                             (1|respondent)+(1|country), data=data_underweight, nAGQ=0,
                           family=binomial(link="logit"), weights=perweight)
underweight_icc <- performance::icc(underweight_model,by_group=TRUE)

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

icc <- left_join(as_tibble(as_tibble(fevtreat_icc)),
                 as_tibble(as_tibble(measles_icc)), 
                 by="Group", suffix=c("_fevtreat","_measles")) %>% 
  left_join(as_tibble(as_tibble(underweight_icc)) %>% 
              rename("ICC_underweight"="ICC"), by="Group") %>% 
  mutate_at(vars(-Group),round,3)

write_csv2(as.data.frame(or),"output/reg_results_children.csv")
write_csv2(as.data.frame(icc),"output/icc_children.csv")

##################### main models ##########################
## fever treatment
fevtreat_model <- glmer(fevtreat~agediff5+kidsex+kidcurage+kidbord+poly+
                                  mage+educlvl+husedlvl+wealthq+urban+
                                  (1|respondent)+(1|country), data=data_fevtreat, nAGQ=0,
                                family=binomial(link="logit"), weights=perweight)
fevtreat_icc <- performance::icc(fevtreat_model,by_group=TRUE)
fevtreat_res <- ggcoefstats(fevtreat_model,
            exclude.intercept=T,stats.labels=F)
ggsave("output/fevtreat_res.png",plot=fevtreat_res,width=7,height=10)

## measeles vaccination
measles_model <- glmer(measles~agediff5+kidsex+kidcurage+kidbord+poly+
                                 mage+educlvl+husedlvl+wealthq+urban+
                                 (1|respondent)+(1|country), nAGQ=0,
                               data=data_measles,
                               family=binomial(link="logit"), weights=perweight)
measles_icc <- performance::icc(measles_model,by_group=TRUE)
measles_res <- ggcoefstats(measles_model,
            exclude.intercept=T,stats.labels=F)
ggsave("output/measles_res.png",plot=measles_res,width=7,height=10)

## underweight
underweight_model <- glmer(underweight~agediff5+kidsex+kidcurage+kidbord+poly+
                                     mage+educlvl+husedlvl+wealthq+urban+
                                     (1|respondent)+(1|country), data=data_underweight, nAGQ=0,
                                   family=binomial(link="logit"), weights=perweight)
underweight_icc <- performance::icc(underweight_model,by_group=TRUE)
underweight_res <- ggcoefstats(underweight_model,
            exclude.intercept=T,stats.labels=F)
ggsave("output/underweight_res.png",plot=underweight_res,width=7,height=10)

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

icc <- left_join(as_tibble(as_tibble(fevtreat_icc)),
                 as_tibble(as_tibble(measles_icc)), 
                 by="Group", suffix=c("_fevtreat","_measles")) %>% 
  left_join(as_tibble(as_tibble(underweight_icc)) %>% 
              rename("ICC_underweight"="ICC"), by="Group") %>% 
  mutate_at(vars(-Group),round,3)

write_csv2(as.data.frame(or),"output/reg_results_main.csv")
write_csv2(as.data.frame(icc),"output/icc_main.csv")
