
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