
data <- haven::read_dta("thesis_data.dta") |>
  janitor::clean_names()

# select variables & agediff
data <- data |> select(country, year, wifenum, kidalive, perweight,
                        kidsex, kidbord, kidcurage, educlvl,
                        husedlvl, wealthq, urban, poly, fevtreat, measles, 
                        underweight, agediff5, agediff, respondent, mage,
                        healthcardkid, vacev, worm, kidalive,lowbw) |> 
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
         agediff5=as.character(agediff5),
         agediff5=case_when(agediff5==">15"~"15+",
                            TRUE~agediff5),
         agediff5=factor(agediff5, levels=c("<0","0-4","5-9","10-14","15+")),
         kidcurage=as_factor(kidcurage),
         kidbord=as_factor(kidbord),
         mage=as_factor(mage) |> relevel(ref="25-29"),
         kidbord=as_factor(kidbord),
         healthcardkid=as_factor(healthcardkid),
         vacev=as_factor(vacev),
         worm=as_factor(worm),
         kidalive=as_factor(kidalive),
         lowbw=as_factor(lowbw)) |> 
  zap_labels()

## load models
fevtreat_model1 <- readRDS("models/fevtreat1.rds")
fevtreat_icc1 <- read_csv("models/fevtreat1_icc.csv")

measles_model1 <- readRDS("models/measles1.rds")
measles_icc1 <- read_csv("models/measles1_icc.csv")

underweight_model1 <- readRDS("models/underweight1.rds")
underweight_icc1 <- read_csv("models/underweight1_icc.csv")

fevtreat_model2 <- readRDS("models/fevtreat2.rds")
fevtreat_icc2 <- read_csv("models/fevtreat2_icc.csv")

measles_model2 <- readRDS("models/measles2.rds")
measles_icc2 <- read_csv("models/measles2_icc.csv")

underweight_model2 <- readRDS("models/underweight2.rds")
underweight_icc2 <- read_csv("models/underweight2_icc.csv")

fevtreat_model3 <- readRDS("models/fevtreat3.rds")
fevtreat_icc3 <- read_csv("models/fevtreat3_icc.csv")

measles_model3 <- readRDS("models/measles3.rds")
measles_icc3 <- read_csv("models/measles3_icc.csv")

underweight_model3 <- readRDS("models/underweight3.rds")
underweight_icc3 <- read_csv("models/underweight3_icc.csv")

## load plots
fevtreat_plot1 <- readRDS("plots/fevtreat_plot1.rds")
fevtreat_plot2 <- readRDS("plots/fevtreat_plot2.rds")

## filter variables
data_fevtreat <- data |> filter(!is.na(fevtreat) & !is.na(agediff5) & !is.na(kidsex) & !is.na(kidcurage) &
                                   !is.na(kidbord) & !is.na(poly) & !is.na(mage) & !is.na(educlvl) &
                                   !is.na(husedlvl) & !is.na(wealthq) & !is.na(urban) & !is.na(kidcurage))
data_measles <- data |> filter(!is.na(measles) & !is.na(agediff5) & !is.na(kidsex) & !is.na(kidcurage) &
                                  !is.na(kidbord) & !is.na(poly) & !is.na(mage) & !is.na(educlvl) &
                                  !is.na(husedlvl) & !is.na(wealthq) & !is.na(urban) & !is.na(kidcurage) &
                                  kidcurage!="less than 1 year")
data_underweight <- data |> filter(!is.na(underweight) & !is.na(agediff5) & !is.na(kidsex) & !is.na(kidcurage) &
                                      !is.na(kidbord) & !is.na(poly) & !is.na(mage) & !is.na(educlvl) &
                                      !is.na(husedlvl) & !is.na(wealthq) & !is.na(urban) & !is.na(kidcurage))

## reweight
data <- data %>%
  group_by(respondent) %>%
  mutate(weight_mult = n() / sum(perweight)) %>%
  ungroup() %>%
  mutate(swt = perweight * weight_mult)

data_fevtreat <- data_fevtreat %>%
  group_by(respondent) %>%
  mutate(weight_mult = n() / sum(perweight)) %>%
  ungroup() %>%
  mutate(swt = perweight * weight_mult)

data_measles <- data_fevtreat %>%
  group_by(respondent) %>%
  mutate(weight_mult = n() / sum(perweight)) %>%
  ungroup() %>%
  mutate(swt = perweight * weight_mult)

data_underweight <- data_fevtreat %>%
  group_by(respondent) %>%
  mutate(weight_mult = n() / sum(perweight)) %>%
  ungroup() %>%
  mutate(swt = perweight * weight_mult)



################################### functions #################################
odds_ratios <-function(model){
  exp(tab <- cbind(Est = fixef(model), 
                   LL = fixef(model) - 1.96 * sqrt(diag(vcov(model))), 
                   UL = fixef(model) + 1.96 * sqrt(diag(vcov(model)))))
}
