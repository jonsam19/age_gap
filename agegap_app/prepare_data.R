## load models
fevtreat_model1 <- readRDS("models/fevtreat1.rds")

measles_model1 <- readRDS("models/measles1.rds")

underweight_model1 <- readRDS("models/underweight1.rds")

fevtreat_model2 <- readRDS("models/fevtreat2.rds")

measles_model2 <- readRDS("models/measles2.rds")

underweight_model2 <- readRDS("models/underweight2.rds")

fevtreat_model3 <- readRDS("models/fevtreat3.rds")

measles_model3 <- readRDS("models/measles3.rds")

underweight_model3 <- readRDS("models/underweight3.rds")

## load plots
fevtreat_plot1 <- readRDS("plots/fevtreat_plot1.rds")
fevtreat_plot2 <- readRDS("plots/fevtreat_plot2.rds")
fevtreat_plot3 <- readRDS("plots/fevtreat_plot3.rds")
measles_plot1 <- readRDS("plots/measles_plot1.rds")
measles_plot2 <- readRDS("plots/measles_plot2.rds")
measles_plot3 <- readRDS("plots/measles_plot3.rds")
underweight_plot1 <- readRDS("plots/underweight_plot1.rds")
underweight_plot2 <- readRDS("plots/underweight_plot2.rds")
underweight_plot3 <- readRDS("plots/underweight_plot3.rds")

## load tables
fevtreat_table <- readRDS("plots/fevtreat_table.rds")
measles_table <- readRDS("plots/measles_table.rds")
underweight_table <- readRDS("plots/underweight_table.rds")

## filter variables
data_fevtreat <- readRDS("cleaned_data.rds") |> filter(!is.na(fevtreat) & !is.na(agediff5) & !is.na(kidsex) & !is.na(kidcurage) &
                                   !is.na(kidbord) & !is.na(poly) & !is.na(mage) & !is.na(educlvl) &
                                   !is.na(husedlvl) & !is.na(wealthq) & !is.na(urban) & !is.na(kidcurage))
data_measles <- readRDS("cleaned_data.rds") |> filter(!is.na(measles) & !is.na(agediff5) & !is.na(kidsex) & !is.na(kidcurage) &
                                  !is.na(kidbord) & !is.na(poly) & !is.na(mage) & !is.na(educlvl) &
                                  !is.na(husedlvl) & !is.na(wealthq) & !is.na(urban) & !is.na(kidcurage) &
                                  kidcurage!="less than 1 year")
data_underweight <- readRDS("cleaned_data.rds") |> filter(!is.na(underweight) & !is.na(agediff5) & !is.na(kidsex) & !is.na(kidcurage) &
                                      !is.na(kidbord) & !is.na(poly) & !is.na(mage) & !is.na(educlvl) &
                                      !is.na(husedlvl) & !is.na(wealthq) & !is.na(urban) & !is.na(kidcurage))

countries = unique(data_fevtreat$country) |> as.vector() |> sort()

## reweight
data_fevtreat <- data_fevtreat %>%
  group_by(respondent) %>%
  mutate(weight_mult = n() / sum(perweight)) %>%
  ungroup() %>%
  mutate(swt = perweight * weight_mult)

data_measles <- data_measles %>%
  group_by(respondent) %>%
  mutate(weight_mult = n() / sum(perweight)) %>%
  ungroup() %>%
  mutate(swt = perweight * weight_mult)

data_underweight <- data_underweight %>%
  group_by(respondent) %>%
  mutate(weight_mult = n() / sum(perweight)) %>%
  ungroup() %>%
  mutate(swt = perweight * weight_mult)

