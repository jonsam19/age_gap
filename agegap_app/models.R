data <- data |> 
  mutate(agediff5=as_factor(agediff5) |> relevel(ref="5-9"))
data_fevtreat <- data_fevtreat |> 
  mutate(agediff5=as_factor(agediff5) |> relevel(ref="5-9"))
data_measles <- data_measles |> 
  mutate(agediff5=as_factor(agediff5) |> relevel(ref="5-9"))
data_underweight <- data_underweight |> 
  mutate(agediff5=as_factor(agediff5) |> relevel(ref="5-9"))

##################### only age gap models #########################
## fever treatment
fevtreat_model <- glmer(fevtreat~agediff5+
                          (1|respondent)+(1|country), data=data_fevtreat, nAGQ=0,
                        family=binomial(link="logit"), weights=swt)
fevtreat_icc <- performance::icc(fevtreat_model,by_group=TRUE) |> as_tibble()
saveRDS(fevtreat_model,file="models/fevtreat1.rds")
write_csv(fevtreat_icc,file="models/fevtreat1_icc.csv")

## measles vaccination
measles_model <- glmer(measles~agediff5+
                         (1|respondent)+(1|country), nAGQ=0, data=data_measles,
                       family=binomial(link="logit"), weights=swt)
measles_icc <- performance::icc(measles_model,by_group=TRUE)
saveRDS(measles_model,file="models/measles1.rds")
write_csv(measles_icc,file="models/measles1_icc.csv")

## underweight
underweight_model <- glmer(underweight~agediff5+
                             (1|respondent)+(1|country), data=data_underweight, nAGQ=0,
                           family=binomial(link="logit"), weights=swt)
underweight_icc <- performance::icc(underweight_model,by_group=TRUE)
saveRDS(underweight_model,file="models/underweight1.rds")
write_csv(underweight_icc,file="models/underweight1_icc.csv")

## save as odds ratios
or1 <- left_join(as_tibble(odds_ratios(fevtreat_model),rownames="term"),
                as_tibble(odds_ratios(measles_model),rownames="term"), 
                by="term", suffix=c("fevtreat","measles")) |> 
  left_join(as_tibble(odds_ratios(underweight_model),rownames="term") |> 
              rename("Estunderweight"="Est", "LLunderweight"="LL",
                     "ULunderweight"="UL"), by="term") |> 
  mutate_at(vars(-term), round,2) |> 
  mutate(ci_fevtreat=paste(Estfevtreat, " ", "(", LLfevtreat, "-", ULfevtreat, ")",sep=""),
         ci_measles=paste(Estmeasles, " ", "(", LLmeasles, "-", ULmeasles, ")",sep=""),
         ci_underweight=paste(Estunderweight, " ", "(", LLunderweight, "-", ULunderweight, ")",sep="")) |> 
  select(term,ci_fevtreat,ci_measles,ci_underweight)

# icc <- left_join(as_tibble(as_tibble(fevtreat_icc)),
#           as_tibble(as_tibble(measles_icc)), 
#           by="Group", suffix=c("_fevtreat","_measles")) |> 
#   left_join(as_tibble(as_tibble(underweight_icc)) |> 
#               rename("ICC_underweight"="ICC"), by="Group") |> 
#   mutate_at(vars(-Group),round,3)

#################### children models #########################
## fever treatment
fevtreat_model <- glmer(fevtreat~agediff5+kidsex+kidcurage+kidbord+
                          (1|respondent)+(1|country), data=data_fevtreat, nAGQ=0,
                        family=binomial(link="logit"), weights=swt)
fevtreat_icc <- performance::icc(fevtreat_model,by_group=TRUE)
saveRDS(fevtreat_model,file="models/fevtreat2.rds")
write_csv(fevtreat_icc,file="models/fevtreat2_icc.csv")

## measeles vaccination
measles_model <- glmer(measles~agediff5+kidsex+kidcurage+kidbord+
                         (1|respondent)+(1|country), nAGQ=0,
                       data=data_measles,
                       family=binomial(link="logit"), weights=swt)
measles_icc <- performance::icc(fevtreat_model,by_group=TRUE)
saveRDS(measles_model,file="models/measles2.rds")
write_csv(measles_icc,file="models/measles2_icc.csv")

## underweight
underweight_model <- glmer(underweight~agediff5+kidsex+kidcurage+kidbord+
                             (1|respondent)+(1|country), data=data_underweight, nAGQ=0,
                           family=binomial(link="logit"), weights=swt)
underweight_icc <- performance::icc(underweight_model,by_group=TRUE)
saveRDS(underweight_model,file="models/underweight2.rds")
write_csv(underweight_icc,file="models/underweight2_icc.csv")

## save as odds ratios
or2 <- left_join(as_tibble(odds_ratios(fevtreat_model),rownames="term"),
                as_tibble(odds_ratios(measles_model),rownames="term"), 
                by="term", suffix=c("fevtreat","measles")) |> 
  left_join(as_tibble(odds_ratios(underweight_model),rownames="term") |> 
              rename("Estunderweight"="Est", "LLunderweight"="LL",
                     "ULunderweight"="UL"), by="term") |> 
  mutate_at(vars(-term), round,2) |> 
  mutate(ci_fevtreat=paste(Estfevtreat, " ", "(", LLfevtreat, "-", ULfevtreat, ")",sep=""),
         ci_measles=paste(Estmeasles, " ", "(", LLmeasles, "-", ULmeasles, ")",sep=""),
         ci_underweight=paste(Estunderweight, " ", "(", LLunderweight, "-", ULunderweight, ")",sep="")) |> 
  select(term,ci_fevtreat,ci_measles,ci_underweight)

# icc <- left_join(as_tibble(as_tibble(fevtreat_icc)),
#                  as_tibble(as_tibble(measles_icc)), 
#                  by="Group", suffix=c("_fevtreat","_measles")) |> 
#   left_join(as_tibble(as_tibble(underweight_icc)) |> 
#               rename("ICC_underweight"="ICC"), by="Group") |> 
#   mutate_at(vars(-Group),round,3)


##################### main models ##########################
## fever treatment
fevtreat_model <- glmer(fevtreat~agediff5+kidsex+kidcurage+kidbord+poly+
                          mage+educlvl+husedlvl+wealthq+urban+
                          (1|respondent)+(1|country), data=data_fevtreat, nAGQ=0,
                        family=binomial(link="logit"), weights=swt)
fevtreat_icc <- performance::icc(fevtreat_model,by_group=TRUE)
saveRDS(fevtreat_model,file="models/fevtreat3.rds")
write_csv(fevtreat_icc,file="models/fevtreat3_icc.csv")

## measeles vaccination
measles_model <- glmer(measles~agediff5+kidsex+kidcurage+kidbord+poly+
                         mage+educlvl+husedlvl+wealthq+urban+
                         (1|respondent)+(1|country), nAGQ=0,
                       data=data_measles,
                       family=binomial(link="logit"), weights=swt)
measles_icc <- performance::icc(measles_model,by_group=TRUE)
saveRDS(measles_model,file="models/measles3.rds")
write_csv(measles_icc,file="models/measles3_icc.csv")

## underweight
underweight_model <- glmer(underweight~agediff5+kidsex+kidcurage+kidbord+poly+
                             mage+educlvl+husedlvl+wealthq+urban+
                             (1|respondent)+(1|country), data=data_underweight, nAGQ=0,
                           family=binomial(link="logit"), weights=swt)
underweight_icc <- performance::icc(underweight_model,by_group=TRUE)
saveRDS(underweight_model,file="models/underweight3.rds")
write_csv(underweight_icc,file="models/underweight3_icc.csv")

## save as odds ratios
or3 <- left_join(as_tibble(odds_ratios(fevtreat_model3),rownames="term"),
                as_tibble(odds_ratios(measles_model3),rownames="term"), 
                by="term", suffix=c("fevtreat","measles")) |> 
  left_join(as_tibble(odds_ratios(underweight_model3),rownames="term") |> 
              rename("Estunderweight"="Est", "LLunderweight"="LL",
                     "ULunderweight"="UL"), by="term") |> 
  mutate_at(vars(-term), round,2) |> 
  mutate(ci_fevtreat=paste(Estfevtreat, " ", "(", LLfevtreat, "-", ULfevtreat, ")",sep=""),
         ci_measles=paste(Estmeasles, " ", "(", LLmeasles, "-", ULmeasles, ")",sep=""),
         ci_underweight=paste(Estunderweight, " ", "(", LLunderweight, "-", ULunderweight, ")",sep="")) |> 
  select(term,ci_fevtreat,ci_measles,ci_underweight)

# icc <- left_join(as_tibble(as_tibble(fevtreat_icc)),
#                  as_tibble(as_tibble(measles_icc)), 
#                  by="Group", suffix=c("_fevtreat","_measles")) |> 
#   left_join(as_tibble(as_tibble(underweight_icc)) |> 
#               rename("ICC_underweight"="ICC"), by="Group") |> 
#   mutate_at(vars(-Group),round,3)

############################### create plots ################################
readRDS("models/fevtreat1.rds") |> 
  plot_model(show.values=TRUE,title="Fever treatment",value.size=3) |> 
  ggplotly() %>% plotly_build() |> saveRDS("plots/fevtreat_plot1.rds")
measles_plot1 <- readRDS("models/measles1.rds")
underweight_plot1 <- readRDS("models/underweight1.rds")

readRDS("models/fevtreat2.rds") |> 
  plot_model(show.values=TRUE,title="Fever treatment",value.size=3) |> 
  ggplotly() %>% plotly_build() |> saveRDS("plots/fevtreat_plot2.rds")
measles_plot2 <- readRDS("models/measles2.rds")
underweight_plot2 <- readRDS("models/underweight2.rds")

fevtreat_plot3 <- readRDS("models/fevtreat3.rds")
measles_plot3 <- readRDS("models/measles3.rds")
underweight_plot3 <- readRDS("models/underweight3.rds")