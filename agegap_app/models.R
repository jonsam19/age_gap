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
# fevtreat_icc <- performance::icc(fevtreat_model,by_group=TRUE) |> as_tibble()
saveRDS(fevtreat_model,file="models/fevtreat1.rds")
# write_csv(fevtreat_icc,file="models/fevtreat1_icc.csv")

## measles vaccination
measles_model <- glmer(measles~agediff5+
                         (1|respondent)+(1|country), nAGQ=0, data=data_measles,
                       family=binomial(link="logit"), weights=swt)
# measles_icc <- performance::icc(measles_model,by_group=TRUE)
saveRDS(measles_model,file="models/measles1.rds")
# write_csv(measles_icc,file="models/measles1_icc.csv")

## underweight
underweight_model <- glmer(underweight~agediff5+
                             (1|respondent)+(1|country), data=data_underweight, nAGQ=0,
                           family=binomial(link="logit"), weights=swt)
# underweight_icc <- performance::icc(underweight_model,by_group=TRUE)
saveRDS(underweight_model,file="models/underweight1.rds")
# write_csv(underweight_icc,file="models/underweight1_icc.csv")

## save as odds ratios
# or1 <- left_join(as_tibble(odds_ratios(fevtreat_model),rownames="term"),
#                 as_tibble(odds_ratios(measles_model),rownames="term"), 
#                 by="term", suffix=c("fevtreat","measles")) |> 
#   left_join(as_tibble(odds_ratios(underweight_model),rownames="term") |> 
#               rename("Estunderweight"="Est", "LLunderweight"="LL",
#                      "ULunderweight"="UL"), by="term") |> 
#   mutate_at(vars(-term), round,2) |> 
#   mutate(ci_fevtreat=paste(Estfevtreat, " ", "(", LLfevtreat, "-", ULfevtreat, ")",sep=""),
#          ci_measles=paste(Estmeasles, " ", "(", LLmeasles, "-", ULmeasles, ")",sep=""),
#          ci_underweight=paste(Estunderweight, " ", "(", LLunderweight, "-", ULunderweight, ")",sep="")) |> 
#   select(term,ci_fevtreat,ci_measles,ci_underweight)

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
# fevtreat_icc <- performance::icc(fevtreat_model,by_group=TRUE)
saveRDS(fevtreat_model,file="models/fevtreat2.rds")
# write_csv(fevtreat_icc,file="models/fevtreat2_icc.csv")

## measeles vaccination
measles_model <- glmer(measles~agediff5+kidsex+kidcurage+kidbord+
                         (1|respondent)+(1|country), nAGQ=0,
                       data=data_measles,
                       family=binomial(link="logit"), weights=swt)
# measles_icc <- performance::icc(fevtreat_model,by_group=TRUE)
saveRDS(measles_model,file="models/measles2.rds")
# write_csv(measles_icc,file="models/measles2_icc.csv")

## underweight
underweight_model <- glmer(underweight~agediff5+kidsex+kidcurage+kidbord+
                             (1|respondent)+(1|country), data=data_underweight, nAGQ=0,
                           family=binomial(link="logit"), weights=swt)
# underweight_icc <- performance::icc(underweight_model,by_group=TRUE)
saveRDS(underweight_model,file="models/underweight2.rds")
# write_csv(underweight_icc,file="models/underweight2_icc.csv")

## save as odds ratios
# or2 <- left_join(as_tibble(odds_ratios(fevtreat_model),rownames="term"),
#                 as_tibble(odds_ratios(measles_model),rownames="term"), 
#                 by="term", suffix=c("fevtreat","measles")) |> 
#   left_join(as_tibble(odds_ratios(underweight_model),rownames="term") |> 
#               rename("Estunderweight"="Est", "LLunderweight"="LL",
#                      "ULunderweight"="UL"), by="term") |> 
#   mutate_at(vars(-term), round,2) |> 
#   mutate(ci_fevtreat=paste(Estfevtreat, " ", "(", LLfevtreat, "-", ULfevtreat, ")",sep=""),
#          ci_measles=paste(Estmeasles, " ", "(", LLmeasles, "-", ULmeasles, ")",sep=""),
#          ci_underweight=paste(Estunderweight, " ", "(", LLunderweight, "-", ULunderweight, ")",sep="")) |> 
#   select(term,ci_fevtreat,ci_measles,ci_underweight)

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
# fevtreat_icc <- performance::icc(fevtreat_model,by_group=TRUE)
saveRDS(fevtreat_model,file="models/fevtreat3.rds")
# write_csv(fevtreat_icc,file="models/fevtreat3_icc.csv")

## measeles vaccination
measles_model <- glmer(measles~agediff5+kidsex+kidcurage+kidbord+poly+
                         mage+educlvl+husedlvl+wealthq+urban+
                         (1|respondent)+(1|country), nAGQ=0,
                       data=data_measles,
                       family=binomial(link="logit"), weights=swt)
# measles_icc <- performance::icc(measles_model,by_group=TRUE)
saveRDS(measles_model,file="models/measles3.rds")
# write_csv(measles_icc,file="models/measles3_icc.csv")

## underweight
underweight_model <- glmer(underweight~agediff5+kidsex+kidcurage+kidbord+poly+
                             mage+educlvl+husedlvl+wealthq+urban+
                             (1|respondent)+(1|country), data=data_underweight, nAGQ=0,
                           family=binomial(link="logit"), weights=swt)
# underweight_icc <- performance::icc(underweight_model,by_group=TRUE)
saveRDS(underweight_model,file="models/underweight3.rds")
# write_csv(underweight_icc,file="models/underweight3_icc.csv")

## save as odds ratios
# or3 <- left_join(as_tibble(odds_ratios(fevtreat_model3),rownames="term"),
#                 as_tibble(odds_ratios(measles_model3),rownames="term"), 
#                 by="term", suffix=c("fevtreat","measles")) |> 
#   left_join(as_tibble(odds_ratios(underweight_model3),rownames="term") |> 
#               rename("Estunderweight"="Est", "LLunderweight"="LL",
#                      "ULunderweight"="UL"), by="term") |> 
#   mutate_at(vars(-term), round,2) |> 
#   mutate(ci_fevtreat=paste(Estfevtreat, " ", "(", LLfevtreat, "-", ULfevtreat, ")",sep=""),
#          ci_measles=paste(Estmeasles, " ", "(", LLmeasles, "-", ULmeasles, ")",sep=""),
#          ci_underweight=paste(Estunderweight, " ", "(", LLunderweight, "-", ULunderweight, ")",sep="")) |> 
#   select(term,ci_fevtreat,ci_measles,ci_underweight)

# icc <- left_join(as_tibble(as_tibble(fevtreat_icc)),
#                  as_tibble(as_tibble(measles_icc)), 
#                  by="Group", suffix=c("_fevtreat","_measles")) |> 
#   left_join(as_tibble(as_tibble(underweight_icc)) |> 
#               rename("ICC_underweight"="ICC"), by="Group") |> 
#   mutate_at(vars(-Group),round,3)

############################### create plots ################################
ggplotly(readRDS("models/fevtreat1.rds") |> 
           plot_model(title="Fever treatment",vline.color="black",axis.lim=c(0.99, 1.01),
                      group.terms=c(1,1,1,1)) +
           set_theme(base = theme_light()) + theme(legend.position="none")) |> 
  plotly_build() |> saveRDS("plots/fevtreat_plot1.rds")
ggplotly(readRDS("models/measles1.rds") |> 
           plot_model(title="Measles",vline.color="black",axis.lim=c(0.99, 1.01),
                      group.terms=c(1,1,1,1)) +
           set_theme(base = theme_light()) + theme(legend.position="none")) |> 
  plotly_build() |> saveRDS("plots/measles_plot1.rds")
ggplotly(readRDS("models/underweight1.rds") |> 
           plot_model(title="Measles",vline.color="black",axis.lim=c(0.99, 1.01),
                      group.terms=c(1,1,1,1)) +
           set_theme(base = theme_light()) + theme(legend.position="none")) |> 
  plotly_build() |> saveRDS("plots/underweight_plot1.rds")



ggplotly(readRDS("models/fevtreat2.rds") |> 
           plot_model(title="Fever treatment",vline.color="black",axis.lim=c(0.99, 1.01),
                      group.terms=c(1,1,1,1,2,3,3,3,3,4,4,4,4,4)) +
           set_theme(base = theme_light()) + theme(legend.position="none")) |> 
  plotly_build() |> saveRDS("plots/fevtreat_plot2.rds")
ggplotly(readRDS("models/measles2.rds") |> 
           plot_model(title="Measles",vline.color="black",axis.lim=c(0.99, 1.01),
                      group.terms=c(1,1,1,1,2,3,3,3,4,4,4,4,4)) +
           set_theme(base = theme_light()) + theme(legend.position="none")) |> 
  plotly_build() |> saveRDS("plots/measles_plot2.rds")
ggplotly(readRDS("models/underweight2.rds") |> 
           plot_model(title="Underweight",vline.color="black",axis.lim=c(0.5, 1.01),
                      group.terms=c(1,1,1,1,2,3,3,3,3,4,4,4,4,4)) +
           set_theme(base = theme_light()) + theme(legend.position="none")) |> 
  plotly_build() |> saveRDS("plots/underweight_plot2.rds")



ggplotly(readRDS("models/fevtreat3.rds") |> 
  plot_model(title="Fever treatment",vline.color="black",axis.lim=c(0.99, 1.01),
             group.terms=c(1,1,1,1,2,3,3,3,3,4,4,4,4,4,
                           5,6,6,6,6,6,7,7,7,8,8,8,9,9,9,9,10)) +
    set_theme(base = theme_light()) + theme(legend.position="none")) |> 
  plotly_build() |> saveRDS("plots/fevtreat_plot3.rds")
ggplotly(readRDS("models/measles3.rds") |> 
           plot_model(title="Measles",vline.color="black",axis.lim=c(0.99, 1.01),
                      group.terms=c(1,1,1,1,2,3,3,3,4,4,4,4,4,
                                    5,6,6,6,6,6,7,7,7,8,8,8,9,9,9,9,10)) +
           set_theme(base = theme_light()) + theme(legend.position="none")) |> 
  plotly_build() |> saveRDS("plots/measles_plot3.rds")
ggplotly(readRDS("models/underweight3.rds") |> 
           plot_model(title="Underweight",vline.color="black",axis.lim=c(0.5, 1.01),
                      group.terms=c(1,1,1,1,2,3,3,3,3,4,4,4,4,4,
                                    5,6,6,6,6,6,7,7,7,8,8,8,9,9,9,9,10)) +
           set_theme(base = theme_light()) + theme(legend.position="none")) |> 
  plotly_build() |> saveRDS("plots/underweight_plot3.rds")



############################### create tables ################################
tab_model(fevtreat_model1,fevtreat_model2,fevtreat_model3,show.obs=TRUE,
          dv.labels = c("Model 1: age gap","Model 2: children","Model 3: parents"),
          pred.labels=c("Intercept","Age gap: <0","Age gap: 0-4","Age gap: 10-14","Age gap: 15+",
                        "Sex: female","Child age: 1 year","Child age: 2 years",
                        "Child age: 3 years","Child age: 4 years",
                        "Birth order: 2nd","Birth order: 3d","Birth order: 4th","Birth order: 5th",
                        "Birth order: 6th +","Polygynous","Mother age: 15-19","Mother age: 20-24",
                        "Mother age: 30-34","Mother age: 35-39","Mother age: 40+",
                        "Mother education: primary","Mother education: secondary",
                        "Mother education: higher","Husband education: primary",
                        "Husband education: secondary","Husband education: higher",
                        "Wealth index: poorer","Wealth index: middle","Wealth index: richer",
                        "Wealth index: richest","Urban")) |> 
  saveRDS("plots/fevtreat_table.rds")

tab_model(measles_model1,measles_model2,measles_model3,show.obs=TRUE,
          dv.labels = c("Model 1: age gap","Model 2: children","Model 3: parents"),
          pred.labels=c("Intercept","Age gap: <0","Age gap: 0-4","Age gap: 10-14","Age gap: 15+",
                        "Sex: female","Child age: 2 years",
                        "Child age: 3 years","Child age: 4 years",
                        "Birth order: 2nd","Birth order: 3d","Birth order: 4th","Birth order: 5th",
                        "Birth order: 6th +","Polygynous","Mother age: 15-19","Mother age: 20-24",
                        "Mother age: 30-34","Mother age: 35-39","Mother age: 40+",
                        "Mother education: primary","Mother education: secondary",
                        "Mother education: higher","Husband education: primary",
                        "Husband education: secondary","Husband education: higher",
                        "Wealth index: poorer","Wealth index: middle","Wealth index: richer",
                        "Wealth index: richest","Urban")) |> 
  saveRDS("plots/measles_table.rds")

tab_model(underweight_model1,underweight_model2,underweight_model3,show.obs=TRUE,
          dv.labels = c("Model 1: age gap","Model 2: children","Model 3: parents"),
          pred.labels=c("Intercept","Age gap: <0","Age gap: 0-4","Age gap: 10-14","Age gap: 15+",
                        "Sex: female","Child age: 1 year","Child age: 2 years",
                        "Child age: 3 years","Child age: 4 years",
                        "Birth order: 2nd","Birth order: 3d","Birth order: 4th","Birth order: 5th",
                        "Birth order: 6th +","Polygynous","Mother age: 15-19","Mother age: 20-24",
                        "Mother age: 30-34","Mother age: 35-39","Mother age: 40+",
                        "Mother education: primary","Mother education: secondary",
                        "Mother education: higher","Husband education: primary",
                        "Husband education: secondary","Husband education: higher",
                        "Wealth index: poorer","Wealth index: middle","Wealth index: richer",
                        "Wealth index: richest","Urban")) |> 
  saveRDS("plots/underweight_table.rds")