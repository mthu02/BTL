setwd('C:/Users/ADMIN/Desktop/02AD/BTL')
library(dplyr)
library(mice)
library(mlbench)
library(caret)
library(tidyr)
library(logistf)
library(tidyverse)
library(finalfit)
library(forcats)
library(epiDisplay)
library(gtsummary)
theme_set(theme_bw())

set.seed(145)

## Import Tobacco Dataset
tbc <- read.csv("VIETNAM_National_07.csv")

## Select some variables
df <- tbc %>% dplyr::select('CR1','CR12','CR14','CR24','CR25','CR30',
                            'CR39','CR40','CR41','CR42','CR43',
                            'CR44','CR45','CR47','CR48','CR51','CR53',
                            'VNR56','VNR57','VNR67')

# Recode variables
df$CR1 <- recode(df$CR1,"1"=1,"2"=0)
df$CR12 <- recode(df$CR12, "1"=0,"2"=1,"3"=2,"4"=3,"5"=4)
df$CR14 <- recode(df$CR14, "1"=1,"2"=0)
df$CR24 <- recode(df$CR24,  "1"=0,"2"=1,"3"=2,"4"=3)
df$CR25 <- recode(df$CR25,  "1"=0,"2"=1,"3"=2,"4"=3)
df$CR30 <- recode(df$CR30, "1"=0,"2"=1,"3"=2,"4"=3,"5"=4)
df$CR39 <- recode(df$CR39, "1"=2,"2"=1,"3"=0)
df$CR40 <- recode(df$CR40, "1"=0,"2"=3,"3"=2,"4"=1)
df$CR41 <- recode(df$CR41, "1"=0,"4"=1,"2"=3,"3"=2) 
df$CR42 <- recode(df$CR42, "1"=1,"2"=0)
df$CR43 <- recode(df$CR43, "1"=0,"4"=1,"2"=3,"3"=2) 
df$CR44 <- recode(df$CR44, "3"=0,"1"=2,"2"=1)
df$CR45 <- recode(df$CR45, "3"=0,"1"=2,"2"=1)
df$CR47 <- recode(df$CR47, "1"=1,"2"=0)
df$CR48 <- recode(df$CR48, "1"=1,"2"=0,"3"=2)
#df$CR51 <- recode(df$CR51,"1"=0,"2"=1,"3"=2,"4"=3,"5"=4,"6"=5)
df$CR53 <- recode(df$CR53,"1"=1,"2"=0)
df$VNR57 <- recode(df$VNR57, "1"=1,"2"=0)
df$VNR56 <- recode(df$VNR56, "3"=0,"1"=2,"2"=1)

#11.2
explanatory <- c('CR12','CR14','CR24','CR25','CR30',
                 'CR39','CR40','CR41','CR42','CR43',
                 'CR44','CR45','CR47','CR48','CR53',#'CR51',
                 'VNR56','VNR57','VNR67')
dependent <- 'CR1'
df %>%
  ff_glimpse(dependent, explanatory)
#8.3 Recode the data
df <- df %>%
  mutate(CR1.factor = factor(CR1) %>%
           fct_recode(
             "Non-smoking" = "0",
             "Smoking" = "1") %>%
           ff_label("Smoking Status"),
         
         CR12.factor = factor(CR12) %>%
           fct_recode("None" = "0",
                      "Both" = "1",
                      "Father only" = "2",
                      "Mother only" = "3",
                      "I dont know" = "4") %>%
           ff_label("Parents smoke"),
         
         CR14.factor = factor(CR14) %>%
           fct_recode("Yes" = "1",
                      "No" = "0") %>%
           ff_label("Family discussion about the harmful effects of smoking"),
         
         CR24.factor = factor(CR24) %>%
           fct_recode("Definitely not" = "0",
                      "Probably not" = "1",
                      "Probably yes" = "2",
                      "Definitely yes" = "3") %>%
           ff_label("Think cigarette smoking is harmful"),
         
         CR25.factor = factor(CR25) %>%
           fct_recode("None of them" = "0",
                      "Some of them" = "1",
                      "Most of them" = "2",
                      "All of them" = "3") %>%
           ff_label("Friend smoke"),
         
         CR30.factor = factor(CR30) %>%
           fct_recode("0"="0",
                      "1 to 2" = "1",
                      "3 to 4" = "2",
                      "5 to 6" = "3",
                      "7" = "4") %>%
           ff_label("Days people smoked in home"),
         
         CR39.factor = factor(CR39) %>%
           fct_recode("None"="0",
                      "A few"="1",
                      "A lot"="2") %>%
           ff_label("Num of Anti-smoking media"),
         
         CR40.factor = factor(CR40) %>%
           fct_recode("Never participate"="0",
                      "Never"="1",
                      "Sometimes"="2",
                      "A lot" = "3") %>%
           ff_label("Num of Anti-smoking events"),
         
         CR41.factor = factor(CR41) %>%
           fct_recode("Never watch TV"="0",
                      "Never"="1",
                      "Sometimes"="2",
                      "A lot"="3") %>%
           ff_label("See actors smoking"),
         
         CR42.factor = factor(CR42) %>%
           fct_recode("Yes"="1",
                      "No"="0") %>%
           ff_label("Have something with a cigarette brand logo"),
         
         CR43.factor = factor(CR43) %>%
           fct_recode("Never watch TV"="0",
                      "Never"="1",
                      "Sometimes"="2",
                      "A lot"="3") %>%
           ff_label("See cigarette brand names on TV"),
         
         CR44.factor = factor(CR44) %>%
           fct_recode("None"="0",
                      "A few"="1",
                      "A lot"="2") %>%
           ff_label("See advertisements for cigarettes"),
         
         CR45.factor = factor(CR45) %>%
           fct_recode("None"="0",
                      "A few"="1",
                      "A lot"="2") %>%
           ff_label("See advertisements for cigarettes on News"),
         
         VNR56.factor = factor(VNR56) %>%
           fct_recode("None"="0",
                      "A few"="1",
                      "A lot"="2") %>%
           ff_label("See advertisements for cigarettes at points of sale"),
         
         VNR57.factor = factor(VNR57) %>%
           fct_recode("Yes"="1",
                      "No"="0") %>%
           ff_label("Gone to a concert sponsored by a tobacco company"),
         
         CR47.factor = factor(CR47) %>%
           fct_recode("Yes"="1",
                      "No"="0") %>%
           ff_label("a cigarette representative offered you a free cigarette"),
         
         CR48.factor = factor(CR48) %>%
           fct_recode("Yes"="1",
                      "No"="0",
                      "Not sure"="2") %>%
           ff_label("Taught about the dangers of smoking"),
         
         #CR51.factor = factor(CR51) %>%
         #  fct_recode("Never"="0",
         #             "This term"="1",
         #             "Last term"="2",
         #             "2 terms ago"="3",
         #             "3 terms ago"="4",
         #             "More than a year ago"="5") %>%
         # ff_label("Lesson"),
         
         CR53.factor = factor(CR53) %>%
           fct_recode("Male"="1",
                      "Female"="0") %>%
           ff_label("Gender"),
         
         VNR67.factor = factor(VNR67) %>%
           ff_label("Grade"))


#11.3
df %>%
  missing_plot(dependent, explanatory)
#11.4
df %>%
  missing_pattern(dependent, explanatory)
#11.5
df$CR1=as.factor(df$CR1)
df$CR53=as.factor(df$CR53)
df$CR12=as.factor(df$CR12)
df$CR14=as.factor(df$CR14)
df$CR25=as.factor(df$CR25)
df$CR30=as.factor(df$CR30)
#df$CR31=as.factor(df$CR31)
df$CR39=as.factor(df$CR39)
df$CR40=as.factor(df$CR40)
df$CR41=as.factor(df$CR41)
df$CR42=as.factor(df$CR42)
df$CR43=as.factor(df$CR43)
df$CR44=as.factor(df$CR44)
df$CR45=as.factor(df$CR45)
df$CR47=as.factor(df$CR47)
df$CR48=as.factor(df$CR48)
#df$CR51=as.factor(df$CR51)
df$VNR57=as.factor(df$VNR57)
df$VNR56=as.factor(df$VNR56)
df$VNR67=as.factor(df$VNR67)

table1 <- df %>% 
  summary_factorlist(dependent, explanatory, 
                     na_include=TRUE, na_include_dependent = TRUE, 
                     total_col = TRUE, add_col_totals = TRUE, p=TRUE)
df %>% 
  missing_pairs(dependent, explanatory, position = "fill")

missing_mar <- df %>% 
  missing_compare(dependent, explanatory)

explanatory <- c('CR12.factor','CR14.factor','CR24.factor','CR25.factor','CR30.factor',
                 'CR39.factor','CR40.factor','CR41.factor','CR42.factor','CR43.factor',
                 'CR44.factor','CR45.factor','CR47.factor','CR48.factor','CR53.factor',#,'CR51.factor'
                 'VNR56.factor','VNR57.factor','VNR67.factor')
dependent <- 'CR1.factor'

df %>% 
  select(all_of(dependent), all_of(explanatory)) %>% 
  missing_predictorMatrix(
    drop_from_imputed = c("CR1", "VNR67")
  ) -> predM

fits <- df %>% 
  select(all_of(dependent), all_of(explanatory)) %>% 
  
  # Usually run imputation with 10 imputed sets
  mice(m = 1, predictorMatrix = predM)  %>% 
  
  # Run logistic regression on each imputed set
  with(glm(formula(ff_formula(dependent, explanatory)), 
           family="binomial"))
data <- df %>% 
  select(all_of(dependent), all_of(explanatory))
imp <- mice(data,m = 10, predictorMatrix = predM) 
dff <- complete(imp)                          # First 6 rows of our multiply imputed data
dff %>%
  ff_glimpse(dependent, explanatory)

explanatory <- c('CR12.factor','CR14.factor','CR24.factor','CR25.factor','CR30.factor',
                 'CR39.factor','CR40.factor','CR41.factor','CR42.factor','CR43.factor',
                 'CR44.factor','CR45.factor','CR47.factor','CR48.factor','CR53.factor',#,'CR51.factor'
                 'VNR56.factor','VNR57.factor','VNR67.factor')
dependent <- 'CR1.factor'
table1 <- dff %>% 
  summary_factorlist(dependent, explanatory, p=TRUE)
knitr::kable(table1, row.names=FALSE)

library(knitr)
library(flextable)
library(magrittr)
library(xtable)
table1 %>% regulartable() %>% autofit()

print(xtable(table1, type = "latex"), file = "filename3.tex")
results <-
  survey::svydesign(~ 1, data = df) %>%
  tbl_svysummary(
    include = c(CR53.factor, VNR67.factor, CR1.factor),
    statistic = list(all_categorical() ~ "{n_unweighted} ({p}%)")
  )

## AICs
fits %>% 
  getfit() %>% 
  purrr::map(AIC) %>%
  unlist() %>% 
  mean()

# C-statistic
fits %>% 
  getfit() %>% 
  purrr::map(~ pROC::roc(.x$y, .x$fitted)$auc) %>% 
  unlist() %>% 
  mean()

# Pool  results
fits_pool <- fits %>% 
  pool()

## Can be passed to or_plot
dff %>% 
  or_plot(dependent, explanatory, glmfit = fits_pool, table_text_size=4)

# Summarise and put in table
fit_imputed <- fits_pool %>%                                  
  fit2df(estimate_name = "OR (multiple imputation)", exp = TRUE)

# Use finalfit merge methods to create and compare results
explanatory <- c('CR12.factor','CR14.factor','CR24.factor','CR25.factor','CR30.factor',
                 'CR39.factor','CR40.factor','CR41.factor','CR42.factor','CR43.factor',
                 'CR44.factor','CR45.factor','CR47.factor','CR48.factor','CR53.factor',#,'CR51.factor'
                 'VNR56.factor','VNR57.factor','VNR67.factor')

table_uni_multi <- dff %>% 
  finalfit(dependent, explanatory, keep_fit_id = TRUE) 

#8.7
p1 <- df %>% 
  ggplot(aes(x = CR53.factor, fill = CR1.factor)) + 
  geom_bar() + 
  theme(legend.position = "none")

p2 <- df %>% 
  ggplot(aes(x = CR53.factor, fill = CR1.factor)) + 
  geom_bar(position = "fill") + 
  ylab("proportion")

library(patchwork)
p1 + p2

p1 <- meldata %>% 
  ggplot(aes(x = ulcer.factor, fill=status.factor)) + 
  geom_bar(position = position_stack(reverse = TRUE)) +
  facet_grid(sex.factor ~ age.factor) + 
  theme(legend.position = "none")

p2 <- meldata %>% 
  ggplot(aes(x = ulcer.factor, fill=status.factor)) + 
  geom_bar(position = position_fill(reverse = TRUE)) +
  facet_grid(sex.factor ~ age.factor)+ 
  theme(legend.position = "bottom")

p1 / p2

#8.10
df %>% 
  summary_factorlist(dependent   = "status_dss", 
                     explanatory = "ulcer.factor")

#8.13
tbl2 <- dff %>% 
  summary_factorlist(dependent, 
                     explanatory,
                     p = TRUE)
tbl2 %>% regulartable() %>% autofit()

print(xtable(table1, type = "latex"), file = "filename2.tex")
#9.4
library(GGally)
explanatory <- c("ulcer.factor", "age", "sex.factor", 
                 "year", "t_stage.factor")
df %>% 
  remove_labels() %>%  # ggpairs doesn't work well with labels
  ggpairs(columns = explanatory)

select_explanatory <- c("ulcer.factor", "sex.factor", "t_stage.factor")

df %>% 
  select(one_of(explanatory)) %>% 
  pivot_longer(-CR53.factor) %>% 
  ggplot(aes(value, fill = CR53.factor)) + 
  geom_bar(position = "fill") +
  ylab("proportion") +
  facet_wrap(~name, scale = "free", ncol = 2) +
  coord_flip()

dependent <- "mort_5yr"
explanatory <- c("ulcer.factor", "age", "sex.factor", 
                 "year", "t_stage.factor")
df %>% 
  glmmulti(dependent, explanatory) %>%
  car::vif()

#9.7
library(finalfit)
dependent <- "mort_5yr"
explanatory <- "ulcer.factor"
df %>% 
  finalfit(dependent, explanatory, metrics = TRUE)

#9.8




logistf.control(
  maxit = 25,
  maxhs = 0,
  maxstep = 20,
  lconv = 1e-05,
  gconv = 1e-05,
  xconv = 1e-05,
  collapse = TRUE,
  fit = "NR"
)
#
fit2 <- logistf(data = dff, CR1.factor~CR53.factor,pl=TRUE)#+CR14+CR24+CR25+CR30+CR31+CR39+CR40+CR41+CR42+CR43+CR44+CR45+CR47+CR48+CR53+VNR56+VNR57
summary(fit2)

exp(coef(fit2))

df %>%
  summary_factorlist(dependent, explanatory, 
                     p=TRUE, add_dependent_label=TRUE) -> t1
knitr::kable(t1, align=c("l", "l", "r", "r", "r"))

df_c %>%
  finalfit(dependent, explanatory, metrics = TRUE)
knitr::kable(t2[[1]], row.names=FALSE, align=c("l", "l", "r", "r", "r", "r"))
knitr::kable(t2[[2]], row.names=FALSE, col.names="")


df_c <- df_c[, (sapply(df_c, nlevels)>1) | (sapply(df_c, is.factor)==FALSE)]

str(df_c[c(explanatory, dependent)])


