library(tidyverse)
library(knitr)
aids_df <- read_csv("Data/aids.csv") %>%
  mutate(
    cid = factor(cid),
    trt = factor(trt, levels = 0:3, labels = c("ZDV only", "ZDV + ddI", "ZDV + Zal", "ddI only")),
    hemo = factor(hemo, levels = 0:1, labels = c("no", "yes")),
    homo = factor(homo, levels = 0:1, labels = c("no", "yes")),
    drugs = factor(drugs, levels = 0:1, labels = c("no", "yes")),
    oprior = factor(oprior, levels = 0:1, labels = c("no", "yes")),
    z30 = factor(z30, levels = 0:1, labels = c("no", "yes")),
    zprior = factor(zprior, levels = 0:1, labels = c("no", "yes")),
    race = factor(race, levels = 0:1, labels = c("white", "non-white")),
    gender = factor(gender, levels = 0:1, labels = c("F", "M")),
    str2 = factor(str2, levels = 0:1, labels = c("naive", "experienced")),
    strat = factor(strat, levels = 1:3, labels = c("Antiretroviral Naive", "> 1 but <= 52 weeks of prior antiretroviral therapy", "> 52 weeks")),
    symptom = factor(symptom, levels = 0:1, labels = c("asymp", "symp")),
    treat = factor(treat, levels = 0:1, labels = c("ZDV only", "others")),
    offtrt = factor(offtrt, levels = 0:1, labels = c("no", "yes"))
  )
aids_df %>%
  filter(cid == 1) %>%
  ggplot(mapping = aes(x = str2, y = time)) +
  geom_boxplot()

aids_df %>%
  group_by(trt, cid) %>%
  count() %>%
  spread(cid, n) %>%
  kable()
