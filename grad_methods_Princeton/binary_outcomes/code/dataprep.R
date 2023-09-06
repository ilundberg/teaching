
library(foreign)
library(tidyverse)
library(Amelia)

d <- read.dta("/Users/iandl/Documents/ff_pub_merge2.dta",
              convert.factors = F) %>%
  mutate(
    ## Primary caregiver at each wave
    m2p = m2a3 %in% c(1,2), # child lives with mother at least half the time at wave 2
    f2p = f2a3 %in% c(1,2), # child lives with father at least half the time at wave 2

    ## Participation of each parent at each wave
    m2int = m2a3 != -9,
    f2int = f2a3 != -9,
      
    ## Each parents' report of eviction in each wave
    m2ev = ifelse(m2h19e == 1, T,
                  ifelse(m2h19e == 2, F, NA)),
    f2ev = ifelse(f2h17e == 1, T,
                  ifelse(f2h17e == 2, F, NA)),
    
    ## Whether child experience eviction each wave, using one report only,
    ## replacing with 0 if child has ever been reported deceased
    evicted = ifelse(cm2samp == 2, F,
                     ifelse(m2p == 1, m2ev, ifelse(f2p == 1, f2ev, NA))),
    income = ifelse(m2p == 1, cm2povco,
                    ifelse(f2p == 1, cf2povco, NA)),
    income = ifelse(income <= -1, NA,
                    ifelse(income >= 5, 5, income)),
    married = ifelse(cm1relf < 0, NA,
                     cm1relf == 1),
    race = factor(ifelse(cm1ethrace < 0, NA, cm1ethrace),
                  labels = c("White","Black","Hispanic","Other"))
  ) %>%
  select(idnum, evicted, income, married, race, m1natwt) %>%
  filter(!is.na(m1natwt)) %>%
  ## There is only 1 person with missing marital status
  ## Fill in as unmarried to avoid confusion in precept
  mutate(married = ifelse(is.na(married), F, married))

## Singly impute for the sake of precept, since they don't
## know multiple imputation yet
filled <- amelia(d,
                 m = 1,
                 idvars = c("idnum","m1natwt"),
                 noms = c("evicted","married","race"),
                 seed = 08544)$imputations[[1]] %>%
  mutate(income = ifelse(income < 0, 0,
                         ifelse(income > 5, 5, income)))

write.dta(filled,
          file = "/Users/iandl/Dropbox/SOC504/2018 materials/Precept/Precept4/data/ffEviction.dta")  


