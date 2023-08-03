#BEFORE:
#      1_data_process.R
#      input data (.csv): @data_sdg381, @data_sdg381_rmnch, @data_sdg382
#      2_data_explore.Rmd

#AFTER:
#      model folder

#NOTE:
# response variables: sdg381, sdg381_rmnch, sdg382

# explanatory variables: 
#                       total.aid.for.health, total.aid.for.health.volatility
#                       total.aid.for.health_rmnch, total.aid.for.health.volatility_rmnch
#                       institutional.index

# control variables: gdp.per.capita, government.health.spending, 
#                    prepaid.private.health.spending, out.of.pocket.health.spending, 
#                    health.worker, sdi
# note: health.worker is not included in analysis due to high correlation with sdi (see 2_data_explore)
 

# random variables: country, year

# transformation: 
## response variables: log
## explanatory, control variables: log (except for: institutional.index)

# 1. SETUP -------------------
# load packages
library(tidyverse)   #process dataframe
library(lme4)        # lme model
library(lmerTest)    # lme model test

# load function
## centering function
#c. <- function (x) {(x - mean(x, na.rm = T))} 
## log, centering function
#clog. <- function (x) {(log(x) - mean(log(x), na.rm = T))} 

# load data
dir_data <- "./data"
dir_model <- "./model"

sdg381 <- read_csv(file.path(dir_data, "@data_sdg381.csv"))
sdg381_rmnch <- read_csv(file.path(dir_data, "@data_sdg381_rmnch.csv"))
sdg382 <- read_csv(file.path(dir_data, "@data_sdg382.csv"))

# transform data
## note: prepaid.private.health.spending has 0 -> cannot log -> for 0 value, add 0.0001 
## as the smallest value of total.aid.for.health 

## sdg381
sdg381 <- sdg381 %>% 
  filter(year < 2019) %>%
  mutate(#response variable
         log_sdg381 = log(sdg381),
         # explanatory variable
         log_total.aid = log(total.aid), 
         log_total.aid.volatility = log(total.aid.volatility),
         log_total.aid.for.health = log(total.aid.for.health), 
         log_total.aid.for.health.volatility = log(total.aid.for.health.volatility),
         log_total.aid.for.health_rmnch = log(total.aid.for.health_rmnch), 
         log_total.aid.for.health.volatility_rmnch = log(total.aid.for.health.volatility_rmnch),
         institutional.index = institutional.index,
         # control variable
         log_gdp.per.capita = log(gdp.per.capita), 
         log_government.health.spending = log(government.health.spending), 
         log_prepaid.private.health.spending = if_else(prepaid.private.health.spending == 0,
                                                       log(prepaid.private.health.spending + 0.0001),
                                                       log(prepaid.private.health.spending)),
         log_out.of.pocket.health.spending = log(out.of.pocket.health.spending), 
         #log_health.worker = log(health.worker), 
         log_sdi = log(sdi),
         # random variables
         fyear = factor(year))

## sdg381_rmnch
sdg381_rmnch <- sdg381_rmnch %>% 
  #filter(year < 2019) %>%
  mutate(#response variable
    log_sdg381_rmnch = log(sdg381_rmnch),
    # explanatory variable
    log_total.aid = log(total.aid), 
    log_total.aid.volatility = log(total.aid.volatility),
    log_total.aid.for.health = log(total.aid.for.health), 
    log_total.aid.for.health.volatility = log(total.aid.for.health.volatility),
    log_total.aid.for.health_rmnch = log(total.aid.for.health_rmnch), 
    log_total.aid.for.health.volatility_rmnch = log(total.aid.for.health.volatility_rmnch),
    institutional.index = institutional.index,
    # control variable
    log_gdp.per.capita = log(gdp.per.capita), 
    log_government.health.spending = log(government.health.spending), 
    log_prepaid.private.health.spending = if_else(prepaid.private.health.spending == 0,
                                                  log(prepaid.private.health.spending + 0.0001),
                                                  log(prepaid.private.health.spending)),
    log_out.of.pocket.health.spending = log(out.of.pocket.health.spending), 
    #log_health.worker = log(health.worker), 
    log_sdi = log(sdi),
    # random variables
    fyear = factor(year))

## sdg382
sdg382 <- sdg382 %>% 
  #filter(year < 2019) %>%
  mutate(#response variable
    log_sdg382 = log(sdg382),
    # explanatory variable
    log_total.aid = log(total.aid), 
    log_total.aid.volatility = log(total.aid.volatility),
    log_total.aid.for.health = log(total.aid.for.health), 
    log_total.aid.for.health.volatility = log(total.aid.for.health.volatility),
    log_total.aid.for.health_rmnch = log(total.aid.for.health_rmnch), 
    log_total.aid.for.health.volatility_rmnch = log(total.aid.for.health.volatility_rmnch),
    institutional.index = institutional.index,
    # control variable
    log_gdp.per.capita = log(gdp.per.capita), 
    log_government.health.spending = log(government.health.spending), 
    log_prepaid.private.health.spending = if_else(prepaid.private.health.spending == 0,
                                                  log(prepaid.private.health.spending + 0.0001),
                                                  log(prepaid.private.health.spending)),
    log_out.of.pocket.health.spending = log(out.of.pocket.health.spending), 
    #log_health.worker = log(health.worker), 
    log_sdi = log(sdi),
    # random variables
    fyear = factor(year))

# 2. SDG 3.8.1. ----------------------------

## 2.0. baseline ---------------------------
# data
data_base <- sdg381 %>% filter(is.na(log_sdg381) == F,
                               is.na(log_gdp.per.capita) == F, 
                               is.na(log_government.health.spending) == F, 
                               is.na(log_prepaid.private.health.spending) == F,
                               is.na(log_out.of.pocket.health.spending) == F, 
                               #is.na(log_health.worker) == F, 
                               is.na(log_sdi) == F)

# fit model
sdg381_base <- lmer(log_sdg381 ~ log_gdp.per.capita + log_government.health.spending + 
                      log_prepaid.private.health.spending + log_out.of.pocket.health.spending +
                      log_sdi +
                      (1 | country) + (1 | fyear), data = data_base, REML = T)
summary(sdg381_base)

# save model
write_rds(sdg381_base, file.path(dir_model, "sdg381_base.rds"))

## 2.1. total aid for health ---------------
# data
data_total.aid.for.health <- sdg381 %>% filter(is.na(log_sdg381) == F,
                                               is.na(log_total.aid.for.health) == F,
                                               is.na(institutional.index) == F,
                                               is.na(log_gdp.per.capita) == F, 
                                               is.na(log_government.health.spending) == F, 
                                               is.na(log_prepaid.private.health.spending) == F,
                                               is.na(log_out.of.pocket.health.spending) == F, 
                                               #is.na(log_health.worker) == F, 
                                               is.na(log_sdi) == F)

# fit model
sdg381_total.aid.for.health <- lmer(log_sdg381 ~ log_total.aid.for.health*institutional.index +
                                      log_gdp.per.capita*log_total.aid.for.health + log_government.health.spending + 
                                      log_prepaid.private.health.spending + log_out.of.pocket.health.spending +
                                      log_sdi +
                                      (1 | country) + (1 | fyear), data = data_total.aid.for.health, REML = T)
summary(sdg381_total.aid.for.health)

# save model
write_rds(sdg381_total.aid.for.health, file.path(dir_model, "sdg381_total.aid.for.health.rds"))

## 2.2. total aid for health volatility ----
# data
data_total.aid.for.health.vol <- sdg381 %>% filter(is.na(log_sdg381) == F,
                                                   is.na(log_total.aid.for.health.volatility) == F,
                                                   is.na(log_total.aid.for.health) == F,
                                                   is.na(institutional.index) == F,
                                                   is.na(log_gdp.per.capita) == F, 
                                                   is.na(log_government.health.spending) == F, 
                                                   is.na(log_prepaid.private.health.spending) == F,
                                                   is.na(log_out.of.pocket.health.spending) == F, 
                                                   #is.na(log_health.worker) == F, 
                                                   is.na(log_sdi) == F)

# fit model
sdg381_total.aid.for.health.vol <- lmer(log_sdg381 ~ log_total.aid.for.health*institutional.index +
                                          log_total.aid.for.health.volatility*institutional.index +
                                          log_gdp.per.capita*log_total.aid.for.health + log_government.health.spending + 
                                          log_prepaid.private.health.spending + log_out.of.pocket.health.spending +
                                          log_sdi +
                                          (1 | country) + (1 | fyear), data = data_total.aid.for.health.vol, REML = T)
summary(sdg381_total.aid.for.health.vol)

# save model
write_rds(sdg381_total.aid.for.health.vol, file.path(dir_model, "sdg381_total.aid.for.health.vol.rds"))

## 2.3. total aid for health - rmnch -------
# data
data_total.aid.for.health_rmnch <- sdg381_rmnch %>% filter(#economic.group == "Quintile 1 (poorest)",
                                                           is.na(log_sdg381_rmnch) == F,
                                               is.na(log_total.aid.for.health_rmnch) == F,
                                               is.na(institutional.index) == F,
                                               is.na(log_gdp.per.capita) == F, 
                                               is.na(log_government.health.spending) == F, 
                                               is.na(log_prepaid.private.health.spending) == F,
                                               is.na(log_out.of.pocket.health.spending) == F, 
                                               #is.na(log_health.worker) == F, 
                                               is.na(log_sdi) == F)

# fit model
sdg381_total.aid.for.health_rmnch <- lmer(log_sdg381_rmnch ~ log_total.aid.for.health_rmnch*institutional.index +
                                            log_total.aid.for.health_rmnch*economic.group +
                                      log_gdp.per.capita*log_total.aid.for.health_rmnch + log_government.health.spending + 
                                      log_prepaid.private.health.spending + log_out.of.pocket.health.spending +
                                      log_sdi +
                                      (1 | country) + (1 | fyear), data = data_total.aid.for.health_rmnch, REML = T)
summary(sdg381_total.aid.for.health_rmnch)

# save model
write_rds(sdg381_total.aid.for.health_rmnch, file.path(dir_model, "sdg381_total.aid.for.health_rmnch.rds"))

## 2.4. total aid for health volatility - rmnch -------
# data
data_total.aid.for.health.vol_rmnch <- sdg381_rmnch %>% filter(#economic.group == "Quintile 1 (poorest)",
                                                           is.na(log_sdg381_rmnch) == F,
                                                           is.na(log_total.aid.for.health.volatility_rmnch) == F,
                                                           is.na(log_total.aid.for.health_rmnch) == F,
                                                           is.na(institutional.index) == F,
                                                           is.na(log_gdp.per.capita) == F, 
                                                           is.na(log_government.health.spending) == F, 
                                                           is.na(log_prepaid.private.health.spending) == F,
                                                           is.na(log_out.of.pocket.health.spending) == F, 
                                                           #is.na(log_health.worker) == F, 
                                                           is.na(log_sdi) == F)

# fit model
sdg381_total.aid.for.health.vol_rmnch <- lmer(log_sdg381_rmnch ~ log_total.aid.for.health_rmnch*institutional.index +
                                                log_total.aid.for.health_rmnch*economic.group +
                                                log_total.aid.for.health.volatility_rmnch*institutional.index +
                                                log_total.aid.for.health.volatility_rmnch*economic.group +
                                            log_gdp.per.capita*log_total.aid.for.health_rmnch + log_government.health.spending + 
                                            log_prepaid.private.health.spending + log_out.of.pocket.health.spending +
                                            log_sdi +
                                            (1 | country) + (1 | fyear), data = data_total.aid.for.health.vol_rmnch, REML = T)
summary(sdg381_total.aid.for.health.vol_rmnch)

# save model
write_rds(sdg381_total.aid.for.health.vol_rmnch, file.path(dir_model, "sdg381_total.aid.for.health.vol_rmnch.rds"))

# 3. SDG 3.8.2 -------------
## 3.0. baseline ---------------------------
# data
data_base <- sdg382 %>% filter(is.na(log_sdg382) == F,
                               is.na(log_gdp.per.capita) == F, 
                               is.na(log_government.health.spending) == F, 
                               is.na(log_prepaid.private.health.spending) == F,
                               is.na(log_out.of.pocket.health.spending) == F, 
                               #is.na(log_health.worker) == F, 
                               is.na(log_sdi) == F)

# fit model
sdg382_base <- lmer(log_sdg382 ~ log_gdp.per.capita + log_government.health.spending + 
                      log_prepaid.private.health.spending + log_out.of.pocket.health.spending +
                      log_sdi +
                      (1 | country) + (1 | fyear), data = data_base, REML = T)
summary(sdg382_base)

# save model
write_rds(sdg382_base, file.path(dir_model, "sdg382_base.rds"))

## 3.1. total aid for health ---------------
# data
data_total.aid.for.health <- sdg382 %>% filter(is.na(log_sdg382) == F,
                                               is.na(log_total.aid.for.health) == F,
                                               is.na(institutional.index) == F,
                                               is.na(log_gdp.per.capita) == F, 
                                               is.na(log_government.health.spending) == F, 
                                               is.na(log_prepaid.private.health.spending) == F,
                                               is.na(log_out.of.pocket.health.spending) == F, 
                                               #is.na(log_health.worker) == F, 
                                               is.na(log_sdi) == F)

# fit model
sdg382_total.aid.for.health <- lmer(log_sdg382 ~ log_total.aid.for.health*institutional.index +
                                      log_gdp.per.capita*log_total.aid.for.health + log_government.health.spending + 
                                      log_prepaid.private.health.spending + log_out.of.pocket.health.spending +
                                      log_sdi +
                                      (1 | country) + (1 | fyear), data = data_total.aid.for.health, REML = T)
summary(sdg382_total.aid.for.health)

# save model
write_rds(sdg382_total.aid.for.health, file.path(dir_model, "sdg382_total.aid.for.health.rds"))

## 3.2. total aid for health volatility ----
# data
data_total.aid.for.health.vol <- sdg382 %>% filter(is.na(log_sdg382) == F,
                                                   is.na(log_total.aid.for.health.volatility) == F,
                                                   is.na(log_total.aid.for.health) == F,
                                                   is.na(institutional.index) == F,
                                                   is.na(log_gdp.per.capita) == F, 
                                                   is.na(log_government.health.spending) == F, 
                                                   is.na(log_prepaid.private.health.spending) == F,
                                                   is.na(log_out.of.pocket.health.spending) == F, 
                                                   #is.na(log_health.worker) == F, 
                                                   is.na(log_sdi) == F)

# fit model
sdg382_total.aid.for.health.vol <- lmer(log_sdg382 ~ log_total.aid.for.health*institutional.index +
                                          log_total.aid.for.health.volatility*institutional.index +
                                          log_gdp.per.capita*log_total.aid.for.health + log_government.health.spending + 
                                          log_prepaid.private.health.spending + log_out.of.pocket.health.spending +
                                          log_sdi +
                                          (1 | country) + (1 | fyear), data = data_total.aid.for.health.vol, REML = T)
summary(sdg382_total.aid.for.health.vol)

# save model
write_rds(sdg382_total.aid.for.health.vol, file.path(dir_model, "sdg382_total.aid.for.health.vol.rds"))




