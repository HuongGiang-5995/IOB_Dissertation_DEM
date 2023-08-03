#BEFORE:
#      all datasets (folders) in ./data

#AFTER:
#      component data (.csv): 
#         explanatory: country, gbd, ihme, oecd, wb
#         response: sdg381, sdg381_rmnch, sdg382
#      analysis data (.csv): 
#         @data_sdg381, @data_sdg381_rmnch, @data_sdg382

# 1. SETUP -------------------
library(tidyverse)   #process dataframe
library(haven)       #read dta stata file
library(psych)       #pca analysis
library(readxl)      #read xlsx
library(zoo)         #create timeseries
library(fGarch)      #fit GARCH model
library(rugarch)     #fit GARCH model

dir_data <- "./data"

# 2. PROCESS EXPLANATORY DATA ------------
## 2.1. COUNTRY --------------

# info needed:
# country_un - sdg region
# country_wb - income class
# country_gbd - location id (to join GBD data)

# country_un
## workflow
## 1. remove Anarctica
## 2. create UN SDG regions
## 3. create short column name
## 4. select important columns
country_un <- read_xlsx(file.path(dir_data, "country data", "UN_country.xlsx"))
country_un <- country_un %>% 
  filter(`Country or Area` != "Antarctica") %>%
  mutate(region_un = if_else(`Sub-region Code` %in% c(15, 145), "Northern Africa and Western Asia",
                                                  if_else(`Sub-region Code` %in% c(202), "Sub-Saharan Africa",
                                                          if_else(`Sub-region Code` %in% c(34, 143), "Central and Southern Asia",
                                                                  if_else(`Sub-region Code` %in% c(30, 35), "Eastern and South-eastern Asia",
                                                                          if_else(`Sub-region Code` %in% c(419), "Latin America and the Caribbean",
                                                                                  if_else(`Sub-region Code` %in% c(53), "Australia and New Zealand",
                                                                                          if_else(`Sub-region Code` %in% c(151, 154, 39, 155, 21), "Europe and Northern America",
                                                                                                  "Oceania")))))))) %>%
  mutate(country = `Country or Area`,
         country.code = `ISO-alpha3 Code`) %>%
  select(country.code, region_un)

# country_wb
## workflow
## 1. filter low, lower middle income groups
## 2. create short column names
## 3. select important columns
country_wb <- read_xlsx(file.path(dir_data, "country data", "WB_country.xlsx"))
country_wb <- country_wb %>% 
  filter(`Income group` %in% c("Low income", "Lower middle income")) %>%
  mutate(country = Economy,
         country.code = Code,
         income.group_wb = `Income group`) %>%
  select(country, country.code, income.group_wb)

# country gbd
## workflow
## 1. filter only countries (level = 3)
## 2. create short column names
## 3. select important columns
country_gbd <- read_xlsx(file.path(dir_data, "country data", "IHME_GBD_2019_country.XLSX"))
country_gbd <- country_gbd %>% 
  filter(Level == 3) %>%
  mutate(country = `Location Name`,
         country.id_gbd = `Location ID`) %>%
  select(country, country.id_gbd)

## mismatch in country name country_wb and country_gbd 
#[1] "Bolivia"                   "Côte d’Ivoire"             "Congo, Dem. Rep."          "Congo, Rep."              
#[5] "Egypt, Arab Rep."          "Micronesia, Fed. Sts."     "Gambia, The"               "Iran, Islamic Rep."       
#[9] "Kyrgyz Republic"           "Lao PDR"                   "Korea, Dem. People's Rep." "West Bank and Gaza"       
#[13] "São Tomé and Príncipe"     "Tanzania"                  "Vietnam"                   "Yemen, Rep."  
country_miss <- tibble(country.id_gbd = c(121, 205, 171, 170,
                          141, 25, 206, 142,
                          37, 12, 7, 
                          215, 189, 20, 157),
                       country = c("Bolivia", "Côte d’Ivoire", "Congo, Dem. Rep.", "Congo, Rep.",
                                   "Egypt, Arab Rep.", "Micronesia, Fed. Sts.", "Gambia, The", "Iran, Islamic Rep.",
                                   "Kyrgyz Republic", "Lao PDR", "Korea, Dem. People's Rep.",
                                   "São Tomé and Príncipe", "Tanzania", "Vietnam", "Yemen, Rep."))
## change country name of mismatch countries
country_gbd <- country_gbd %>% 
  filter(!country.id_gbd %in% country_miss$country.id_gbd) %>%
  bind_rows(country_miss)

# merge data
## workflow
## 1. join country_wb and country_un
## 2. join country_gbd
## 3. remove countries without gbd id (no GBD data)
country_list <- left_join(country_wb, country_un, by = "country.code")
country_list <- left_join(country_list, country_gbd, by = "country")
country_list <- country_list %>% 
  filter(is.na(country.id_gbd) == F) %>%
  arrange(country)

## save data
write_csv(country_list, file.path(dir_data, "country.csv"))
country_list <- read_csv(file.path(dir_data, "country.csv"))

## 2.2. WORLD BANK WDI -----------
## Note:
## no data for Korea, Dem. People's Rep.
## GDP (constant USD 2015, power: million)

## workflow
## 1. create year column: pivot longer
## 2. create year column: remove character
## 3. adjust value: convert character to numeric
## 4. adjust value: round by 2 decimal
## 5. create important columns: pivot wider 1
## 6. create important columns: pivot wider 2
## 7. keep only studied countries with correct country names
## 8. create short column names, convert gdp to million usd
## 9. select important columns

wb <- read_csv(file.path(dir_data, "WB_WDI", "WB_WDI.csv"))
wb <- wb[1:1064,] %>% 
  pivot_longer(cols = 5:67, names_to = "year") %>% 
  mutate(year = as.numeric(substr(year, 1, 4))) %>% 
  mutate(value = as.numeric(if_else(value == "..", "", value))) %>%
  mutate(value = round(value, 2)) %>%
  select(!`Series Code`) %>% 
  pivot_wider(names_from = `Series Name`, values_from = value) %>%
  inner_join(country_list, by = c("Country Code" = "country.code")) %>%
  mutate(country.code = `Country Code`,
         gini.index = `Gini index`,
         people.at.poverty = `Poverty headcount ratio at national poverty lines (% of population)`,
         gdp.per.capita = `GDP per capita (constant 2015 US$)`,
         gdp = `GDP (constant 2015 US$)`/1000000) %>%
  select(country, country.code, year, 
         gini.index, people.at.poverty, gdp.per.capita, gdp)

## save data
write_csv(wb, file.path(dir_data, "wb_wdi.csv"), na = "")

## 2.3. WGI ---------------------------------------------------------------------

## workflow
## 1. create short column name
## 2. select importatn columns
wgi <- read_dta(file.path(dir_data, "WGI_institutional quality", "wgidataset.dta"))
wgi <- wgi %>% mutate(country = countryname,
                      country.code = code) %>%
  select(country, country.code, year, vae, pve, gee, rqe, rle, cce)

# 3. create institutional.index
## new wgi data without NA values
wgi_1 <- wgi %>% filter(is.na(vae) == F & is.na(pve) == F & is.na(gee) == F & is.na(rqe) == F & is.na(rle) == F & is.na(cce) == F)
PCA_results <- principal(select(wgi_1, vae:cce), nfactors = 1)
### proportion variance explained by the index: 85% (see PC1 in PCA_results)
wgi_1 <- wgi_1 %>% 
  mutate(institutional.index = as.vector(PCA_results$scores)) %>% 
  select(country, country.code, year, institutional.index)

## merge institutional.index to wgi
wgi <- left_join(wgi, wgi_1 , by = c("country", "country.code", "year"))

## save data
write_csv(wgi, file.path(dir_data, "wgi.csv"), na = "")

## 2.4. OECD total aid ----------------
### 2.4.1. total aid ------------------
# Disbursement data
## Note data: 
### Aid type: Memo: ODA Total, Gross disbursements (then divide by GDP constant 2015 USD from wb_wdi)
### Amount type: Constant prices (using reference year 2021)
### Unit: Million US dollars

oecd <- read_csv(file.path(dir_data, "OECD_foreign aid disbursement", "OECD_TABLE2A_disbursement.csv"))

# wb gdp data
wb <- read_csv(file.path(dir_data, "wb_wdi.csv"))
wb_gdp <- wb %>% select(country, country.code, year, gdp)

# mismatched countries
country_miss <- tibble(country_oecd = c("Democratic Republic of the Congo", "Congo", "Côte d'Ivoire", 
                                        "Egypt", "Gambia", "Iran", 
                                        "Kyrgyzstan", "Lao People's Democratic Republic", "Democratic People's Republic of Korea",
                                        "Micronesia", "Sao Tome and Principe", "Viet Nam", "Yemen"),
                       country = c("Congo, Dem. Rep.", "Congo, Rep.", "Côte d’Ivoire", 
                                   "Egypt, Arab Rep.",  "Gambia, The", "Iran, Islamic Rep.",
                                   "Kyrgyz Republic", "Lao PDR", "Korea, Dem. People's Rep.",
                                   "Micronesia, Fed. Sts.", "São Tomé and Príncipe", "Vietnam", "Yemen, Rep."))

## workflow:
## 1. correct country name: add mismatch names 
## 2. correct country name: add matched names
## 3. filter country as in country_list
## 4. join country_list to have country.code
## 5. filter `Aid type` and Donor
## 6. create short names
## 7. add gdp from wb data
## 8. calculate total.aid (per gdp)
## 9. select important columns

oecd <- oecd %>% 
  left_join(country_miss, by = c("Recipient" = "country_oecd")) %>% 
  mutate(country = if_else(is.na(country) == T, Recipient, country)) %>%
  filter(country %in% country_list$country) %>%
  left_join(country_list) %>%
  filter(`Aid type` == "Memo: ODA Total, Gross disbursements",
         `Amount type` == "Constant Prices",
         Donor == "Official Donors, Total") %>%
  rename(total.aid = Value,
         year = Year) %>%
  left_join(wb_gdp, by = c("country", "country.code", "year")) %>%
  mutate(total.aid = total.aid/(gdp)*100) %>%
  select(country, country.code, year, total.aid) 

## save data
write_csv(oecd, file.path(dir_data, "oecd_total.aid.csv"), na = "")

### 2.4.2. total aid volatility ---------------------

# Note:
## model formula: Boateng et al., 2021; 
### and "simplest (GARCH) model" https://blog.devgenius.io/volatility-modeling-with-r-arch-and-garch-models-11fde2d7ac38
## model fitting: section 5.2.3. GARCH in R https://faculty.baruch.cuny.edu/smanzan/FINMETRICS/_book/volatility-models.html

## Model: AR(1)-GARCH(1,1): AR(1) in the conditional mean 
## rugarch: armarOrder = c(1,0)
## fGarch: arma(1,0)

oecd <- read_csv(file.path(dir_data, "oecd_total.aid.csv"))

## create country list
oecd_country <- sort(unique(filter(oecd, is.na(total.aid) == F)$country)) 

## calculate aid volatility
oecd_total.aid.vol <- tibble()
for (i in 1:length(oecd_country)) {
  # print progress
  print(paste0("processing country ", oecd_country[i]))
  
  # extract country
  aid <- oecd %>% filter(country == oecd_country[i], is.na(total.aid) == F)
  aid.z <- zoo(x=aid$total.aid, order.by=aid$year)
  
  
  #fit GARCH model (rugarch)
  spec = ugarchspec(variance.model=list(garchOrder=c(1,1)),
                    mean.model=list(armaOrder=c(1,0)),
                    distribution.model="norm",)
  fit <- ugarchfit(data=aid.z,spec=spec)
  ##summary of GARCH fit
  fit_sum <- as.data.frame(round(fit@fit$matcoef, 3))
  
  # if model converges: continue; else: fit with fGarch
  if (fit_sum$` Estimate`[3] != 0 & fit_sum$` Estimate`[4] != 0) {
    
    # extract volatility
    aid.vol <- tibble(country = aid$country,
                      country.code = aid$country.code,
                      year = aid$year,
                      total.aid.volatility = as.numeric(sigma(fit)),
                      package = "rugarch")  
  } else {
    
    # fit GARCH model (fGarch)
    fit <- garchFit(~ arma(1,0) + garch(1,1), data=aid.z, trace=FALSE)
    
    # check if model is convergent
    ## if fail to converge, then skip
    fit_sum <- as.data.frame(round(fit@fit$matcoef, 3))
    
    # if model converges: continue; else: next
    if (fit_sum$` Estimate`[3] != 0 & fit_sum$` Estimate`[4] != 0) {
      
      # extract volatility
      aid.vol <- tibble(country = aid$country,
                        country.code = aid$country.code,
                        year = aid$year,
                        total.aid.volatility = fit@sigma.t,
                        package = "fGarch")  # class is numeric
    } 
    
    else {
      print("  model fails to converge")
      next
    }
    
  }
  
  # compile aid.volatility 
  oecd_total.aid.vol <- bind_rows(oecd_total.aid.vol, aid.vol)
  
}

## save data
write_csv(oecd_total.aid.vol, file.path(dir_data, "oecd_total.aid.vol.csv"), na = "")

## 2.5. IHME GBD -------------
### 2.5.1. human resources for health -------------

# Note: All health workers (workers per 10,000 population)

# workflow:
# 1. filter "All health workers"
# 2. keep only studied countries and add correct country name
# 3. create short name
# 4. select important columns

gbd_hrh <- read_csv(file.path(dir_data, "IHME_GBD_2019_human resources for health", "IHME_GBD_2019_HRH_1990_2019.CSV"))
gbd_hrh <- gbd_hrh %>% 
  filter(cadre == "All health workers") %>%
  inner_join(country_list, by = c("location_id" = "country.id_gbd")) %>%
  rename(year = year_id,
         health.worker = mean) %>%
  select(country, country.code, year, health.worker)

## save data
write_csv(gbd_hrh, file.path(dir_data, "gbd_hrh.csv"), na = "")

### 2.5.2. social demographic index -------------

# better country name format in xlsx file but better value format in csv file
gbd_sdi_50_69_xlsx <- read_xlsx(file.path(dir_data, "IHME_GBD_2019_social demographic index", "IHME_GBD_2019_SDI_1950_1969.xlsx"))
gbd_sdi_50_69_csv <- read_csv(file.path(dir_data, "IHME_GBD_2019_social demographic index", "IHME_GBD_2019_SDI_1950_1969.csv"))
gbd_sdi_50_69 <- bind_cols(gbd_sdi_50_69_xlsx[1], gbd_sdi_50_69_csv[-1])

gbd_sdi_70_89_xlsx <- read_xlsx(file.path(dir_data, "IHME_GBD_2019_social demographic index", "IHME_GBD_2019_SDI_1970_1989.xlsx"))
gbd_sdi_70_89_csv <- read_csv(file.path(dir_data, "IHME_GBD_2019_social demographic index", "IHME_GBD_2019_SDI_1970_1989.csv"))
gbd_sdi_70_89 <- bind_cols(gbd_sdi_70_89_xlsx[1], gbd_sdi_70_89_csv[-1])

gbd_sdi_90_19_xlsx <- read_xlsx(file.path(dir_data, "IHME_GBD_2019_social demographic index", "IHME_GBD_2019_SDI_1990_2019.xlsx"))
gbd_sdi_90_19_csv <- read_csv(file.path(dir_data, "IHME_GBD_2019_social demographic index", "IHME_GBD_2019_SDI_1990_2019.csv"))
gbd_sdi_90_19 <- bind_cols(gbd_sdi_90_19_xlsx[1], gbd_sdi_90_19_csv[-1])

# mistmatched country names
country_gbd <- read_xlsx(file.path(dir_data, "country data", "IHME_GBD_2019_country.XLSX"))
country_miss <- tibble(country_sdi = c("Bolivia", "DR Congo", "Congo (Brazzaville)",
                                       "Cape Verde", "Federated States of Micronesia", "The Gambia",
                                       "Iran", "Laos", "North Korea",
                                       "São Tomé and PrÍncipe", "eSwatini", "Syria", 
                                       "Tanzania", "Vietnam"),
                       country_gbd = c("Bolivia (Plurinational State of)", "Democratic Republic of the Congo", "Congo",
                                       "Cabo Verde", "Micronesia (Federated States of)", "Gambia", 
                                       "Iran (Islamic Republic of)", "Lao People's Democratic Republic", "Democratic People's Republic of Korea", 
                                       "Sao Tome and Principe", "Eswatini", "Syrian Arab Republic", 
                                       "United Republic of Tanzania","Viet Nam"))


# workflow:
# 1. filter "All health workers"
# 2. keep only studied countries and add correct country name
# 3. create short name
# 4. select important columns
# better country name format in xlsx file but better value format in csv file

gbd_sdi <- bind_cols(gbd_sdi_50_69, gbd_sdi_70_89[-1], gbd_sdi_90_19[-1]) %>%
  pivot_longer(cols = 2:71,
               names_to = "year",
               values_to = "sdi") %>%
  mutate(year = as.numeric(year)) %>%
  left_join(country_miss, by = c("Location" = "country_sdi")) %>%
  mutate(country_gbd = if_else(is.na(country_gbd) == T, Location, country_gbd)) %>% 
  inner_join(country_gbd, by = c("country_gbd" = "Location Name")) %>%
  inner_join(country_list, by = c("Location ID" = "country.id_gbd")) %>%
  select(country, country.code, year, sdi)

## save data
write_csv(gbd_sdi, file.path(dir_data, "gbd_sdi.csv"), na = "")

## 2.6. IHME health finance -------------------
### 2.6.1. global health spending ------------

# wb gdp data
wb <- read_csv(file.path(dir_data, "wb_wdi.csv"))
wb_gdp <- wb %>% select(country, country.code, year, gdp)

# workflow:
# 1. filter studied countries
# 2. add country.code
# 3. add gdp from wb data
# 4. convert ppp_total_mean, dah_total_mean to numeric
# 5. calculate spending (per gdp, per the) (short name)
# 6. select important columns

the <- read_csv(file.path(dir_data, "IHME_global health spending", "IHME_HEALTH_SPENDING_1995_2019.CSV"))
the <- the %>% 
  filter(location_id %in% country_list$country.id_gbd) %>%
  left_join(country_list, by = c("location_id" = "country.id_gbd")) %>%
  left_join(wb_gdp, by = c("country", "country.code", "year")) %>%
  mutate(ppp_total_mean = as.numeric(ppp_total_mean),
         dah_total_mean = as.numeric(dah_total_mean)) %>%
  mutate(# health spending per gdp
         total.health.spending = (the_total_mean*1000)/(gdp*1000000)*100,
         government.health.spending = (ghes_total_mean*1000)/(gdp*1000000)*100,
         prepaid.private.health.spending = (ppp_total_mean*1000)/(gdp*1000000)*100,
         out.of.pocket.health.spending = (oop_total_mean*1000)/(gdp*1000000)*100,
         total.aid.for.health = (dah_total_mean*1000)/(gdp*1000000)*100,
         # health spending total
         the_total = the_total_mean,
         ghes_total = ghes_total_mean,
         ppp_total = ppp_total_mean,
         oop_total = oop_total_mean,
         dah_total = dah_total_mean,
         # health spending per total health spending
         ghes_per_the = ghes_per_the_mean*100,
         ppp_per_the = ppp_per_the_mean*100,
         oop_per_the = oop_per_the_mean*100,
         dah_per_the = dah_per_the_mean*100) %>%
  select(country, country.code, year,
         total.health.spending:dah_per_the)  
  
## save data
write_csv(the, file.path(dir_data, "ihme_the.csv"), na = "")

### 2.6.2. global health spending - dah volatility ------------

the <- read_csv(file.path(dir_data, "ihme_the.csv"))

## create country list
the_country <- sort(unique(filter(the, is.na(total.aid.for.health) == F)$country)) 

## calculate aid volatility
the_dah.vol <- tibble()
for (i in 1:length(the_country)) {
  # print progress
  print(paste0("processing country ", the_country[i]))
  
  # extract country
  aid <- the %>% filter(country == the_country[i], is.na(total.aid.for.health) == F)
  aid.z <- zoo(x=aid$total.aid.for.health, order.by=aid$year)
  
  
  #fit GARCH model (rugarch)
  spec = ugarchspec(variance.model=list(garchOrder=c(1,1)),
                    mean.model=list(armaOrder=c(1,0)),
                    distribution.model="norm",)
  fit <- ugarchfit(data=aid.z,spec=spec)
  ##summary of GARCH fit
  fit_sum <- as.data.frame(round(fit@fit$matcoef, 3))
  
  # if model converges: continue; else: fit with fGarch
  if (fit_sum$` Estimate`[3] != 0 & fit_sum$` Estimate`[4] != 0) {
    
    # extract volatility
    aid.vol <- tibble(country = aid$country,
                      country.code = aid$country.code,
                      year = aid$year,
                      total.aid.for.health.volatility = as.numeric(sigma(fit)),
                      package = "rugarch")  
  } else {
    
    # fit GARCH model (fGarch)
    fit <- garchFit(~ arma(1,0) + garch(1,1), data=aid.z, trace=FALSE)
    
    # check if model is convergent
    ## if fail to converge, then skip
    fit_sum <- as.data.frame(round(fit@fit$matcoef, 3))
    
    # if model converges: continue; else: next
    if (fit_sum$` Estimate`[3] != 0 & fit_sum$` Estimate`[4] != 0) {
      
      # extract volatility
      aid.vol <- tibble(country = aid$country,
                        country.code = aid$country.code,
                        year = aid$year,
                        total.aid.for.health.volatility = fit@sigma.t,
                        package = "fGarch")  # class is numeric
    } 
    
    else {
      print("  model fails to converge")
      next
    }
    
  }
  
  # compile aid.volatility 
  the_dah.vol <- bind_rows(the_dah.vol, aid.vol)
  
}

## save data
write_csv(the_dah.vol, file.path(dir_data, "ihme_the.vol.csv"), na = "")

### 2.6.3. aid for reproductive, marternal, neo and child health (RMNCH)  ------------

# wb gdp data
wb <- read_csv(file.path(dir_data, "wb_wdi.csv"))
wb_gdp <- wb %>% select(country, country.code, year, gdp)

# workflow:
# 1. filter studied countries
# 2. add country.code
# 3. convert data to numeric
# 4. summarize total.aid: group_by
# 5. summarize total.aid: summarize
# 6. add gdp from wb data
# 7. calculate total.aid.for.health_rmnch (per gdp) (short name)
# 8. select important columns
# 9. arrange by country, year

dah <- read_csv(file.path(dir_data, "IHME_global development assisstance for health", "IHME_DAH_DATABASE_1990_2021.CSV"))
dah_rmnch <- dah %>% 
  filter(gbd_location_id %in% country_list$country.id_gbd) %>%
  left_join(country_list, by = c("gbd_location_id" = "country.id_gbd")) %>%
  mutate(rmh_dah_21 = as.numeric(rmh_dah_21),
         nch_dah_21 = as.numeric(nch_dah_21)) %>%
  group_by(country, country.code, year) %>%
  summarize(rmh_dah_21 = sum(rmh_dah_21, na.rm = T),
            nch_dah_21 = sum(nch_dah_21, na.rm = T)) %>%
  left_join(wb_gdp, by = c("country", "country.code", "year")) %>%
  mutate(total.aid.for.health_rmnch = ((rmh_dah_21 + nch_dah_21)*1000)/(gdp*1000000)*100) %>%
  select(country, country.code, year, total.aid.for.health_rmnch) %>%
  arrange(country, year)
 
## save data
write_csv(dah_rmnch, file.path(dir_data, "ihme_dah_rmnch.csv"))

### 2.6.4. aid for reproductive, marternal, neo and child health (RMNCH) - dah volatility -----------------------

dah_rmnch <- read_csv(file.path(dir_data, "ihme_dah_rmnch.csv"))

## create country list
dah_rmnch_country <- sort(unique(filter(dah_rmnch, is.na(total.aid.for.health_rmnch) == F)$country)) 

## calculate aid volatility
dah_rmnch.vol <- tibble()
for (i in 1:length(dah_rmnch_country)) {
  # print progress
  print(paste0("processing country ", dah_rmnch_country[i]))
  
  # extract country
  aid <- dah_rmnch %>% filter(country == the_country[i], is.na(total.aid.for.health_rmnch) == F)
  aid.z <- zoo(x=aid$total.aid.for.health_rmnch, order.by=aid$year)
  
  
  #fit GARCH model (rugarch)
  spec = ugarchspec(variance.model=list(garchOrder=c(1,1)),
                    mean.model=list(armaOrder=c(1,0)),
                    distribution.model="norm",)
  fit <- ugarchfit(data=aid.z,spec=spec)
  ##summary of GARCH fit
  fit_sum <- as.data.frame(round(fit@fit$matcoef, 3))
  
  # if model converges: continue; else: fit with fGarch
  if (fit_sum$` Estimate`[3] != 0 & fit_sum$` Estimate`[4] != 0) {
    
    # extract volatility
    aid.vol <- tibble(country = aid$country,
                      country.code = aid$country.code,
                      year = aid$year,
                      total.aid.for.health.volatility_rmnch = as.numeric(sigma(fit)),
                      package = "rugarch")  
  } else {
    
    # fit GARCH model (fGarch)
    fit <- garchFit(~ arma(1,0) + garch(1,1), data=aid.z, trace=FALSE)
    
    # check if model is convergent
    ## if fail to converge, then skip
    fit_sum <- as.data.frame(round(fit@fit$matcoef, 3))
    
    # if model converges: continue; else: next
    if (fit_sum$` Estimate`[3] != 0 & fit_sum$` Estimate`[4] != 0) {
      
      # extract volatility
      aid.vol <- tibble(country = aid$country,
                        country.code = aid$country.code,
                        year = aid$year,
                        total.aid.for.health.volatility_rmnch = fit@sigma.t,
                        package = "fGarch")  # class is numeric
    } 
    
    else {
      print("  model fails to converge")
      next
    }
    
  }
  
  # compile aid.volatility 
  dah_rmnch.vol <- bind_rows(dah_rmnch.vol, aid.vol)
  
}

## save data
write_csv(dah_rmnch.vol, file.path(dir_data, "ihme_dah_rmnch.vol.csv"), na = "")

# 3. PROCESS RESPONSE DATA ----------
## 3.1. SDG 3.8.1 -------------
### 3.1.1. GBD ----------------

# GBD 2017
## workflow
# 1. filter studied countries
# 2. add country.code
# 3. filter estimate_type, indicator_outline 
# 5. create short names
# 6. select important columns
# 7. arrange by country, year

sdg381_17 <- read_csv(file.path(dir_data, "sdg_3.8.1/GBD 2017", "IHME_GBD_2017_HEALTH_SDG_1990_2030.CSV"))
sdg381_17 <- sdg381_17 %>% 
  filter(location_id %in% country_list$country.id_gbd) %>%
  left_join(country_list, by = c("location_id" = "country.id_gbd")) %>%
  filter(estimate_type == "past",
         indicator_outline == "3.8.1") %>%
  rename(year = year_id,
         sdg381 = unscaled_value) %>%
  select(country, country.code, year, sdg381) %>%
  arrange(country, year)

# GBD 2019
## workflow
# 1. filter studied countries
# 2. add country.code
# 3. filter year_id, indicator_name 
# 5. create short names
# 6. select important columns
# 7. arrange by country, year

sdg381_19 <- read_csv(file.path(dir_data, "sdg_3.8.1/GBD 2019", "IHME_GBD_2019_UHC_1990_2019.CSV"))
sdg381_19 <- sdg381_19 %>% 
  filter(location_id %in% country_list$country.id_gbd) %>%
  left_join(country_list, by = c("location_id" = "country.id_gbd")) %>%
  filter(year_id == 2019,
         indicator_name == "UHC effective coverage index") %>%
  rename(year = year_id,
         sdg381 = val) %>%
  select(country, country.code, year, sdg381) %>%
  arrange(country, year)

# merge data
sdg381 <- bind_rows(sdg381_17, sdg381_19)

## save data
write_csv(sdg381, file.path(dir_data, "sdg381.csv"), na = "")

### 3.1.2. WHO RMNCH ------------------------

## workflow
# 1. filter studied countries
# 2. add correct country names
# 3. filter indicator_name, dimension 
# 5. create short names
# 6. select important columns
# 7. arrange by country, year

sdg381_rmnch <- read_xlsx(file.path(dir_data, "sdg_3.8.1/WHO RMNCH", "WHO_RMNCH.xlsx"))
sdg381_rmnch <- sdg381_rmnch %>% 
  filter(iso3 %in% country_list$country.code) %>%
  left_join(country_list, by = c("iso3" = "country.code")) %>%
  filter(indicator_name == "Composite coverage index (%)",
         dimension == "Economic status (wealth quintile)") %>%
  rename(year = date,
         economic.group = subgroup,
         sdg381_rmnch = estimate,
         sdg381_rmnch_average = setting_average,
         country.code = iso3) %>%
  select(country, country.code, year, economic.group, sdg381_rmnch, sdg381_rmnch_average) %>%
  arrange(country, year)

## save data
write_csv(sdg381_rmnch, file.path(dir_data, "sdg381_rmnch.csv"), na = "")

## 3.2. SDG 3.8.2 -------------

# workflow:
# 1. filter studied countries
# 2. add correct country names
# 3. filter Total data 
# 5. create short names
# 6. select important columns
# 7. arrange by country, year

sdg382 <- read_csv(file.path(dir_data, "sdg_3.8.2", "WHO_SDG 3.8.2_10percent.csv"))
sdg382 <- sdg382 %>% 
  filter(SpatialDimValueCode %in% country_list$country.code) %>%
  left_join(country_list, by = c("SpatialDimValueCode" = "country.code")) %>%
  filter(Dim1 == "Total") %>%
  rename(country.code = SpatialDimValueCode,
         year = Period,
         sdg382 = Value) %>%
  select(country, country.code, year, sdg382) %>%
  arrange(country, year)
  
## save data
write_csv(sdg382, file.path(dir_data, "sdg382.csv"), na = "")

#Note: countries without sdg382 data:
## Algeria
## Eritrea
## Korea, Dem. People's Rep.
## Micronesia, Fed. Sts.
## Papua New Guinea
## Samoa
## Solomon Islands
## Vanuatu

# 4. COMBINE DATA ------------

# load data
data_list <- list.files(path = dir_data, pattern = "^[^@]*.csv")

## explanatory data
country_list <- read_csv(file.path(dir_data, data_list[1]))
gbd_hrh <- read_csv(file.path(dir_data, data_list[2]))
gbd_sdi <- read_csv(file.path(dir_data, data_list[3]))
ihme_dah_rmnch <- read_csv(file.path(dir_data, data_list[4]))
ihme_dah_rmnch.vol <- read_csv(file.path(dir_data, data_list[5]))
ihme_the <- read_csv(file.path(dir_data, data_list[6]))
ihme_the.vol <- read_csv(file.path(dir_data, data_list[7]))
oecd_total.aid <- read_csv(file.path(dir_data, data_list[8]))
oecd_total.aid.vol <- read_csv(file.path(dir_data, data_list[9]))
wb_wdi <- read_csv(file.path(dir_data, data_list[13]))
wgi <- read_csv(file.path(dir_data, data_list[14]))

### combine data
list_explanatory <- list(gbd_hrh, gbd_sdi, 
                         ihme_dah_rmnch, ihme_dah_rmnch.vol,
                         ihme_the, ihme_the.vol,
                         oecd_total.aid, oecd_total.aid.vol,
                         wb_wdi, wgi)
data_explanatory <- purrr::reduce(list_explanatory, 
                                  full_join, 
                                  by = c("country", "country.code", "year")) %>%
  select(-package.x, -package.y, -package) %>%
  left_join(country_list, by = c("country", "country.code"))
write_csv(data_explanatory, file.path(dir_data, "@data_explanatory.csv"))


## response data
sdg381 <- read_csv(file.path(dir_data, data_list[10]))
sdg381_rmnch <- read_csv(file.path(dir_data, data_list[11]))
sdg382 <- read_csv(file.path(dir_data, data_list[12]))

## combine data

## 4.1. SDG 3.8.1 -------------
data_sdg381 <- left_join(sdg381, data_explanatory, by = c("country", "country.code", "year"))
write_csv(data_sdg381, file.path(dir_data, "@data_sdg381.csv"))

## 4.2. SDG 3.8.1 RMNCH -------------
data_sdg381_rmnch <- left_join(sdg381_rmnch, data_explanatory, by = c("country", "country.code", "year"))
write_csv(data_sdg381_rmnch, file.path(dir_data, "@data_sdg381_rmnch.csv"))

## 4.3. SDG 3.8.2 -------------
data_sdg382 <- left_join(sdg382, data_explanatory, by = c("country", "country.code", "year"))
write_csv(data_sdg382, file.path(dir_data, "@data_sdg382.csv"))

