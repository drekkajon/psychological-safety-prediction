###############################
## GSS 2024 Data Grab for AJ ##
###############################

## Written by KS Geist

## ------ Load necessary packages --------------------------

### Uses pacman to assist in package load and unload
if (!require("pacman")) install.packages("pacman")
pacman::p_unload(pacman::p_loaded(), character.only = TRUE)
# ## Install GSSR
# install.packages('gssr', repos =
#        c('https://kjhealy.r-universe.dev', 'https://cloud.r-project.org'))
# 
# ## Install GSS Rdoc
# install.packages('gssrdoc', repos =
#        c('https://kjhealy.r-universe.dev', 'https://cloud.r-project.org'))

pacman::p_load(tidyverse, 
               ggplot2,
               gssr,
               gssrdoc,
               kableExtra, 
               gtsummary, 
               survey,
               haven)

## ----- Variable selection -------------------------------
## Let's spell out suitable variables to build a model to predict which employees will have a good relationship with their boss, using the idea that this will depend on core demographic traits, job characteristics like their working status, typical hours, and industry, their perceptions of job and financial stability, and attitudes such as their job satisfaction, levels of pre-existing trust, and their political ideology (which can indicate levels of trust in organizations)

# -- bossemp: confidence in management (target)

### Demographics:
# -- age: continuous, strong baseline predictor (older individuals may be better at navigating and maintaining good relationships with bosses)
# -- sex: categorical
# -- race: categorical (consider collapsing categories)
# -- educ: years of education 
# -- marital: marital status (married individuals may be more likely to tolerate bad relationships for financial stability than risk job loss)
# -- realinc — inflation-adjusted income (better than income alone)

## Employment characteristics:
# -- wrkstat: employment status
# -- indus10 — occupational industry/sector based on 4-digit NAIC code; I strongly suggest collapsing
# -- hrs1 — hours worked last week

## Perceptions of job stability:
# -- joblose: perceived likelihood of job loss (higher perceived chance of loss --> more likely to put up with bad relationships)
# -- jobfind: ease of finding a comparable job (easier to find job --> less likely to put up with bad relationships)
# -- satfin: satisfaction with financial situation (dissatisfied --> more likely to put up with bad relationships)

## Attitudes:
# -- satjob: job satisfaction
# -- trust: general trust in people
# -- polviews: political ideology (often predictive of institutional trust)

## ------ Data acquisition --------------------------------
## Figure out which of the years and ballots have the chosen target variable:
bossempQs <- gssrdoc::gss_which_ballots("bossemps")
bossempQs
## Oooh, so we need to tidy up those ballots first:
bossempQs <- bossempQs %>%
  ## Split string
  mutate(ballots = str_split(ballots, "/")) %>%
  ## Now unnest (separate)
  unnest(ballots) %>%
  ## Get rid of the blank ballots
  filter(ballots != "-") %>%
  ## Rename for downstream sanity
  rename(ballot = ballots) %>% 
  ## Mutate for downstream sanity
  mutate(ballot = recode(ballot,
                         "A" = 1,
                         "B" = 2,
                         "C" = 3,
                         "D" = 4))
bossempQs
## Make sure you keep the years! That's important structure to the data (temporal)


## Now, let's grab all the data into a single df:
## Start from bossempQs which have the years and ballot ids
gss <- bossempQs %>%
  ## Use map2() to grab each year
  mutate(data = map2(year, ballot, ~ {
    
    ## Grab each year 
    df <- gssr::gss_get_yr(.x)
    
    df %>%
      ## Grab each ballot
    filter(ballot == .y) %>%
      ## Grab the chosen variables
      select(year, ballot, bossemps, wrkstat, age, sex, race, educ,
           marital, realinc, indus10, hrs1,
           joblose, jobfind, satfin, satjob,
           trust, polviews, region)
  })) %>%
  pull(data) %>%
  bind_rows() %>% 
  ## Save industry as a number - we need it later:
  mutate(indus10num = as.numeric(indus10)) 

gssCleaned <- gss %>%
  select(-indus10num) %>% 
  ## Last step! Need to convert from the STATA import format using haven
  mutate(across(where(haven::is.labelled), haven::as_factor)) %>%
  ## Convert factors to character temporarily
  mutate(across(where(is.factor), as.character)) %>%
  ## Then get rid of those pesky characters that should be NAs:
  mutate(across(everything(), ~na_if(., "iap"))) %>%
  mutate(across(everything(), ~na_if(., "dk"))) %>%
  mutate(across(everything(), ~na_if(., "no answer"))) %>% 
  mutate(across(everything(), ~na_if(., "don't know"))) %>% 
  mutate(across(everything(), ~na_if(., "a number"))) %>% 
  
  ## Then change back what should be a number vs. factor
  mutate(
    across(c(sex, race, marital, wrkstat, trust, polviews, indus10, region, year,
             bossemps), as.factor)) %>%
  ## Parse_number is best for survey data because it will deal with '89 and older' --> 89 or '89+' --> 89
    mutate(
      age     = suppressWarnings(parse_number(age)),
      educ    = suppressWarnings(parse_number(educ)),
      realinc = suppressWarnings(parse_number(realinc)),
      hrs1    = suppressWarnings(parse_number(hrs1)),
    ) %>% 
  ## And then add the industry number back:
  mutate(indus10num = gss$indus10num) %>% 
  ## Okay really last step - filter to just those employed:
  filter(!is.na(hrs1) & hrs1 > 0)

  
## ----- CLEANING & TRANSFORMATION ---------------------------------
## I will help you with some of the trickier cleaning, and leave the easier ones to you!

##### 1. INDUSTRY
## In my role, we use NAICS codes for industry/sector, and often collapse like
## this:
#### Extractive + Construction
#### Manufacturing
#### Trade + Transport
#### Information + Finance
#### Professional + Business services
#### Education + Social Services
#### Public Administration
#### Healthcare
#### Other services
## NAICS codes are hierarchical, where the first 2 digits ~ sector
gssCleaned <- gssCleaned %>%
  mutate(naics2 = floor(indus10num / 100),  ## first 2 digits
         industry = as.factor(case_when(
           # --- Extractive + Construction ---
           naics2 %in% c(11, 21, 23) ~ "Extract_Construction",
           
           # --- Manufacturing ---
           naics2 %in% c(31, 32, 33) ~ "Manufacturing",
           
           # --- Trade + Transport ---
           naics2 %in% c(42, 44, 45, 48, 49) ~ "Trade_Transport",
           
           # --- Information + Finance ---
           naics2 %in% c(51, 52, 53) ~ "Info_Finance",
           
           # --- Professional / Business ---
           naics2 %in% c(54, 55, 56) ~ "Professional",
           
           # --- Healthcare (separate, as requested) ---
           naics2 == 62 ~ "Healthcare",
           
           # --- Education + Social Services ---
           naics2 %in% c(61, 81) ~ "EducationSS",
           
           # --- Public administration ---
           naics2 == 92 ~ "PublicAdmin",
           
           # --- Other ---
           TRUE ~ "Other"
  ))) %>% 
  ## And always drop the original column!!
  select(-indus10num, -naics2)

## ----- TRUST -----------------------------------
## Let's recode as binary for simplicity: 
gssCleaned <- gssCleaned %>% 
  mutate(trustBinary = case_when(
    trust == "most people can be trusted" ~ 1,
    TRUE ~ 0
  ))
## Proof we preserved:
table(gssCleaned$trust, useNA = "always")
table(gssCleaned$trustBinary, useNA = "always")
## Then always drop the original column:
gssCleaned <- gssCleaned %>% select(-trust)

## ----- JOB LOSE + JOB FIND ---------------------
## Check the reverse coding; job lose higher value == "worse" and job find higher value == "better" so we need them to go in the same directions!
gssCleaned <- gssCleaned %>% 
  mutate(
    jobloseNum = case_when(
      joblose == "not likely" ~ 1,
      joblose == "not too likely" ~ 2,
      joblose == "fairly likely" ~ 3,
      joblose == "very likely" ~ 4,
      TRUE ~ NA_real_
    ),
    jobfindNum = case_when(
      jobfind == "very easy" ~ 1,
      jobfind == "somewhat easy" ~ 2,
      jobfind == "not easy" ~ 3,
      TRUE ~ NA_real_
    )
  )
## Proof we preserved:
table(gssCleaned$joblose, useNA = "always")
table(gssCleaned$jobloseNum, useNA = "always")
## Proof we preserved:
table(gssCleaned$jobfind, useNA = "always")
table(gssCleaned$jobfindNum, useNA = "always")
## Then, drop your original variables!
gssCleaned <- gssCleaned %>% 
  select(-joblose, -jobfind)

## ----- JOB SAT + FIN SAT ---------------------
## These are coded in the same direction -- and we could even engineer a new variable
## that is a measure of SATISFACTION if desired
gssCleaned <- gssCleaned %>% 
  mutate(
    satfinNum = case_when(
      satfin == "not satisfied at all" ~ 1,
      satfin == "more or less satisfied" ~ 2,
      satfin == "pretty well satisfied" ~ 3,
      TRUE ~ NA_real_
    ),
    
    satjobNum = case_when(
      satjob == "very dissatisfied" ~ 1,
      satjob == "a little dissatisfied" ~ 2,
      satjob == "moderately satisfied" ~ 3,
      satjob == "very satisfied" ~ 4,
      TRUE ~ NA_real_
    ),
    
    ## Engineered satisfaction variable taken as the mean of the two measures of satisfaction
    ## Note that if one is missing, then the other will still be returned
    satisfaction = rowMeans(
          across(c(satjobNum, satfinNum)),
          na.rm = TRUE
      )
  )
## Proof we preserved:
table(gssCleaned$satfin, useNA = "always")
table(gssCleaned$satfinNum, useNA = "always")
## Proof we preserved:
table(gssCleaned$satjob, useNA = "always")
table(gssCleaned$satjobNum, useNA = "always")
## Then, drop your original variables!
gssCleaned <- gssCleaned %>% 
  select(-satfin, -satjob)

##### DROP UNUSED LEVELS OF FACTORS
gssCleaned <- gssCleaned %>% 
  droplevels()


## ----- Quick Summary Statistics by Year ----------------------
gssCleaned %>%
  select(-ballot, -indus10) %>%
  tbl_summary(
    by = year,
    type = list(satisfaction ~ "continuous"),
    statistic = list(
      satisfaction ~ "{mean} ({sd})" ## Had to add because tbl_summary was choking for some mystery
    )
  )

## AND YOU CAN FINISH CLEAN UP!

