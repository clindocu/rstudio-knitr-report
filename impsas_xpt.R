#### Set R working directory
setwd("C:/Temp/ALEA01/")
#### Use install.packages("knitr"), install.packages("foreign"), etc. if necessary
library(knitr)
library(foreign)
#### tidyverse: ggplot2, tibble, tidyr, readr, purrr, dplyr
library(tidyverse)
library(xtable)
library(gridExtra)
library(survminer)
library(ggthemes)
library(reporttools)
library(texreg)
library(car)
library(Hmisc)
#### Read all .xpt files from working directory. Derive date/times. Set factor, levels, labels.
ADLB <- read.xport("ADLB.xpt")
ADRS1 <- read.xport("ADRS1.xpt")
ADRS2 <- read.xport("ADRS2.xpt")
ADSL <- read.xport("ADSL.xpt")
ADTTE <- read.xport("ADTTE.xpt")
label(ADLB$STUDYID) <- "Study Identifier"
label(ADLB$USUBJID) <- "Unique Subject Identifier"
label(ADLB$ASEQ) <- "Sequence Number"
ADLB$SEX <- factor(ADLB$SEX, c("F", "M"), exclude = "")
levels(ADLB$SEX) <- c("Female", "Male")
label(ADLB$SEX) <- "Sex"
label(ADLB$PARAM) <- "Parameter"
label(ADLB$PARAMCD) <- "Parameter Code"
ADLB$AVISIT <- factor(ADLB$AVISIT, c("BASELINE", "WEEK 8", "WEEK 16", "WEEK 24"), exclude = "")
levels(ADLB$AVISIT) <- c("Baseline", "Week 8", "Week 16", "Week 24")
label(ADLB$AVISIT) <- "Analysis Visit"
label(ADLB$AVISITN) <- "Analysis Visit (N)"
label(ADLB$AVAL) <- "Analysis value"
label(ADLB$BASE) <- "Baseline Value"
label(ADLB$CHG) <- "Change from Baseline"
label(ADLB$PCHG) <- "Percent Change from Baseline"
label(ADLB$DTYPE) <- "Derivation Type"
label(ADLB$ANRLO) <- "Analysis Normal Range Lower Limit"
label(ADLB$ANRHI) <- "Analysis Normal Range Upper Limit"
ADLB$ANRIND <- factor(ADLB$ANRIND, c("LOW", "NORMAL", "HIGH"), exclude = "")
levels(ADLB$ANRIND) <- c("Low", "Normal", "High")
label(ADLB$ANRIND) <- "Analysis Reference Range Indicator"
ADLB$ABLFL <- factor(ADLB$ABLFL, c("Y"), exclude = "")
levels(ADLB$ABLFL) <- c("Yes")
label(ADLB$ABLFL) <- "Baseline Record Flag"
ADLB$BNRIND <- factor(ADLB$BNRIND, c("LOW", "NORMAL", "HIGH"), exclude = "")
levels(ADLB$BNRIND) <- c("Low", "Normal", "High")
label(ADLB$BNRIND) <- "Baseline Reference Range Indicator"
ADLB$SHIFT1 <- factor(ADLB$SHIFT1, c("LOW to LOW", "LOW to NORMAL", "LOW to HIGH", "NORMAL to LOW", "NORMAL to NORMAL", "NORMAL to HIGH", "HIGH to LOW", "HIGH to NORMAL", "HIGH to HIGH"), exclude = "")
levels(ADLB$SHIFT1) <- c("Low to Low", "Low to Normal", "Low to High", "Normal to Low", "Normal to Normal", "Normal to High", "High to Low", "High to Normal", "High to High")
label(ADLB$SHIFT1) <- "Shift 1"
ADLB$ANRIND2 <- factor(ADLB$ANRIND2, c("ABNORMAL", "NORMAL"), exclude = "")
levels(ADLB$ANRIND2) <- c("Abnormal", "Normal")
label(ADLB$ANRIND2) <- "Analysis Reference Range Indicator 2"
ADLB$BNRIND2 <- factor(ADLB$BNRIND2, c("ABNORMAL", "NORMAL"), exclude = "")
levels(ADLB$BNRIND2) <- c("Abnormal", "Normal")
label(ADLB$BNRIND2) <- "Baseline Reference Range Indicator 2"
ADLB$SHIFT2 <- factor(ADLB$SHIFT2, c("ABNORMAL to ABNORMAL", "ABNORMAL to NORMAL", "NORMAL to NORMAL", "NORMAL to ABNORMAL"), exclude = "")
levels(ADLB$SHIFT2) <- c("Abnormal to Abnormal", "Abnormal to Normal", "Normal to Normal", "Normal to Abnormal")
label(ADLB$SHIFT2) <- "Shift 2"
label(ADLB$AGEGR1) <- "Pooled Age Group 1"
label(ADRS1$STUDYID) <- "Study Identifier"
label(ADRS1$USUBJID) <- "Unique Subject Identifier"
label(ADRS1$PARAMCD) <- "Parameter Code"
label(ADRS1$PARAM) <- "Parameter"
label(ADRS1$AVAL) <- "Analysis Value"
ADRS1$AVALC <- factor(ADRS1$AVALC, c("COMPLETE RESPONSE", "PARTIAL RESPONSE", "STABLE DISEASE", "PROGRESSIVE DISEASE", "MISSING"), exclude = "")
levels(ADRS1$AVALC) <- c("Complete Response", "Partial Response", "Stable Disease", "Progressive Disease", "Missing")
label(ADRS1$AVALC) <- "Analysis Value (C)"
label(ADRS1$RSEVALN) <- "Assessor type (N)"
label(ADRS1$RSEVAL) <- "Assessor type"
label(ADRS1$PARCAT1) <- "Parameter Category 1"
label(ADRS1$AGEGR1) <- "Pooled Age Group 1"
label(ADRS2$STUDYID) <- "Study Identifier"
label(ADRS2$USUBJID) <- "Unique Subject Identifier"
label(ADRS2$PARAMCD) <- "Parameter Code"
label(ADRS2$PARAM) <- "Parameter"
label(ADRS2$AVAL) <- "Analysis Value"
ADRS2$AVALC <- factor(ADRS2$AVALC, c("RESPONDER", "NON RESPONDER"), exclude = "")
levels(ADRS2$AVALC) <- c("Responder", "Non Responder")
label(ADRS2$AVALC) <- "Analysis Value (C)"
label(ADRS2$RSEVALN) <- "Assessor type (N)"
label(ADRS2$RSEVAL) <- "Assessor type"
label(ADRS2$PARCAT1) <- "Parameter Category 1"
label(ADRS2$AGEGR1) <- "Pooled Age Group 1"
label(ADSL$STUDYID) <- "Study Identifier"
label(ADSL$USUBJID) <- "Unique Subject Identifier"
label(ADSL$SUBJID) <- "Subject Identifier for the Study"
label(ADSL$SITEID) <- "Study Site Identifier"
label(ADSL$AGE) <- "Age"
label(ADSL$AGEU) <- "Age Units"
label(ADSL$AGEGR1) <- "Pooled Age Group 1"
label(ADSL$AGEGR1N) <- "Pooled Age Group 1 (N)"
ADSL$SEX <- factor(ADSL$SEX, c("F", "M"), exclude = "")
levels(ADSL$SEX) <- c("Female", "Male")
label(ADSL$SEX) <- "Sex"
ADSL$SEXN <- factor(ADSL$SEXN, c(0, 1), exclude = "")
levels(ADSL$SEXN) <- c("Female", "Male")
label(ADSL$SEXN) <- "Sex (N)"
ADSL$RACE <- factor(ADSL$RACE, c("WHITE", "BLACK OR AFRICAN AMERICAN", "AMERICAN INDIAN OR ALASKA NATIVE", "ASIAN"), exclude = "")
levels(ADSL$RACE) <- c("White", "Black or African American", "American Indian or Alaska Native", "Asian")
label(ADSL$RACE) <- "Race"
ADSL$RACEN <- factor(ADSL$RACEN, c(1, 2, 3, 4), exclude = "")
levels(ADSL$RACEN) <- c("White", "Black or African American", "American Indian or Alaska Native", "Asian")
label(ADSL$RACEN) <- "Race (N)"
label(ADSL$WEIGHTBL) <- "Weight at Baseline (kg)"
label(ADSL$L10WTBL) <- "Log10(Weight (kg))"
label(ADSL$HEIGHT) <- "Height (cm)"
label(ADSL$BMI) <- "Body Mass Index (kg/m2)"
label(ADSL$ARM) <- "Description of Planned Arm"
label(ADSL$TRT01P) <- "Description of Actual Arm"
ADSL$FASFL <- factor(ADSL$FASFL, c("Y", "N"), exclude = "")
levels(ADSL$FASFL) <- c("Yes", "No")
label(ADSL$FASFL) <- "Full Analysis Set Population Flag"
label(ADSL$HGBBL) <- "Hemoglobin at Baseline (g/dL)"
ADSL$ECOGBL <- factor(ADSL$ECOGBL, c(0, 1, 2, 3, 4), exclude = "")
levels(ADSL$ECOGBL) <- c("0", "1", "2", "3", "4")
label(ADSL$ECOGBL) <- "ECOG at Baseline"
label(ADSL$DSTERM) <- "Reported Term for the Disposition Event"
ADSL$EOSSTT <- factor(ADSL$EOSSTT, c("COMPLETED", "DISCONTINUED", "ONGOING"), exclude = "")
levels(ADSL$EOSSTT) <- c("Completed", "Discontinued", "Ongoing")
label(ADSL$EOSSTT) <- "End of Study Status"
ADSL$DCSREAS <- factor(ADSL$DCSREAS, c("ADVERSE EVENT", "LOST TO FOLLOW-UP", "WITHDRAWAL BY SUBJECT"), exclude = "")
levels(ADSL$DCSREAS) <- c("Adverse Event", "Lost to Follow-Up", "Withdrawal by Subject")
label(ADSL$DCSREAS) <- "Reason for Discontinuation from Study"
label(ADTTE$STUDYID) <- "Study Identifier"
label(ADTTE$USUBJID) <- "Unique Subject Identifier"
label(ADTTE$ASEQ) <- "Analysis Sequence Number"
label(ADTTE$PARAM) <- "Parameter"
label(ADTTE$PARAMCD) <- "Parameter Code"
label(ADTTE$AVAL) <- "Analysis Value"
label(ADTTE$CNSR) <- "Censor (1=censored)"
label(ADTTE$EVNTDESC) <- "Event or Censoring Description"
label(ADTTE$AGEGR1) <- "Pooled Age Group 1"
label(ADTTE$AGEGR1N) <- "Pooled Age Group 1 (N)"
