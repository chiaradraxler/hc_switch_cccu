library(formr)
library(tidyverse)
load("cccu_MA.RData")


table(cccu_MA$DOV_Q26_6)
table(cccu_MA$NOBARRIER)
crosstabs(~ NOBARRIER + DOV_Q26_6, data = cccu_MA)

crosstabs(~ hc + DOV_Q26_6, data = cccu_MA)
crosstabs(~ hc + NOBARRIER, data = cccu_MA)

