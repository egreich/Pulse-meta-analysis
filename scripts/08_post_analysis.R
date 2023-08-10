### Post-analysis analysis
# glm code

library(tidyverse)


# Load output data
load("data_output/df_all.Rdata") # df_all


## Question 1: Is there a pulse?
## Response variable of interest: response_cat (1-4)
## Predictor variables of interest: varGroup, Sample.unit, MAT.C.wc, MAP.mm.wc, Pulse.type, Pulse.amount.mm, preVAR

# Make data in the correct format
df_all <- df_all %>%
  mutate(varGroup = as.factor(varGroup), Sample.unit = as.factor(Sample.unit), 
         Pulse.type = as.factor(Pulse.type), response_cat = as.numeric(response_cat))

df_all$Sample.unit <- factor(df_all$Sample.unit, levels = c("leaf", "individual", "plot/collar", "footprint"))


q1_glm <- glm(response_cat ~ varGroup+Sample.unit+MAT.C.wc+MAP.mm.wc+Pulse.type+Pulse.amount.mm+preVar, data = df_all, family = poisson)
sink("data_output/q1_glm.txt") # sink will save the summary output to a text file
summary(q1_glm)
sink() # end what we save to the text file



## Question 2: If there is a pulse, do t.peak (time to peak response) 
## and y.peak (magnitude of peak response) depend on the variables of interest?
## Response of interest: y.peak and t.peak mean
## Predictor variables of interest: varGroup, Sample.unit, MAT.C.wc, MAP.mm.wc, Pulse.type, Pulse.amount.mm, preVAR

# Filter for all data that is at least somewhat Ricker-like
df_all2 <- df_all %>%
  filter(mean_w != 0)

q2.t_glm <- glm(mean_t.peak ~ varGroup+Sample.unit+MAT.C.wc+MAP.mm.wc+Pulse.type+Pulse.amount.mm+preVar, data = df_all2)
sink("data_output/q2.t_glm.txt")
summary(q2.t_glm)
sink()

q2.y_glm <- glm(mean_y.peak ~ varGroup+Sample.unit+unitDuration+MAT.C.wc+MAP.mm.wc+Pulse.type+Pulse.amount.mm+preVar, data = df_all2)
sink("data_output/q2.y_glm.txt")
summary(q2.y_glm)
sink()

## Question 3: If there is a linear pulse response, how steep is mm (slope, speed of response) depending on the variables of interest?
## Response of interest:  mm mean
## Predictor variables of interest: varGroup, Sample.unit, MAT.C.wc, MAP.mm.wc, Pulse.type, Pulse.amount.mm, preVAR
## intercept should be 0, maybe look into

# Filter for all pulses with at least some linear characteristics
df_all3 <- df_all %>%
  filter(mean_w != 1) %>%
  mutate(mean_mm = abs(mean_mm)) # take absolute value, because we only care about steepness

q3_glm <- glm(mean_mm ~ varGroup+Sample.unit+MAT.C.wc+MAP.mm.wc+Pulse.type+Pulse.amount.mm+preVar, data = df_all3)
sink("data_output/q3_glm.txt")
summary(q3_glm)
sink()

q3_glm_coeff <- summary(q3_glm)$coef[,1]


