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

library(ordinal)
q1_clm <- clm(as.factor(response_cat) ~ varGroup+Sample.unit+MAT.C.wc+MAP.mm.wc+Pulse.type+Pulse.amount.mm+preVar+obs+preVar*MAP.mm.wc, data = df_all)
sink("data_output/q1_clm.txt") # sink will save the summary output to a text file
summary(q1_clm)
sink() # end what we save to the text file

## Question 1.1: Is there a pulse? (Q1 but cat responses are collapsed)
## Response variable of interest: response_cat (1-2)
## Predictor variables of interest: varGroup, Sample.unit, MAT.C.wc, MAP.mm.wc, Pulse.type, Pulse.amount.mm, preVAR

# Make data in the correct format
df_all1 <- df_all
df_all1$response_cat <- ifelse(df_all1$response_cat< 4, 1, 2)


q1.1_glm <- glm(as.factor(response_cat) ~ varGroup+Sample.unit+MAT.C.wc+MAP.mm.wc+Pulse.type+Pulse.amount.mm+preVar+obs+preVar*MAP.mm.wc+MAP.mm.wc*Pulse.amount.mm, data = df_all1, family = binomial)
sink("data_output/q1.1_glm.txt") # sink will save the summary output to a text file
summary(q1.1_glm)
sink() # end what we save to the text file

## Question 2: If there is a pulse, do t.peak (time to peak response) - NOTE: use 'lm" instead
## and y.peak (magnitude of peak response) depend on the variables of interest?
## Response of interest: y.peak and t.peak mean
## Predictor variables of interest: varGroup, Sample.unit, MAT.C.wc, MAP.mm.wc, Pulse.type, Pulse.amount.mm, preVAR

# Filter for all data that is at least somewhat Ricker-like
df_all2 <- df_all %>%
  filter(mean_w != 0)

q2.t_glm <- glm(mean_t.peak ~ varGroup+Sample.unit+MAT.C.wc+MAP.mm.wc+Pulse.type+Pulse.amount.mm+preVar+obs+preVar*MAP.mm.wc+MAP.mm.wc*Pulse.amount.mm, data = df_all2)
sink("data_output/q2.t_glm.txt")
summary(q2.t_glm)
sink()

q2.y_glm <- glm(mean_y.peak ~ varGroup+Sample.unit+MAT.C.wc+MAP.mm.wc+Pulse.type+Pulse.amount.mm+preVar+obs+preVar*MAP.mm.wc+MAP.mm.wc*Pulse.amount.mm, data = df_all2)
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

q3_glm <- glm(mean_mm ~ varGroup+Sample.unit+MAT.C.wc+MAP.mm.wc+Pulse.type+Pulse.amount.mm+preVar+obs+preVar*MAP.mm.wc+MAP.mm.wc*Pulse.amount.mm, data = df_all3)
sink("data_output/q3_glm.txt")
summary(q3_glm)
sink()

q3_glm_coeff <- summary(q3_glm)$coef[,1]


#### Save tables in convenient format for plotting

# Data for figure of heatmap for p-values

df_q1 <- as.data.frame(summary(q1_clm)$coefficients[,4]) %>%
  rownames_to_column()
df_q1 <- df_q1[-(1:3),]
colnames(df_q1) <- c("pred", "q1")
df_q1.1 <- as.data.frame(summary(q1.1_glm)$coefficients[,4])%>%
  rownames_to_column()
df_q1.1 <- df_q1.1[-1,]
colnames(df_q1.1) <- c("pred", "q1.1")

df_q2.t <- as.data.frame(summary(q2.t_glm)$coefficients[,4]) %>%
  rownames_to_column()
df_q2.t <- df_q2.t[-1,]
colnames(df_q2.t) <- c("pred", "q2.t")

df_q2.y <- as.data.frame(summary(q2.y_glm )$coefficients[,4]) %>%
  rownames_to_column()
df_q2.y <- df_q2.y[-1,]
colnames(df_q2.y) <- c("pred", "q2.y")

df_q3 <- as.data.frame(summary(q3_glm )$coefficients[,4]) %>%
  rownames_to_column()
df_q3 <- df_q3[-1,]
colnames(df_q3) <- c("pred", "q3")


df_q <- full_join(df_q1, df_q1.1)
df_q <- full_join(df_q, df_q2.t)
df_q <- full_join(df_q, df_q2.y)
df_q <- full_join(df_q, df_q3)

colnames(df_q) <- c("pred", "Response type", "Response or no response", "time of peak", "magnitude of peak", "speed of linear response")

rowp<- df_q$pred
df_q <- df_q %>%
  select(-pred)
rownames(df_q) <- rowp
rownames(df_q) <- c("varGroupwater", "Sample.unitindividual", "Sample.unitplot/collar", "Sample.unitfootprint",
                    "MAT.C.wc", "MAP.mm.wc","Pulse.typeNatural", "Pulse.amount.mm", "preVar", "obs",
                    "preVar*MAP.mm.wc", "MAP.mm.wc*Pulse.amount.mm")

write.csv(df_q, file = "data_output/p_mat.csv")


# Data for figure of heatmap for coefficients

df_q1 <- as.data.frame(summary(q1_clm)$coefficients[,c(1,4)]) %>%
  rownames_to_column()
df_q1 <- df_q1[-(1:3),]
colnames(df_q1) <- c("pred","est","q1")
df_q1$est <- ifelse(df_q1$q1>0.05, 0, df_q1$est)
df_q1 <- df_q1 %>%
  select(c(1,2)) %>%
  rename(q1 = est)

df_q1.1 <- as.data.frame(summary(q1.1_glm)$coefficients[,c(1,4)])%>%
  rownames_to_column()
df_q1.1 <- df_q1.1[-1,]
colnames(df_q1.1) <- c("pred","est","q1.1")
df_q1.1$est <- ifelse(df_q1.1$q1.1>0.05, 0, df_q1.1$est)
df_q1.1 <- df_q1.1 %>%
  select(c(1,2)) %>%
  rename(q1.1 = est)

df_q2.t <- as.data.frame(summary(q2.t_glm)$coefficients[,c(1,4)]) %>%
  rownames_to_column()
df_q2.t <- df_q2.t[-1,]
colnames(df_q2.t) <- c("pred","est", "q2.t")
df_q2.t$est <- ifelse(df_q2.t$q2.t>0.05, 0, df_q2.t$est)
df_q2.t <- df_q2.t %>%
  select(c(1,2)) %>%
  rename(q2.t = est)

df_q2.y <- as.data.frame(summary(q2.y_glm )$coefficients[,c(1,4)]) %>%
  rownames_to_column()
df_q2.y <- df_q2.y[-1,]
colnames(df_q2.y) <- c("pred","est", "q2.y")
df_q2.y$est <- ifelse(df_q2.y$q2.y>0.05, 0, df_q2.y$est)
df_q2.y <- df_q2.y %>%
  select(c(1,2)) %>%
  rename(q2.y = est)

df_q3 <- as.data.frame(summary(q3_glm )$coefficients[,c(1,4)]) %>%
  rownames_to_column()
df_q3 <- df_q3[-1,]
colnames(df_q3) <- c("pred","est", "q3")
df_q3$est <- ifelse(df_q3$q3>0.05, 0, df_q3$est)
df_q3 <- df_q3 %>%
  select(c(1,2)) %>%
  rename(q3 = est)


df_q <- full_join(df_q1, df_q1.1)
df_q <- full_join(df_q, df_q2.t)
df_q <- full_join(df_q, df_q2.y)
df_q <- full_join(df_q, df_q3)

colnames(df_q) <- c("pred", "Response type", "Response or no response", "time of peak", "magnitude of peak", "speed of linear response")

rowp<- df_q$pred
df_q <- df_q %>%
  select(-pred)
rownames(df_q) <- rowp
rownames(df_q) <- c("varGroupwater", "Sample.unitindividual", "Sample.unitplot/collar", "Sample.unitfootprint",
                    "MAT.C.wc", "MAP.mm.wc","Pulse.typeNatural", "Pulse.amount.mm", "preVar", "obs",
                    "preVar*MAP.mm.wc", "MAP.mm.wc*Pulse.amount.mm")

write.csv(df_q, file = "data_output/coeff_mat.csv")


# Basic Interaction Plot 
interaction.plot(x.factor = df_all2$MAP.mm.wc * df_all2$preVar, 
                 trace.factor = df_all2$response_cat,  
                 response = df_all2$mean_t.peak, fun = mean)

