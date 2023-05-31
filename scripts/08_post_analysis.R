### Post-analysis analysis
# Either a boosted regression tree or a bayesian glm


library(gbm)
library(MASS)


# Load output data
load("data_output/df_all.Rdata") # out_list


## Question 1: Is there a pulse?
## Response variable of interest: response_cat (1-4)
## Predictor variables of interest: varGroup, Sample.unit, unitDuration, MAT.C.wc, MAP.mm.wc, Pulse.type, Pulse.amount.mm, preVAR

##     BOOSTING     ##

# Make data in the correct format
df_all <- df_all %>%
  mutate(varGroup = as.factor(varGroup), Sample.unit = as.factor(Sample.unit), 
         unitDuration = as.factor(unitDuration), Pulse.type = as.factor(Pulse.type), response_cat = as.factor(response_cat))


# Validation Set 
set.seed(2)
train.index <- sample(c(1:nrow(df_all)), 430, replace=F) 
train <- df_all[train.index,]
test <- df_all[-train.index,]

# Build the Boosted Regression Model
set.seed(24)
q1 <- gbm(response_cat ~ varGroup+Sample.unit+MAT.C.wc+MAP.mm.wc+Pulse.type+Pulse.amount.mm+preVar, 
                    data=train, n.trees = 500, interaction.depth=4,
                    shrinkage = 0.1) # learning rate

q1_relinf<- summary(q1)
save(q1_relinf, file = "data_output/q1_relinf.Rdata")

# if we had interactions
#interact.gbm(x=q1, data=train, i.var = "MAP.mm.wc")

q1_grid_preVar <- plot(q1,i="preVar", return.grid = T)
q1_grid_MAP <- plot(q1,i="MAP.mm.wc", return.grid = T)
q1_grid_MAT <- plot(q1,i="MAT.C.wc", return.grid = T)


write.csv(q1_grid_preVar, file = "data_output/q1_grid_preVar.csv")
write.csv(q1_grid_MAP, file = "data_output/q1_grid_MAP.csv")
write.csv(q1_grid_MAT, file = "data_output/q1_grid_MAT.csv")


#plotmo(q1)

# Validating the Model (MSE)
boost.pred1 <- predict(q1, newdata=test)
mean((boost.pred1 - test$response_cat)^2)

# q1_glm <- glm(response_cat ~ varGroup+Sample.unit+MAT.C.wc+MAP.mm.wc+Pulse.type+Pulse.amount.mm+preVar, data = df_all, family = poisson)
# sink("data_output/q1_glm.txt")
# summary(q1_glm)
# sink()

ggplot(data= df_all) +
  geom_jitter(aes(x = MAP.mm.wc, y = response_cat), alpha= 0.5) +
  theme_bw()


## Question 2: If there is a pulse, do t.peak (time to peak response) 
## and y.peak (magnitude of peak response) depend on the variables of interest?
## Response of interest: y.peak and t.peak mean
## Predictor variables of interest: varGroup, Sample.unit, unitDuration, MAT.C.wc, MAP.mm.wc, Pulse.type, Pulse.amount.mm, preVAR

# Validation Set 
df_all2 <- df_all %>%
  filter(mean_w != 0)
set.seed(2)
train.index <- sample(c(1:nrow(df_all2)), 350, replace=F) 
train <- df_all2[train.index,]
test <- df_all2[-train.index,]

# Build the Boosted Regression Model
q2.y <- gbm(mean_y.peak ~ varGroup+Sample.unit+MAT.C.wc+MAP.mm.wc+Pulse.type+Pulse.amount.mm+preVar, distribution = "gaussian", 
                    data=train, n.trees = 500, interaction.depth=4,
                    shrinkage = 0.1) # learning rate

q2.y_relinf<- summary(q2.y)
save(q2.y_relinf, file = "data_output/q2.y_relinf.Rdata")

q2.y_grid_preVar <- plot(q2.y,i="preVar", return.grid = T)
q2.y_grid_Sample.unit <- plot(q2.y,i="Sample.unit", return.grid = T)
q2.y_grid_Pulse.amount.mm <- plot(q2.y,i="Pulse.amount.mm", return.grid = T)

write.csv(q2.y_grid_preVar, file = "data_output/q2.y_grid_preVar.csv")
write.csv(q2.y_grid_Sample.unit, file = "data_output/q2.y_grid_Sample.unit.csv")
write.csv(q2.y_grid_Pulse.amount.mm, file = "data_output/q2.y_grid_Pulse.amount.mm.csv")

# Validating the Model (MSE)
q2.y.pred <- predict(q2.y, newdata=test)
mean((q2.y.pred - test$w)^2)

q2.y_glm <- glm(mean_y.peak ~ varGroup+Sample.unit+unitDuration+MAT.C.wc+MAP.mm.wc+Pulse.type+Pulse.amount.mm+preVar, data = df_all2)
sink("data_output/q2.y_glm.txt")
summary(q2.y_glm)
sink()


q2.t <- gbm(mean_t.peak ~ varGroup+Sample.unit+MAT.C.wc+MAP.mm.wc+Pulse.type+Pulse.amount.mm+preVar, distribution = "gaussian", 
                    data=train, n.trees = 500, interaction.depth=4,
                    shrinkage = 0.1) # learning rate

q2.t_relinf<- summary(q2.t)
save(q2.t_relinf, file = "data_output/q2.t_relinf.Rdata")

q2.t_grid_preVar <- plot(q2.t,i="preVar", return.grid = T)
q2.t_grid_MAP <- plot(q2.t,i="MAP.mm.wc", return.grid = T)
q2.t_grid_MAT <- plot(q2.t,i="MAT.C.wc", return.grid = T)

write.csv(q2.t_grid_preVar, file = "data_output/q2.t_grid_preVar.csv")
write.csv(q2.t_grid_MAP, file = "data_output/q2.t_grid_MAP.csv")
write.csv(q2.t_grid_MAT, file = "data_output/q2.t_grid_MAT.csv")


# Validating the Model (MSE)
q2.t.pred <- predict(q2.t, newdata=test)
mean((q2.t.pred - test$mean_t.peak)^2)

q2.t_glm <- glm(mean_t.peak ~ varGroup+Sample.unit+unitDuration+MAT.C.wc+MAP.mm.wc+Pulse.type+Pulse.amount.mm+preVar, data = df_all2)
sink("data_output/q2.t_glm.txt")
summary(q2.t_glm)
sink()

## Question 3: If there is a linear pulse response, how steep is mm (slope, speed of response) depending on the variables of interest?
## Response of interest:  mm mean
## Predictor variables of interest: varGroup, Sample.unit, unitDuration, MAT.C.wc, MAP.mm.wc, Pulse.type, Pulse.amount.mm, preVAR
## intercept should be 0, maybe look into
# Validation Set 
df_all3 <- df_all %>%
  filter(mean_w != 1) %>%
  mutate(mean_mm = abs(mean_mm)) # take absolute value, because we only care about steepness
set.seed(2)
train.index <- sample(c(1:nrow(df_all3)), 50, replace=F) 
train <- df_all3[train.index,]
test <- df_all3[-train.index,]

# Build the Boosted Regression Model
q3 <- gbm(mean_mm ~ varGroup+Sample.unit+MAT.C.wc+MAP.mm.wc+Pulse.type+Pulse.amount.mm+preVar, distribution = "gaussian", 
                    data=train, n.trees = 500, interaction.depth=4,
                    shrinkage = 0.1) # learning rate


q3_relinf<- summary(q3)
save(q3_relinf, file = "data_output/q3_relinf.Rdata")

q3_grid_preVar <- plot(q3,i="preVar", return.grid = T)
q3_grid_MAP <- plot(q3,i="MAP.mm.wc", return.grid = T)
q3_grid_MAT <- plot(q3,i="MAT.C.wc", return.grid = T)

write.csv(q3_grid_preVar, file = "data_output/q3_grid_preVar.csv")
write.csv(q3_grid_MAP, file = "data_output/q3_grid_MAP.csv")
write.csv(q3_grid_MAT, file = "data_output/q3_grid_MAT.csv")

# Validating the Model (MSE)
q3.pred <- predict(q3, newdata=test)
mean((q3.pred - test$mean_mm)^2)

q3_glm <- glm(mean_mm ~ varGroup+Sample.unit+MAT.C.wc+MAP.mm.wc+Pulse.type+Pulse.amount.mm+preVar, data = df_all2)
sink("data_output/q3_glm.txt")
summary(q3_glm)
sink()


