# Plot model output
# install.packages("broom.mixed")
library(coda)
library(broom.mixed)
library(dplyr)
library(tidyr)
library(ggplot2)

# Load data and coda
load("models/01-test-Ricker/model_input.Rdata")  # List of all variable dataframes
# Var names: 1:"ET", 2:"WUE", 3:"T", 4:"Gs", 5:"PWP", 6:"ecosystemR", 
# 7:"abovegroundR", 8:"belowgroundR", 9:"NPP", 10:"GPP", 11:"Anet"

varname <- "ET"
dfin <- out_list[[varname]]

# Create file names
jm_codafilename <- paste("./models/01-test-Ricker/coda/jm_coda_", varname,".RData", sep = "")
jm_repfilename <- paste("./models/01-test-Ricker/coda/jm_rep_", varname,".RData", sep = "")


# load("models/01-test-Ricker/inputET.Rdata") # et3

pulse_table <- dfin %>%
  expand(nesting(Study.ID, Pulse.ID)) %>%
  mutate(sID = as.numeric(factor(Study.ID))) %>%
  arrange(Study.ID) %>%
  tibble::rownames_to_column() %>%
  rename(pID = rowname) %>%
  mutate(pID = as.numeric(pID)) %>%
  relocate(pID, .after = sID)

# Join with full table, restrict to 14 days after pulse, remove pre-pulse days
df2 <- dfin %>%
  left_join(pulse_table) %>%
  relocate(sID, pID) %>%
  filter(Days.relative.to.pulse <= 14,
         Days.relative.to.pulse != -1)

# Plot each pulse
ggplot(df2, aes(x = Days.relative.to.pulse + 1,
                y = LRR)) +
  # geom_errorbar(aes(ymin = LRR - sqrt(poolVar),
  #  ymax = LRR + sqrt(poolVar),
  #  color = as.factor(sID)),
  # width = 0) +
  geom_point(aes(color = as.factor(sID))) +
  geom_hline(yintercept = 0, lty = 2) +
  facet_wrap(~pID) +
  theme_bw() +
  guides(color = "none")

# Load codas

load(file = jm_codafilename)
load(file = jm_repfilename) 

# Summarize
param_sum <- tidyMCMC(jm_coda,
                      conf.int = TRUE,
                      conf.method = "HPDinterval")

rep_sum <- tidyMCMC(jm_rep,
                    conf.int = TRUE,
                    conf.method = "HPDinterval")

pred <- cbind.data.frame(df2, rep_sum)


# Extract posterior means for Ricker parameters
ypeaks <- param_sum %>%
  filter(grepl("^y.peak", term))

tpeaks <- param_sum %>%
  filter(grepl("^t.peak", term))

Ltpeaks <- param_sum %>%
  filter(grepl("^Lt.peak", term))

pulse_params <- data.frame(pID = 1:nrow(pulse_table),
                           ypeak = ypeaks$estimate,
                           tpeak = tpeaks$estimate,
                           Ltpeak = Ltpeaks$estimate)

# Recreate Ricker function
rick <- function(t,  ypeak, tpeak, Ltpeak) {

  LRR = ypeak * exp(1 - t/tpeak + log(t) - Ltpeak)
  
  return(LRR)
  
}

# Create dataframe of functions for plotting
t_in <- seq(0, 14, by = 0.1)
pulse_values <- data.frame(pID = rep(1:nrow(pulse_params), each = length(t_in)),
                           t = rep(t_in, nrow(pulse_params)),
                           ypeak = rep(pulse_params$ypeak, each = length(t_in)),
                           tpeak = rep(pulse_params$tpeak, each = length(t_in)),
                           Ltpeak = rep(pulse_params$Ltpeak, each = length(t_in))) %>%
  mutate(lrr = rick(t, ypeak, tpeak, Ltpeak))

# Plot replicated and actual ET LRR by pulse

ggplot() +
  geom_pointrange(data = pred, 
                  aes(x = Days.relative.to.pulse + 1,
                      y = estimate,
                      ymin = conf.low,
                      ymax = conf.high),
                  size = 0.5,
                  alpha = 0.5) +
  geom_point(data = pred, 
             aes(x = Days.relative.to.pulse + 1,
                 y = LRR,
                 color = as.factor(sID))) +
  geom_line(data = pulse_values,
            aes(x = t, y = lrr)) +
  scale_y_continuous(limits = c(-2, 4)) +
  geom_hline(yintercept = 0, lty = 2) +
  facet_wrap(~pID) +
  theme_bw() +
  guides(color = "none")

# Model fit
m1 <- lm(estimate ~ LRR, data = pred)
summary(m1)

ggplot() +
  geom_abline(slope = 1, 
              intercept = 0,
              lty = 2) +
  geom_errorbar(data = pred, aes(x = LRR, 
                                 ymin = conf.low,
                                 ymax = conf.high),
                alpha = 0.25) +
  geom_point(data = pred, aes(x = LRR, y = estimate)) +
  annotate("text", label = "R^2==0.879",
           x = -1, y = 4.5,
            parse = TRUE) +
  scale_x_continuous("Observed ET LRR") +
  scale_y_continuous("Predicted ET LRR") +
  theme_bw(base_size = 12) +
  coord_equal()


# Plot parameters
# pop <- param_sum %>%
#   filter(grepl("mu\\.y\\.peak", term) |
#            grepl("mu\\.Lt\\.peak", term)) %>%
#   tidyr::separate(term, c("level", "variable"))

pulse <- param_sum %>%
  filter(grepl("^y\\.peak", term) |
           grepl("^t\\.peak", term)) %>%
  tidyr::separate(term, c("variable", NA, "pulse"))


ggplot() +
  geom_pointrange(data = pulse, 
                  aes(x = variable,
                      y = estimate,
                      ymin = conf.low,
                      ymax = conf.high,
                      color = factor(pulse)),
                  position = position_dodge(width = 1),
                  alpha = 0.5) 
  # geom_pointrange(data = pop, 
  #                 aes(x = variable,
  #                     y = estimate,
  #                     ymin = conf.low,
  #                     ymax = conf.high),
  #                 size = 1) +
  scale_y_continuous("Posterior mean") +
  facet_wrap(~variable,
             scales = "free") +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  guides(color = "none")
