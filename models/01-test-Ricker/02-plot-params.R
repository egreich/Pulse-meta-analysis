# Plot model output

library(coda)
library(broom.mixed)
library(dplyr)
library(ggplot2)

# Load data and coda
load("models/01-test-Ricker/inputET.Rdata")
et2 <- et2 %>%
  mutate(sID = as.numeric(factor(Study.ID)))
load(file = "models/01-test-Ricker/coda/jm_coda.Rdata")
load(file = "models/01-test-Ricker/coda/jm_rep.Rdata") 

# Summarize
param_sum <- tidyMCMC(jm_coda,
                      conf.int = TRUE,
                      conf.method = "HPDinterval")

rep_sum <- tidyMCMC(jm_rep,
                    conf.int = TRUE,
                    conf.method = "HPDinterval")
pred <- cbind.data.frame(et2, rep_sum)

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
  annotate("text", label = "R^2==0.64",
           x = -1, y = 4.5,
            parse = TRUE) +
  scale_x_continuous("Observed ET LRR") +
  scale_y_continuous("Predicted ET LRR") +
  theme_bw(base_size = 12) +
  coord_equal()


# Plot parameters
pop <- param_sum %>%
  filter(grepl("mu\\.maxy", term) |
           grepl("mu\\.peakt", term)) %>%
  tidyr::separate(term, c("level", "variable"))

pulse <- param_sum %>%
  filter(grepl("^maxy", term) |
           grepl("^peakt", term)) %>%
  tidyr::separate(term, c("variable", "pulse"))

ggplot() +
  geom_pointrange(data = pulse, 
                  aes(x = variable,
                      y = estimate,
                      ymin = conf.low,
                      ymax = conf.high,
                      color = factor(pulse)),
                  position = position_dodge(width = 1),
                  alpha = 0.5) +
  geom_pointrange(data = pop, 
                  aes(x = variable,
                      y = estimate,
                      ymin = conf.low,
                      ymax = conf.high),
                  size = 1) +
  scale_y_continuous("Posterior mean") +
  facet_wrap(~variable,
             scales = "free") +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  guides(color = "none")
