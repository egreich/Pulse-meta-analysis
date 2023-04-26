#  Create plots by study/pulse for each response variable

library(tidyverse)

# Load data
load("models/model_input.Rdata") # out_list

if(!dir.exists("models/plots")){
  dir.create("models/plots")
}

for(vn in names(out_list)) {
  dfin <- out_list[[vn]]
  #dfin <- out_list[["ET"]]
  
  # Create study_pulse combination, create integer sID
  
  pulse_table <- dfin %>%
    expand(nesting(Study.ID, Pulse.ID)) %>%
    mutate(sID = as.numeric(factor(Study.ID))) %>%
    arrange(Study.ID) %>%
    tibble::rownames_to_column() %>%
    rename(pID = rowname) %>%
    mutate(pID = as.numeric(pID)) %>%
    relocate(pID, .after = sID)
  
  # Join with full table, restrict to 14 days after pulse, remove pre-pulse days
  df <- dfin %>%
    left_join(pulse_table) %>%
    relocate(sID, pID) %>%
    filter(Days.relative.to.pulse <= 14,
           Days.relative.to.pulse != -1)
  
  # Plot
  ggplot(df, aes(x = Days.relative.to.pulse + 1,
                 y = LRR)) +
    geom_errorbar(aes(ymin = LRR - sqrt(poolVar),
     ymax = LRR + sqrt(poolVar),
     color = as.factor(Pulse.ID)),
     width = 0,
     alpha = 0.25) +
    geom_point(aes(color = as.factor(Pulse.ID)),
               size = 0.75) +
    geom_vline(xintercept = 0, lty = 3) +
    geom_hline(yintercept = 0, lty = 2) +
    
    facet_wrap(~sID, scales = "free_y") +
    scale_y_continuous(paste0("LRR of ", vn)) +
    theme_bw() +
    guides(color = "none")
  
  ggsave(filename = paste0("models/plots/pulse_", vn, ".png"),
         height = 4,
         width = 6,
         units = "in")
  
}



