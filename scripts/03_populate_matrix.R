# Populate matrix of overlapping parameters
# Diagonal: # of studies with just single variable
# Above: # of studies with both variables
# Below: # of observations with both variables

library(readr)
library(dplyr)


# Import data
d_clean <- read_csv("./data_clean/Clean_record_info.csv") %>%
  mutate(study_pulse = paste0(Study.ID, "_", Pulse.ID),
         study_pulse_record = paste0(study_pulse, "_", 
                                     Time.relative.to.pulse, "_",
                                     Time.relative.to.pulse.unit))

str(d_clean)

# Important variables
variables <- c("WUE", "ET", "T", "Gs", "PWP",
               "ecosystemR", "abovegroundR", "belowgroundR",
               "NPP", "GPP", "Anet")

# Calculate diagonal
diag <- d_clean  %>%
  mutate(varType = factor(varType, levels = variables)) %>%
  group_by(varType) %>%
  summarize(n_record = n(),
            n_pulse = length(unique(study_pulse)),
            n_study = length(unique(Study.ID))) %>%
  filter(varType %in% variables) 

count_mat <- matrix(NA, 
                    nrow = length(variables),
                    ncol = length(variables))

for(i in 1:length(variables)) {
  for(j in 1:length(variables)) {
     if(i == j) { # diagonal
       count_mat[i, j] <- diag$n_record[i]
     } else if(i > j){ # lower left half
        o_study <- d_clean %>%
         filter(varType == variables[i] | varType == variables[j]) %>%
         group_by(Study.ID) %>%
         summarize(nvar = length(unique(varType))) %>%
         filter(nvar == 2) %>%
         nrow()
        count_mat[i, j] <- o_study
        # o_pulse <- d_clean %>%
        #   filter(varType == variables[i] | varType == variables[j]) %>%
        #   group_by(study_pulse) %>%
        #   summarize(nvar = length(unique(varType))) %>%
        #   filter(nvar == 2) %>%
        #   nrow() 

     } else if(j > i) { # upper right half
       o_record <- d_clean %>%
         filter(varType == variables[i] | varType == variables[j]) %>%
         group_by(study_pulse_record) %>%
         summarize(nvar = length(unique(varType))) %>%
         filter(nvar == 2) %>%
         nrow() 
       count_mat[i, j] <- o_record
     }
  }
}

# Convert to dataframe, label
count_df <- data.frame(count_mat) 
colnames(count_df) <- variables
rownames(count_df) <- variables

# write out
if(!dir.exists("data_clean/tables")) {
  dir.create("data_clean/tables")
}
write.csv(count_df, file = "data_clean/tables/matrix_study_record.csv")



