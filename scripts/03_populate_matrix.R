# Populate matrix of overlapping parameters
# Diagonal: # of studies with just single variable
# Above: # of studies with both variables
# Below: # of observations with both variables

library(readr)
library(dplyr)


# Import data
d_clean = read_csv("./data_clean/Clean_record_info.csv")

str(d_clean)

# Important variables
variables <- c("WUE", "ET", "T", "Gs", "PWP",
               "ecosystemR", "abovegroundR", "belowgroundR",
               "NPP", "GPP", "Anet")
