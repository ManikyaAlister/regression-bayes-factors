rm(list = ls())

# load data 
data_raw <- read.csv("data/raw/Fitness and social cognition data.csv")

# Data to be analysed in fitness models  ----------------------------------

## List of all participants to remove
rm_participants_fitness <- c("P011", "P016", "P024", "P027", "P029", "P031", "P051", "P053", "P054", "P004", "P046")

## Remove rows for those participants 
data_fitness <- data_raw[! data_raw[,"p_number"] %in% rm_participants_fitness,]

# Save data 
save(data_fitness, file = "data/clean/data_fitness.Rdata")

# Data to be analysed in strength models  ---------------------------------

## List of all participants to remove
rm_participants_strength <- c("P011", "P016", "P024", "P027", "P029", "P031", "P051", "P053", "P054")

## Remove rows for those participants 
data_strength <- data_raw[! data_raw[,"p_number"] %in% rm_participants_strength,]

# Save data 
save(data_strength, file = "data/clean/data_strength.Rdata")

