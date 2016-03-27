### Saving files  ####
saveRDS(all_episodes_TNG, "data/all_episodes_TNG.Rdata")

readr::write_csv(all_episodes_TNG, "data/all_episodes_TNG.csv")

# These don't work. 
# all_episodes_TNG <- load("data/all_episodes_TNG.Rdata")
# 
# source("data/all_episodes_TNG.Rdata")
all_episodes_TNG <- readRDS("data/all_episodes_TNG.Rdata")

saveRDS(TNG, "data/TNG.RData")
readr::write_csv(TNG, "data/TNG.csv")
