### Saving files  ####
saveRDS(all_episodes_TNG, "data/all_episodes_TNG.Rdata")

readr::write_csv(all_episodes_TNG, "data/all_episodes_TNG.csv")
