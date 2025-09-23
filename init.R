# Run this script to process raw sealog export csv to cached RDS files

source("functions.R")

library(tidyverse)

#### Set additional search fields here

search.field.custom <- NULL

####

dir.create("cache")

cache.files <- list.files("cache", full.names = T)
sealog.files.path <- list.files("sealog", full.names = T, recursive = T)

if (any(duplicated(sealog.files.path))) {
    stop(paste0("Following files are duplicated: \n\n", paste(sealog.files.path[duplicated(sealog.files.path)], collapse = "\n"), collapse = ""))
}

# Extract cruise/dive informations

sealog.file.tbl <- sealog.files.path %>% str_split("/", simplify = T) %>% as_tibble()
cruises <- unique(sealog.file.tbl$V2) 
LUT.cruise_dive <- sealog.file.tbl$V3 %>% 
    str_split("_", simplify = T) %>% 
    as_tibble() %>% 
    bind_cols(data.frame(path = sealog.files.path)) %>%
    rename(cruise = V1, dive = V2) %>%
    select(-V3) %>%
    mutate(deployment = paste(cruise, dive, sep = "_"))
dives <- unique(LUT.cruise_dive$dive)

# Construct cache files

walk(1:nrow(LUT.cruise_dive), \(i) {
    cache.path <- paste0("cache/", paste(LUT.cruise_dive$deployment[i], "default.RDS", sep = "_"), collapse = "")
    if (cache.path %in% cache.files) {
        cat(cache.path, "already cached.\n")
        return()
    }
    
    sealog.tbl <- read.table(LUT.cruise_dive$path[i], header = T, sep = ",")
    sealog.tbl.4Search <- searchField.init(sealog.tbl, search.fields.user = search.field.custom)
    
    saveRDS(sealog.tbl.4Search, cache.path)
    cat(cache.path, "successfully cached.\n")
})

LUT.cruise_dive.2cache <- LUT.cruise_dive %>%
    mutate(
        cache_path = paste0("cache/", deployment, "_default.RDS")
    )

# Construct status report file
meta.replay <- youtube_yaml("replay-meta.yml") 

dive.replayAvailable <- meta.replay %>% 
    .$dive %>%
    unique()

LUT.cruise_dive.3replaycheck <- LUT.cruise_dive.2cache %>% 
    mutate(replay = ifelse(dive %in% dive.replayAvailable, T, F))

report <- list(
    cruises = cruises,
    dives = dives,
    cruise_dive.tbl = LUT.cruise_dive.3replaycheck,
    replayAvailable = dive.replayAvailable
)

saveRDS(report, "cache/status_report.RDS")
