init_cache <- function(search.field.custom = NULL) {
    suppressMessages({
        require(tidyverse)
    })
    # Run this function to process raw sealog export csv to cached RDS files
    
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
}

searchField.init <- function(sealog.tbl, search.fields.user = NULL, search.options = F) {
    suppressMessages({
        require(stringdist)
        require(tidyverse)
    })
    sealog.fields <- colnames(sealog.tbl)

    if (search.options) {
        return(sealog.fields)
    }

    search.fields.default <- c(
        "event_value", "event_free_text", "event_option.comment", "event_option.notes"
    )
    
    if (!is.null(search.fields.user)) {
        search.fields <- c(search.fields.default, search.fields.user)
    } else {
        search.fields <- search.fields.default
    }
    
    search.fields.parseExpr <- paste("paste(", paste(search.fields, collapse = ", "), ", sep = ';')", collapse = "")
    
    if (!is.null(search.fields.user)) {
        # user custom construction call
        sealog.tbl.4Search <- sealog.tbl %>% 
        mutate(
            search.field.userSearch = search.fields.parseExpr %>% rlang::parse_expr() %>% rlang::eval_tidy()
        )
    } else {
        # default construction call
        sealog.tbl.4Search <- sealog.tbl %>% 
        mutate(
            search.field.defaultSearch = search.fields.parseExpr %>% rlang::parse_expr() %>% rlang::eval_tidy(),
            search.field.userSearch = NULL
        )
    }
    
    return(sealog.tbl.4Search)    
}

youtube_yaml <- function(path) {
    suppressMessages({
        require(stringdist)
        require(tidyverse)
    })
    youtube <- yaml::read_yaml(path) %>% bind_rows()
    
    youtube.cl <- youtube %>% 
        mutate(
            start_UTC = ymd_hms(start_UTC),
            length = hms::as_hms(length) %>% seconds_to_period(),
            ID = paste0("S0", dive, "P", part)
        ) %>% 
        mutate(
            end_UTC = start_UTC + length,
            dive_no = dive,
            dive = paste0("S", str_pad(dive, 4, "left", "0"))
        )
    
    return(youtube.cl)
}

timecode2link <- function(UTCs, youtube.cl) {
    suppressMessages({
        require(stringdist)
        require(tidyverse)
    })
    if (any(!is.POSIXct(UTCs))) {
        UTCs <- as.POSIXct(UTCs, format = "%Y-%m-%d %H:%M", tz = "UTC")
    } 
    if (any(is.na(UTCs))) {
        stop("Timecode in unknown format. Only '%Y-%m-%d %H:%M' accepted. E.g.: `2025-03-04 18:30`")
    }

    map_dfr(UTCs, \(UTC){
        bef <- UTC > youtube.cl$start_UTC
        aft <- UTC < youtube.cl$end_UTC
        ID <- youtube.cl$ID[as.logical(bef*aft)]
        ID <- ifelse(is.null(ID), "NA", ID)
        sec <- ifelse(ID == "NA", "NA", period_to_seconds(as.period(UTC - youtube.cl[youtube.cl$ID == ID,]$start_UTC)))
        data.frame(ID, sec)
        url <- ifelse(ID == "NA", "NA", paste0(youtube.cl[youtube.cl$ID == ID,]$url, "?t=", sec))
        res <- data.frame(ID, sec, url)
        if (is.na(res$url)) {
            res$url <- "Not streamed. URL unavailable."
        }
        return(res)
    })
}

searchSealog <- function(searchTxt, searchRange.cruise="all", searchRange.dive="all", expansion = F, exact = F, ambiguity.threshold = 0.2, case_sensitive = F) {
    suppressMessages({
        require(stringdist)
        require(tidyverse)
    })
    if (searchTxt == "") {
        return(list(status="error", content="Keyword can't be empty"))
    }
    status <- readRDS("cache/status_report.RDS")

    if ("all" %in% searchRange.cruise) {
        searchRange.cruise <- status$cruises
    } else {
        availabilityCheck <- searchRange.cruise %in% status$cruises
        if (any(!availabilityCheck)) {
            stop(paste0("Following cruises are not cached for search: \n\n", paste(searchRange.cruise[which(!availabilityCheck)], collapse = "\n")))
        }
    }
    if ("all" %in% searchRange.dive) {
        searchRange.dive <- status$dives
    } else {
        availabilityCheck <- searchRange.dive %in% status$dives
        if (any(!availabilityCheck)) {
            stop(paste0("Following dives are not cached for search: \n\n", paste(searchRange.dive[which(!availabilityCheck)], collapse = "\n")))
        }
    }

    searchRange.cruise_dive <- status$cruise_dive.tbl %>%
        filter(cruise %in% searchRange.cruise & dive %in% searchRange.dive)
    
    sealog.master <- map_dfr(searchRange.cruise_dive$cache_path, ~readRDS(.x))
    
    sealog.master$search.field.defaultSearch <- sealog.master$search.field.defaultSearch %>% 
        gsub(";", "", .) 

    if (!case_sensitive) {
        sealog.master$search.field.defaultSearch <- tolower(sealog.master$search.field.defaultSearch)
        searchTxt <- tolower(searchTxt)
    }
    
    ambiguity.allowance <- floor(nchar(searchTxt) * ambiguity.threshold)
    
    has.match <- function(searchField, searchTxt, ambiguity.allowance, expansion = F, exact = F){

        fullMatch <- ifelse(str_detect(searchField, searchTxt), T, F)
    
        if (exact) {
            return(ifelse(str_detect(searchField, regex(paste0("\\b", searchTxt, "\\b", collapse = ""))), T, F))
        }
        if (fullMatch) {return(T)}
        if (expansion) {
            expansion <- ambiguity.allowance
        } else if (!expansion) {
            expansion <- 0
        }
        
        dist.matrix <- map(1:(nchar(searchField)-nchar(searchTxt)+1), \(i) {
            str_sub_all(
                searchField, 
                start = i, 
                end = i+nchar(searchTxt)-1+expansion)
        }) %>% 
            unlist() %>% 
            stringdistmatrix(searchTxt) %>% 
            as.vector()
    
        ambiguitymatch <- dist.matrix <= ambiguity.allowance + expansion
        return(any(ambiguitymatch))
    }
    
    match.res <- map(1:nrow(sealog.master), \(i){
        searchField <- sealog.master$search.field.defaultSearch[i]
        ifelse(
            nchar(searchField) <= nchar(searchTxt), 
            stringdistmatrix(searchField, searchTxt) < ambiguity.allowance, 
            has.match(searchField, searchTxt, ambiguity.allowance, expansion, exact)
        )
    }) %>% unlist()

    sealog.match <- sealog.master[which(match.res),]
    
    if (nrow(sealog.match) == 0) {
        return(list(status="no_result", content="No result."))
    }
    
    return(list(status="exit", content=sealog.match))
}

searchres_youtube <- function(tbl, replay_meta_path) {
    suppressMessages({
        require(stringdist)
        require(tidyverse)
    })
    tbl.youtube <- bind_cols(
        tbl, 
        timecode2link(
            tbl$ts %>% gsub("T", " ", .) %>% gsub("\\.[0-9]{3}Z$", "", .) %>% as.POSIXct(tz = "UTC"), 
            youtube_yaml(replay_meta_path)
        )
    )
    return(tbl.youtube)
}

searchres_formatter <- function(tbl) {
    suppressMessages({
        require(stringdist)
        require(tidyverse)
    })
        tbl.format <- tbl %>%
            mutate(
                Cruise = cruise_id,
                Dive = lowering_id,
                Time = ts,
                `Event value` = event_value,
                `Event author` = event_author,
                `Event free text` = event_free_text,
                `Event option comment` = event_option.comment,
                `Event option notes` = event_option.notes,
                `Event option type` = event_option.type,
                `Event option subtype` = event_option.subtype,
                `Replay url` = url,
                .keep = "none"
            )
        return(tbl.format)
}

status_report <- function(path = "cache/status_report.RDS") {
    suppressMessages({
        require(stringdist)
        require(tidyverse)
    })
    status <- readRDS(path)

    list.cruise <- paste("## Available cruises", paste(paste("-", status$cruises, sep = " "), collapse = "\n"), sep = "\n\n")
    list.dives <- paste("## Available dives", paste(paste("-", status$dives, sep = " "), collapse = "\n"), sep = "\n\n")
    cat(list.cruise, list.dives, sep = "\n\n")
}
