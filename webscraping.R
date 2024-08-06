#!/usr/bin/env Rscript

# PROJECT:  Teacher Job Postings
# PROGRAM:  Web-Scrape School Spring Job Postings (new website)
# AUTHOR:   Andrew Camp

# ========================================================================================== (0) Preamble ====
# Load helper functions
source("01_code/001_scrape_school_spring_helpers.R")

# ===================================================================================== (1) Get job links ====
# Getting all unique links at once and sleeping to avoid a timeout
job_list = get_links(state = "All states")

# =================================================================== (2) Compare with most recent scrape ====
prior_scrapes = fileSnapshot("02_data/scraped")$info
prior_scrapes = prior_scrapes[grep("_school_spring.FEATHER$", rownames(prior_scrapes)), ]


if (nrow(prior_scrapes) == 0 ) {
  message("No old scrapes found. Starting fresh")
  
  to_scrape_list = job_list
  rm(job_list)
  
  newscrape = TRUE
}

if (difftime(Sys.time(), prior_scrapes[which.max(prior_scrapes$mtime),]$mtime, units = "days") <= 7) {
  
  # First load in prior scrape
  last_scrape = read_feather(paste0("02_data/scraped/",
                                    rownames(prior_scrapes[which.max(prior_scrapes$mtime),])))
  last_scrape = last_scrape[, job_id := as.integer(job_id)]
  last_scrape = last_scrape[update_status != "Job no longer posted"]
  
  # Next creating the data tables of new job ID to scrape/keep/list as remove
  to_scrape_list = job_list[!(jobId %in% last_scrape$job_id)]
  
  to_keep_list = last_scrape[job_id %in% job_list$jobId]
  to_remove_list = last_scrape[!(job_id %in% job_list$jobId)]
  
  message(paste("Updating last scrape with", 
                nrow(to_scrape_list), "new postings,", 
                nrow(to_keep_list), "previously scraped postings, and",
                nrow(to_remove_list), "removed postings."))
  
  newscrape = FALSE
  
}

if (difftime(Sys.time(), prior_scrapes[which.max(prior_scrapes$mtime),]$mtime, units = "days") > 7) {
  
  to_scrape_list = job_list
  rm(job_list)
  
  newscrape = TRUE
  
  message("Last scrape more than 7 days old. Starting fresh")
  
}

# ===================================================================== (3) Scraping individual job pages ====
# Calling and timing the scrape helper function helper function
jobs = vector("list", nrow(to_scrape_list))
system.time({
  
  # Creating a progress bar
  message(paste("Scraping", nrow(to_scrape_list), "job listings from School Spring"))
  pb = txtProgressBar(min = 0, max = nrow(to_scrape_list), style = 3)
  
  # Using for loop
  for (i in 1:nrow(to_scrape_list)) {
    
    # Getting scrape
    jobs[[i]] = scrape(jid = to_scrape_list[i, ]$jobId,
                       jlat = to_scrape_list[i, ]$lat,
                       jlong = to_scrape_list[i, ]$lng)
    
    # Sleeping to avoid a timeout
    setTxtProgressBar(pb, i)
    Sys.sleep(runif(1, min = 0, max = 0.1))
    
  }
  
  close(pb)
})




# ========================================================================== (4) Combining and saving out ====
#** Some very light cleaning
jobs = rbindlist(jobs, fill = TRUE)
jobs = janitor::clean_names(jobs)

#** Setting the update status variable and re-combining with previous scrapes
if (newscrape == TRUE) {
  jobs = jobs[, update_status := "Newly scraped"]

} else {
  
  to_keep_list = to_keep_list[, update_status := "Still posted, not scraped"]
  to_remove_list = to_remove_list[, update_status := "Job no longer posted"]
  jobs = jobs[, update_status := "Newly scraped"]
  
  jobs = rbind(jobs, to_keep_list, to_remove_list, fill = TRUE)
  
}

#** Adding in date scraped and update status variables
jobs = jobs[, date_scraped := Sys.time()]

# Saving out
today = Sys.Date() 
today = gsub("-", "_", today)

path <- paste0('02_data/scraped/', today, "_school_spring.FEATHER")
write_feather(jobs, path)

# cleaning everything out
rm(list = ls())


