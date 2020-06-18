# dependencies
library(dplyr)
library(rtweet)
library(purrr)

simprove_token <- function() {
  rtweet::create_token(
    "stats_twitter_bot",
    consumer_key = Sys.getenv("SIMPROVEBOT_CONSUMER_KEY"),
    consumer_secret = Sys.getenv("SIMPROVEBOT_CONSUMER_SECRET"),
    access_token = Sys.getenv("SIMPROVEBOT_ACCESS_TOKEN"),
    access_secret = Sys.getenv("SIMPROVEBOT_ACCESS_SECRET"),
    set_renv = FALSE
  )
}

# pull new tweets --------------------------------------------------------------

## search terms
hashtag <- "(#simprove OR #simulationstudy)"

## use since_id from previous search (if exists)
if (file.exists(file.path("data", "search.rds"))) {
  previous_tweets <- readRDS(file.path("data", "search.rds"))
  since_id <- previous_tweets$status_id[1]
} else {
  since_id <- NULL
}

## search for up to 100,000 tweets using the hashtag
simprove_tweets <- rtweet::search_tweets(
  hashtag,
  n = 1e5,
  verbose = FALSE,
  since_id = since_id,
  retryonratelimit = TRUE,
  include_rts = FALSE,
  token = simprove_token()
)

if (!is_empty(simprove_tweets)) {
  simprove_tweets <- distinct(simprove_tweets,
                                  status_id,
                                  .keep_all = TRUE)
}

# select tweets to retweet -----------------------------------------------------

# the chance of being retweeted is 0.95
retweet_true <- rbinom(NROW(simprove_tweets), 1, 0.95)
if (!all(retweet_true == 0)){
  tweets_to_retweet <- simprove_tweets[retweet_true == 1,]$status_id
} else {
  tweets_to_retweet <- NULL
}

# bind and save data -----------------------------------------------------------

if (!is_empty(simprove_tweets)) {
  ## if there's already a search data file saved, then read it in,
  ## drop the duplicates then update the data
  if (file.exists(file <- file.path("data", "search.rds"))) {

    ## bind rows with archive
    simprove_tweets <- do_call_rbind(
      list(simprove_tweets[, "status_id"], readRDS(file = file)))

    ## determine whether each observation has a unique status_id
    kp <- !duplicated(simprove_tweets$status_id)

    ## remove rows with duplicated status_ids
    statstwitter_tweets <- simprove_tweets[kp, ]
  }
  ## save shareable data (only status_ids)
  saveRDS(simprove_tweets[, "status_id"], file = file)
} else print("No new #SIMprove tweets out there")

# retweet tweets (all with a probability of 0.95 of getting retweeted) ---------
if (!is.null(tweets_to_retweet)) {
  walk(tweets_to_retweet, function(.x) {
    post_tweet(retweet_id = .x,
               token = simprove_token())
    Sys.sleep(20)
  })
} else print("No tweets selected")
