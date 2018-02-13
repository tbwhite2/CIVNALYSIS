library(data.table)
api_key <- "GbCx5cmXSsPr7N4oAjI4Ub0C6"
api_secret <- "3xfJdbQpG2RVbZiOd1xz2abLtQdU6QAfyKKTKMvhtfem21Ao7H"
access_token <- "4919608432-k0bdpcBOA8qnJPoP63TCl1W5cHHYCCnKEluFUTy"
access_token_secret <- "sS8RLfvQuq7VjJs7QNL3BOA20FRrlQzVjm7aYsWt1vF5S"
#custom function to get tweets by a username
# get_user_tweets = function(user, n, api_key, api_secret, access_token, access_token_secret) {
  #set up oauth
  auth = httr::oauth_app("twitter", key=api_key, secret=api_secret)
  sig  = httr::sign_oauth1.0(auth, token=access_token, token_secret=access_token_secret)
  user <- "CivGame"
  # user <- "GreatWhiteBryan"
  n_to_get <- 100
  
  tweet_info_grabber <- function(new_tweet_id){
    GETurl <- paste0("https://api.twitter.com/1.1/statuses/show.json?trim_user=true&id=",new_tweet_id)
    
    one_tweet = httr::GET(GETurl, sig)
    one_tweet_content = httr::content(one_tweet)
    if(is.null(one_tweet_content$errors)){
      data.table(
        
        tweet_tid = one_tweet_content$id_str,
        tweet_uid = one_tweet_content$user$id_str,
        tweet_time = as.POSIXct(one_tweet_content$created_at, format = "%a %b %d %H:%M:%S %z %Y"),
        tweet_text = one_tweet_content$text,
        tweet_in_reply_tid = ifelse(!is.null(one_tweet_content$in_reply_to_status_id_str),one_tweet_content$in_reply_to_status_id_str,NA),
        tweet_in_reply_uid = ifelse(!is.null(one_tweet_content$in_reply_to_user_id_str),one_tweet_content$in_reply_to_user_id_str,NA),
        tweet_fav = one_tweet_content$favorite_count,
        tweet_rt = one_tweet_content$retweet_count
      )
    }else{
      NULL
    }
  }
  
  timeline_filler <- function(timeline){
  # timeline <-  timelineContent
    for(i in 1:length(timeline)){
      # i <- 2
      cat(i,"\n")
      j <- 1
      
      new_tweet_list <- list()
      
      new_tweet <- data.table(
        tweet_tid = timeline[[i]]$id_str,
        tweet_uid = timeline[[i]]$user$id_str,
        tweet_time = as.POSIXct(timeline[[i]]$created_at, format = "%a %b %d %H:%M:%S %z %Y"),
        tweet_text = timeline[[i]]$text,
        tweet_in_reply_tid = ifelse(!is.null(timeline[[i]]$in_reply_to_status_id_str),timeline[[i]]$in_reply_to_status_id_str,NA),
        tweet_in_reply_uid = ifelse(!is.null(timeline[[i]]$in_reply_to_user_id_str),timeline[[i]]$in_reply_to_user_id_str,NA),
        tweet_fav = timeline[[i]]$favorite_count,
        tweet_rt = timeline[[i]]$retweet_count
      )
      if (!exists("tweet_dt") || !(new_tweet$tweet_tid %in% tweet_dt$tweet_tid)){
        
        new_tweet_list[[j]] <- new_tweet
        
        flag <- T
        while (flag) {
          if (!is.na(new_tweet$tweet_in_reply_tid)) {
            if (!exists("tweet_dt") ||
                !(new_tweet$tweet_in_reply_tid %in% tweet_dt$tweet_tid)){
              
              j <- j + 1
              new_tweet <- tweet_info_grabber(new_tweet$tweet_in_reply_tid)
              if(!is.null(new_tweet)){
                new_tweet_list[[j]] <- new_tweet
                flag <- !is.na(new_tweet$tweet_in_reply_tid)
              }else{
                flag <- F
              }
              
            } else{
              flag <- F
            }
          }else{
            flag <- F
          }
          
        }
        
        new_tweet_dt <- rbindlist(new_tweet_list)
        if(exists("tweet_dt")){
          tweet_dt <- rbind(tweet_dt, new_tweet_dt)
        }else{
          tweet_dt <- new_tweet_dt
        }
      }
    }
    tweet_dt
  }
  
  #Get Timeline
  GETurl    = paste0("https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=",
                     user,"&count=", n_to_get,"&exclude_replies=false&include_rts=true")
  timelineRaw = httr::GET(GETurl, sig)
  timelineContent = httr::content(timelineRaw)
  
  #Get Timeline Tweets
  tweet_dt <- timeline_filler(timelineContent)
  
  tweet_dt[,tweet_time := as.POSIXct(tweet_time, format = "%a %b %d %H:%M:%S %z %Y")]
  
  #Find earliest tweet so far
  min_civ_tweet <- tweet_dt[tweet_dt[tweet_uid == 38705128,.I[tweet_time == min(tweet_time)], tweet_uid]$V1]
  

  #Go further back in timeline
  GETurl    = paste0("https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=",
                     user,"&count=", n_to_get,"&exclude_replies=false&include_rts=true&max_id=", min_civ_tweet$tweet_tid)
  timelineRaw = httr::GET(GETurl, sig)
  timelineContent = httr::content(timelineRaw)
  #MOARE timeline tweets
  new_tweet_dt <- timeline_filler(timelineContent)
  new_tweet_dt[,tweet_time := as.POSIXct(tweet_time, format = "%a %b %d %H:%M:%S %z %Y")]
  
  tweet_dt <- rbind(tweet_dt, new_tweet_dt)
  
  tweet_dt <- unique(tweet_dt)
  

  #Find earliest tweet
  min_civ_tweet <- tweet_dt[tweet_dt[tweet_uid == 38705128,.I[tweet_time == min(tweet_time)], tweet_uid]$V1]
  
  GETurl    = paste0("https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=",
                     user,"&count=", n_to_get,"&exclude_replies=false&include_rts=true&max_id=", min_civ_tweet$tweet_tid)
  timelineRaw = httr::GET(GETurl, sig)
  timelineContent = httr::content(timelineRaw)
  
  new_tweet_dt <- timeline_filler(timelineContent)
  new_tweet_dt[,tweet_time := as.POSIXct(tweet_time, format = "%a %b %d %H:%M:%S %z %Y")]
  
  tweet_dt <- rbind(tweet_dt, new_tweet_dt)
  
  tweet_dt <- unique(tweet_dt)
  
  
  
  saveRDS(tweet_dt,"tweet_dt.RDS")
  
  
  # heck <- timeline_filler(timelineContent[1:10])
  # ids_test <- unlist(lapply(timelineContent[1:10],function(x){x$id_str}))
  # ids_test %in% tweet_dt$tweet_tid
  # saveRDS(tweet_dt,"tweet_dt.RDS")
  # 
  # 
  # as.POSIXct("Thu Dec 21 21:51:56 +0000 2017", format = "%a %b %d %H:%M:%S %z %Y")
  
  # tweet_dt <- rbind(tweet_dt, new_tweet_dt)
  # tweet_dt_list[[i]] <- new_tweet_dt 
  # 
  # response_tweet <- tweet_info_grabber(new_tweet$tweet_in_reply_tid)
  # 
  # base_tweet <- tweet_info_grabber(response_tweet$tweet_in_reply_tid)
  # # some_guy_tid <- timelineContent[[1]]$in_reply_to_status_id_str #some guy's tweet @civ
  # # some_guy_name <- timelineContent[[1]]$in_reply_to_screen_name
  # # some_guy_uid <- timelineContent[[1]]$in_reply_to_user_id_str
  # new_tweet_id <- new_tweet$tweet_tid[[1]][[1]]
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # one_civ_tweet_content$text
  # original_civ_tweet <- one_civ_tweet_content$in_reply_to_status_id_str
  # GETurl <- paste0("https://api.twitter.com/1.1/statuses/show.json?trim_user=true&id=",original_civ_tweet)
  # original_civ_tweet = httr::GET(GETurl, sig)
  # original_civ_tweet_content = httr::content(original_civ_tweet)
  # 
  # 
  # 
  # 
  # lapply(timelineContent, function(x){x$created_at})
  # one_civ_tweet_id <- "960528520106541056" #ZULU Tease
  # 960941139091111936
  # GETurl <- paste0("https://api.twitter.com/1.1/statuses/show.json?trim_user=true&id=",960936163698663425)
  # one_civ_tweet = httr::GET(GETurl, sig)
  # one_civ_tweet_content = httr::content(one_civ_tweet)
  # civ_user_id <- one_civ_tweet_content$user$id_str
  # 
  # GETurl <-"https://api.twitter.com/1/related_results/show/960586559106859008.json?include_entities=1"
  # test = httr::GET(GETurl, sig)
  # one_civ_tweet_content = httr::content(test)
  # 
  # 
  # 
  # 
  # GETurl <- paste0("GET https://api.twitter.com/1.1/statuses/show.json?user_id=",civ_user_id)
  # about_civ_tweets = httr::GET(GETurl, sig)
  # one_civ_tweet_content = httr::content(one_civ_tweet)
  # 
  # 
  # GETurl <- paste0("GET https://api.twitter.com/1.1/statuses/retweeters/ids.json?id=",
  #                  one_civ_tweet_id,
  #                  "&count=100&stringify_ids=true")
  # 
  # GETurl <- "GET https://api.twitter.com/1.1/statuses/retweets/509457288717819904.json"
  # rt_civ_tweet = httr::GET(GETurl, sig)
  # one_civ_tweet_content = httr::content(one_civ_tweet)
  # # install.packages("twitteR")
  # # library(twitteR)
  # # 
  # # # Change the next four lines based on your own consumer_key, consume_secret, access_token, and access_secret. 
  # # consumer_key <- "OQMbUsBfWQ1mVUGASpSArbG33"
  # # consumer_secret <- "GQ5kc0BlwJZE2FYyvv8cxn845z32ES6HsID87cawkQ075jwyIy"
  # # access_token <- "4338966852-lBmLvEg9mADHIdjK2hT4W5mtHmI9jRKxcV4PTrB"
  # # access_secret <- "AwKRZw9AvTMvMrb2jouX5JHTjDASI3zeceVsemgQa1SSq"
  # # 
  # # setup_twitter_oauth(api_key, api_secret, access_token,access_token_secret)
  # # tw = twitteR::searchTwitter('#realDonaldTrump + #HillaryClinton', n = 1e4, since = '2016-11-08', retryOnRateLimit = 1e3)
  # # d = twitteR::twListToDF(tw)