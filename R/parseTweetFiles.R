#' Text cleaner
#' 
#' Cleans all garbage symbols, links, retweet tags, etc from the tweet text.
#' 
#' @param x A string to remove symbols from
#' 
#' @return The string with the symbols removed
#' 
#' @examples
#' \dontrun{text = unlist(lapply(tweet.df$text, clean_text))}
#' 
#' @export
#' 
clean_text = function(x) {
  x = as.character(x)
  
  #Remove html tags
  x = gsub("<ed>", "", x)
  x = gsub("+", "", x, fixed = TRUE)
  #Remove emote leftovers
  x = gsub("<U[0-9a-fA-F]{4}>", "", x)
  y = unlist(strsplit(x, "\\s+"))
  #Change these to remove different links
  x = paste(y[!grepl("^@|\\.com$|\\.org$|\\.net$|^http|\\.edu$", y)], collapse = " ")
  x = gsub("RT", "", x, fixed = TRUE)
  
  #Remove odd symbols and punctuation
  x = sub("&amp;#039;", "", x)
  x = sub("&amp;", "", x)
  x = sub("&gt;", "", x)
  x = sub("&lt;", "", x)
  x = iconv(x, "UTF-8", "ASCII", sub = "")
  x = gsub("[[:punct:]]", "", x)
  x = gsub("\\s+", " ", x)
  return(x)
}

#' Handles leading / trailing white space.
#' 
#' Removes all trailing / leading white space from a tweet.
#' 
#' @param x The tweet to remove white space from.
#' 
#' @return The string or array of strings with the white space removed
#' @examples
#' \dontrun{text = trim(text)}
#' 
trim <- function (x) gsub("^\\s+|\\s+$", "", x) 

#' Formats timestamps
#' 
#' Remove extra information from dates and sort them
#' 
#' @param x The date of a tweet
#' 
#' @return The date in the proper format to convert to a posixt format
#' 
#' @examples
#' \dontrun{date = clean_dates(date)}
#' 
clean_dates = function(x) {
  x = paste(x[c(2,3,6,4)], collapse = ".")
}

#' Handles stopwords
#' 
#' Removes all words in an array that are on a given stoplist
#' 
#' @param word.vector An array of words to filter
#' @param stoplist The stoplist to filter with
#' 
#' @return The word array with all stopwords removed.
#' 
#' @examples
#' \dontrun{text = strsplit(text, " ")}
#' \dontrun{text = lapply(text, mywhich, stoplist)}
#' 
mywhich <- function(word.vector, stoplist) {
  word.vector = word.vector[!(word.vector %in% stoplist)]
  word.vector[which(word.vector != "")]
}

#' Cleans tweet data frames
#' 
#' Performs all necessary cleaning on a data frame of tweets. This includes removing all symbols from tweets, converting
#' them to lower case, removing all stop words, and converting timestamps to an R usable format. 
#' Can also filter by time zone if desired (default does not filter)
#' 
#' @param tweets.df An array of tweets with desired variables attached. (Use dplyr to filter variables)
#' @param tz A list of time zones to filter by, currently case sensitive
#' @param stoplist The stoplist used to filter words 
#' 
#' @return The tweet data frame with all editing / filtering done. Empty dataset 
#' 
#' @examples
#' \dontrun{df = select(rawdata, text, time_zone)}
#' \dontrun{tweets = clean_tweets(dataframe)}
#' \dontrun{tweets = clean_tweets(dataframe, tz = c("Pacific Time (US & Canada)", "Eastern Time (US & Canada)),
#' stoplist = stoplist))}
#' 
#' @export
#' 
clean_tweets = function(tweets.df, tz = NULL, stoplist = NULL) {
  Sys.setlocale('LC_ALL','C') 
  #Remove time zones not in tz, return an empty dataset if no tweets from that time zone
  tweets.df$time_zone == as.character(tweets.df$time_zone)
  if(!is.null(tz)) {
    tweets.df = dplyr::filter(tweets.df, time_zone %in% tz)   
    if(length(tweets.df[,1]) == 0) {
      return(tweets.df)
    }
  }
  
  #Filter links and html codes from text
  text = unlist(lapply(tweets.df$text, clean_text))
  text = trim(text)
  text = tolower(text)
  tweets.df$text = text
  linkless_tweets = tweets.df
  
  #convert factor back to character for future splitting
  linkless_tweets$text <- unlist(lapply(linkless_tweets$text, as.character))
  
  #Split the tweets into words and remove stopwords
  linkless_tweets$text = strsplit(linkless_tweets$text, " ")
  linkless_tweets$text = lapply(linkless_tweets$text, mywhich, stoplist)
  linkless_tweets$text = unlist(lapply(linkless_tweets$text, paste, collapse = " "))
  
  #Converting Time stamps into R standard
  if(!is.null(linkless_tweets$created_at)) {
    word_dates = strsplit(as.character(linkless_tweets$created_at), " ")
    word_dates = sapply(word_dates, clean_dates)
    word_dates = gsub(":", ".", word_dates)
    num_dates = as.POSIXct(strptime(word_dates, "%B.%d.%Y.%H.%M.%OS", tz = "GMT"))
    linkless_tweets$created_at = num_dates
  }
  return(linkless_tweets)
}

#' Matches tweets to zipcodes
#' 
#' Labels an array of geo-located tweets with state abbreviations using shapefiles provided by the Census Bureau. This function
#' should be run after cleaning the tweets, and only works on tweets with a latitude / longitude. Any tweets without
#' lattitude / longitude codes are labeled with NA.
#' 
#' @param located_tweets A cleaned array of tweets with latitude / longitude variables.
#' 
#' @return The tweet data frame with all in US tweets labeled with a state and all others with NA
#'         
#' @examples
#' \dontrun{tweets = clean_tweets(dataframe)}
#' \dontrun{located_tweets = locate_tweets(tweets)}
#' 
#' @export
locate_tweets = function(located_tweets) {
  if(is.null(located_tweets$lat) || is.null(located_tweets$lon)) {
    temp = rep(NA, length(located_tweets[,1]))
    edited_tweets = data.frame(located_tweets, temp)
    return(edited_tweets)
  }
  untagged_tweets = dplyr::filter(located_tweets, is.na(lon) | is.na(lat))
  tagged_tweets = dplyr::filter(located_tweets, !is.na(lon) & !is.na(lat))
  
  if(nrow(tagged_tweets) == 0) {
    temp = rep(NA, length(located_tweets[,1]))
    edited_tweets = data.frame(located_tweets, temp)
    return(edited_tweets)
  }
  
  #Shapefile from https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html 
  areas = maptools::readShapeSpatial(system.file("extdata", "cb_2014_us_state_500k.shp", package = "parseTweetFiles"))
  
  #Convert the lon/lat points to coords
  points = data.frame(tagged_tweets$lon, tagged_tweets$lat)
  points = sp::SpatialPoints(points)
  
  #Overlay the points into the polygons, mapping the coordinates to states
  statedata = sp::over(points, areas)
  
  #Filter the states from the data frame, attach them to the tweets
  state = statedata$STUSPS
  state_tweets = data.frame(tagged_tweets, state)
  state = rep(NA, length(untagged_tweets[,1]))
  temp = data.frame(untagged_tweets, state)
  edited_tweets = rbind(state_tweets, temp)
  return(edited_tweets)
}

#' Processes a folder of tweet files
#' 
#' Processes all tweet-files in a directory and saves them in the given directory. This functions filters each
#' files variables, removes symbols, emotes and stopwords, and tags tweets with zip codes if desired.
#' 
#' @param tweetdir The file directory of tweets to read from. No files other than the tweet files should be in this 
#' directory. The tweet files should be in the format as provided by streamR's parseTweets.
#' @param outputdir The location to save the tweets to. 
#' @param loc True if the tweets should be tagged with zip codes. Uses 2014 shapefile from census.gov 
#' @param vars The variables to filter from the tweet files.
#' @param makedf True if a dataframe containing all the edited tweets should be returned, false otherwise. Defaults
#' to true.
#' @param ... tz and stoplist arguments for the inner clean.tweets call.
#' 
#' @return The list of filenames edited.
#'         
#' @examples
#' \dontrun{process_files("~/Documents/RawTweets", "~/Documents/EditedTweets")}
#' \dontrun{process_files("~/Documents/RawTweets", "~/Documents/EditedTweets", loc = TRUE, 
#' vars = c("text", "time_zone"), tz = c("Eastern Time (US & Canada)", "NA"), stoplist = stoplist)}
#' 
#' @export 
process_files = function(tweetdir, outputdir, loc = FALSE, vars = "text", makedf = TRUE, ...) {
  #Fix a bug with reading some characters from a csv
  Sys.setlocale('LC_ALL','C') 
  filenames = dir(tweetdir)
  
  #Create the filenames for the edited files
  editedfns = paste(filenames, "e", sep = "")
  existing = dir(outputdir)
  
  #Don't edit files which have already been edited
  if(length(existing) > 0) {
    alreadyEdited = which(editedfns %in% existing)
    if(length(alreadyEdited) != 0) {
      filenames = filenames[-alreadyEdited]
    }
  }
  
  length = length(filenames)
  if(length == 0) {
    print("Tweet Editing Up To Date")
    return()
  }
  
  extras = list(...)
  
  if(makedf) {
    df = do.call("rbind", lapply(filenames, manage_tweet_df, tweetdir, outputdir, vars, extras, loc, makedf))
    return(df)
  }
  lapply(filenames, manage_tweet_df, tweetdir, outputdir, vars, extras, loc, makedf)
  return("Done with no errors")
}

manage_tweet_df = function(filename, tweetdir, outputdir, vars, extras, loc, makedf) {
  if(file.info(paste(tweetdir, filename, sep = "/"))$size != 0) {
    tweets.df = read.csv(paste(tweetdir, filename, sep = "/"), header = T)
    tweets.df = dplyr::select(tweets.df, one_of(vars))
    etweets.df = clean_tweets(tweets.df, tz = extras$tz, stoplist = extras$stoplist) 
    etweets.df = dplyr::filter(etweets.df, text != "")
    if(loc == TRUE) {
      etweets.df = locate_tweets(etweets.df)
    }
    write.csv(x = etweets.df, file = paste(paste(outputdir, filename, sep = "/"), "e", sep = ""), row.names = FALSE)
    if(makedf) {
      return(etweets.df)
    }
  }
  
  else {
    return(NULL)
  }
}

#' Create a data frame from multiple tweet files
#'
#' This function reads in a folder of tweet file then combines them all into one usable data frame
#' 
#' @param tweetdir The file directory containing the tweet files. No other files should be in this folder.
#' 
#' @return A dataframe containing every tweet.
#'
#' @examples
#' \dontrun{make.tweet.df("~/Documents/EditedTweets")} 
#'
#' @export
make_tweet_df = function(tweetdir) {
  filenames = dir(tweetdir)
  df = do.call("rbind", lapply(filenames, read_df_file, tweetdir))
  #  df = read.csv(paste(tweetdir, filenames[1], sep = "/"), header = T)
  #  for(i in 2:length(filenames)) {
  #    filename = filenames[i]
  #    temp = read.csv(paste(tweetdir, filename, sep = "/"), header = T)
  #    df = rbind(df, temp)
  #  }
  return(df)
}

read_df_file = function(filename, tweetdir) {
  return(read.csv(paste(tweetdir, filename, sep = "/"), header = T))
}

#' Fit multiple topic models to successive time intervals using the maptpx package on a time marked Tweet data frame. 
#' The number of topics will be chosen independently for each interval via Bayes. The model created will be saved to a 
#' maptpx_model folder in the current directory, and the model is also visualized using the LDAvis package and saved 
#' into a maptpx_vis folder also in the current directory. Both of these folders need to be created before running the
#' function. 
#' 
#' @param tweets.df The dataframe of time marked tweets to fit models to. Should have a column "created_at" with times in
#' the posixct format.
#' @param start.time The first time point to start model fitting.
#' @param difference The length of time for each time interval (in hours).
#' @param num.steps The number of time intervals to fit models to. 
#' @param topic.min The smallest number of topics to consider for each model. Defaults to 5.
#' @param topic.max The largest number of topics to consider for each model. Defaults to 55.
#' @param model.kill The number of models with decreasing bayes factor fit before choosing the best model. A lower number
#' tend to choose topic models with fewer topics, while a higher number may choose more topics. See the maptpx topics
#' function package for more details. Defaults to 3.
#' 
#' @return The number of topics chosen for each time interval
#'
#' @examples
#' \dontrun{time = as.POSIXct('2015-04-24 12:11', tz = "GMT")}
#' \dontrun{model_time_points(tweets.df, time, difference = 1.5, num.steps = 96, topic.min = 5, 
#' topic.max = 10, model.kill = 4)} 
#'
#' @export
model_time_points = function(tweets.df, start.time, difference, num.steps, topic.min = 5, 
                             topic.max = 55, model.kill = 3) {
  steps = difference*60*60*0:num.steps
  times = start.time + steps
  return(sapply(times, model_time_point, tweets.df = tweets.df, difference = difference, topic.min = topic.min,
                topic.max = topic.max, model.kill = model.kill))
}

model_time_point = function(start.time, tweets.df, difference, topic.min, topic.max, model.kill) {
  text = dplyr::filter(tweets.df, created_at >= start.time & created_at <= start.time+(difference*60*60))$text
  if(length(text) == 0) {
    print("No Tweets during this time period")
    return(0)
  }
  
  corp = tm::Corpus(tm::VectorSource(text))
  dtm = tm::DocumentTermMatrix(corp)
  
  dtm = tm::removeSparseTerms(dtm, 0.999)
  dtm.dense = as.matrix(dtm)
  doc.length = rowSums(dtm.dense)
  dtm = dtm[doc.length > 0,]
  testmap = maptpx::topics(dtm, K = c(topic.min:topic.max), kill = model.kill)
  save(testmap, file = paste("./maptpx_model/", start.time, testmap$K, "topic model", sep = " "))
  
  vocab = colnames(dtm)
  doc.length = doc.length[which(doc.length != 0)]
  term.frequency = colSums(dtm.dense)
  term.frequency = term.frequency[which(term.frequency != 0)]
  theta = testmap$omega
  phi = t(testmap$theta)
  
  json <- LDAvis::createJSON(phi = phi,
                     theta = theta, 
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)
  LDAvis::serVis(json, out.dir = paste("./maptpx_vis/", start.time, "maptpx", testmap$K, "topic model", sep = " "),
         open.browser = FALSE)
  
  return(testmap$K)
}

#' 5634 Tweets about Ferguson
#'
#' A dataset containing the text and metadata of 5634 tweets containing keywords concerning Ferguson or other
#' related incidents.
#'
#' @format A data frame with 5634 rows and 42 columns
#' 
#' @source Downloaded using the streamR filterstream function.
"fergusontweets"

#' 7585 Tweets from the US
#'
#' A dataset containing the text and metadata of 7585 tweets on any topic sent from the US.
#'
#' @format A data frame with 7585 rows and 42 columns
#' 
#' @source Downloaded using the streamR filterstream function.
"locatedtweets"

#' 655 random tweets
#'
#' A dataset containing the text and metadata of 655 on any topic sent from any location.
#'
#' @format A data frame with 655 rows and 42 columns
#' 
#' @source Downloaded using the streamR filterstream function.
"unlocatedtweets"


