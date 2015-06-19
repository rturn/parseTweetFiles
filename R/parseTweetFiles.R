#' Handles link symbols
#'
#' Removes all punctuation from tweet text while also removing the protocol (http) and domain (.com) indicators from 
#' any links in the text.
#' 
#' @param x A string or array of strings to remove symbols from
#' 
#' @return The string or array of strings with the symbols removed
#' @examples
#' \dontrun{text = unlist(lapply(tweets$text, clean_links))}
#' 
#' @export
#' 
clean_links = function(x) {
  x = as.character(x)
  y = unlist(strsplit(x, "\\s+"))
  x = paste(y[!grepl("@|\\.com|\\.org|\\.net|http|RT|-", y)], collapse = " ") #Change these to remove different link artifacts
  x = gsub("&|/", " ", x) 
  x = gsub("[[:punct:]]", "", x)
  x = gsub("persuaded", "persuadex", x)
  x = gsub("u[0-9a-fA-F]{4}", "", x)
  x = gsub("persuadex", "persuaded", x)
}

#' Handles HTML expressions
#' 
#' Removes all html codes that represent symbols / puncation.
#' 
#' @param x A string or array of strings to remove symbols from
#' 
#' @return The string or array of strings with the symbols removed
#' 
#' @examples
#' \dontrun{text = filter_html(text)}
#' 
#' @export
#' 
filter_html = function(x) {
  x = as.character(x)
  x = sub("&amp;#039;", "'", x)
  x = sub("&amp;", "&", x)
  x = sub("&gt;", ">", x)
  x = sub("&lt;", "<", x)
  x = iconv(x, "latin1", "ASCII", sub = "")
  x = gsub("#|_|:|\"|,|'","",x)
  x = gsub("|", "", x, fixed = TRUE)
  x = gsub("&", "", x, fixed = TRUE)
  x = gsub(".", "", x, fixed = TRUE)
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
#' @return The tweet data frame with all editing / filtering done.
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
  geo_tweets = tweets.df
  
  if(!is.null(tz)) {
    geo_tweets$time_zone == as.character(geo_tweets$time_zone)
    if("NA" %in% tz) {
      temp = filter(tweets.df, is.na(time_zone))
    } 
    temp2 = filter(tweets.df, time_zone %in% tz)   
    temp = rbind(temp, temp2)
    if(length(temp[,1]) == 0) {
      return(temp)
    }
    geo_tweets = temp
  }
  
  #Filter links and html codes from text
  geo_tweets$text = filter_html(geo_tweets$text) 
  text = unlist(lapply(geo_tweets$text, clean_links))
  text = trim(text)
  text = tolower(text)
  geo_tweets$text = text
  linkless_tweets = geo_tweets
  
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
  untagged_tweets = filter(located_tweets, is.na(lon) | is.na(lat))
  tagged_tweets = filter(located_tweets, !is.na(lon) & !is.na(lat))
  
  if(nrow(tagged_tweets) == 0) {
    temp = rep(NA, length(located_tweets[,1]))
    edited_tweets = data.frame(located_tweets, temp)
    return(edited_tweets)
  }
  
  #Shapefile from https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html 
  areas = readShapeSpatial(system.file("extdata", "cb_2014_us_state_500k.shp", package = "parseTweetFiles"))
  
  #Convert the lon/lat points to coords
  points = data.frame(tagged_tweets$lon, tagged_tweets$lat)
  points = SpatialPoints(points)
  
  #Overlay the points into the polygons, mapping the coordinates to states
  statedata = over(points, areas)
  
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
#' files variables, runs clean_tweets on them, and runs locate_tweets on them if desired. 
#' 
#' @param tweetdir The file directory of tweets to read from. No files other than the tweet files should be in this 
#' directory. The tweet files should be in the format as provided by streamR's parseTweets.
#' @param outputdir The location to save the tweets to. 
#' @param loc Determines if the tweets should be zip code tagged. Boolean variable
#' @param vars The variables to filter from the tweet files.
#' @param ... tz and stoplist arguments for the inner clean.tweets call. Defaults both to false
#' 
#' @return The list of filenames edited.
#'         
#' @examples
#' \dontrun{process_files("~/Documents/RawTweets", "~/Documents/EditedTweets")}
#' \dontrun{process_files("~/Documents/RawTweets", "~/Documents/EditedTweets", loc = TRUE, 
#' vars = c("text", "time_zone"), tz = c("Jerusalem", "NA"), stoplist = stoplist)}
#' 
#' @export 
process_files = function(tweetdir, outputdir, loc = FALSE, vars = "text", ...) {
  filenames = dir(tweetdir)
  editedfns = paste(filenames, "e", sep = "")
  existing = dir(outputdir)
  
  if(length(existing) > 0) {
    filenames = filenames[-which(editedfns %in% existing)]
  }
  
  length = length(filenames)
  if(length == 0) {
    print("Tweet Editing Up To Date")
    return()
  }
  
  extras = list(...)
  for(i in 1:length) {
    filename = filenames[i]
    if(file.info(paste(tweetdir, filename, sep = "/"))$size != 0) {
      tweets.df = read.csv(paste(tweetdir, filename, sep = "/"), header = T, fileEncoding = "latin1")
      tweets.df = select(tweets.df, one_of(vars))
      etweets.df = clean_tweets(tweets.df, tz = extras$tz, stoplist = extras$stoplist) 
      if(loc == TRUE) {
        etweets.df = locate_tweets(etweets.df)
      }
      write.csv(x = etweets.df, file = paste(paste(outputdir, filename, sep = "/"), "e", sep = ""), row.names = FALSE)
    }
  }
  
  output = rep("", length(filenames))
  output[1] = filenames[1]
  for(i in 2:length) {
    output[i] = filenames[i]
  }
  return(output)
}

#' Create a data frame from multiple tweet files
#'
#' This function reads in a folder of tweet file then combines them all into one usable data frae
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
  df = read.csv(paste(tweetdir, filenames[1], sep = "/"), header = T, fileEncoding = "latin1")
  for(i in 2:length(filenames)) {
    filename = filenames[i]
    temp = read.csv(paste(tweetdir, filename, sep = "/"), header = T, fileEncoding = "latin1")
    df = rbind(df, temp)
  }
  return(df)
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