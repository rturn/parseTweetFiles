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
}

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

#' Removes all trailing / leading white space from a tweet.
#' 
#' @param x The tweet to remove white space from.
#' 
#' @return The string or array of strings with the white space removed
#' @examples
#' \dontrun{text = trim(text)}
#' 
#'
trim <- function (x) gsub("^\\s+|\\s+$", "", x) 

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

#' Performs all necessary cleaning on a data frame of tweets. This includes removing all symbols from tweets, converting
#' them to a lower case array of words, removing all stop words from this array, andconverting timestamps to an 
#' R usable format. Can also filter by time zone if desired (default does not filter)
#' 
#' @param tweets.df An array of tweets with desired variables attached. (Use dplyr to filter variables)
#' @param tz A list of time zones to filter by, currently case sensitive
#' @param stoplist The stoplist used to filter words 
#' 
#' @return The tweet data frame with all editing / filtering done.
#' 
#' @examples
#' \dontrun{df = select(rawdata, text, time_zone)}
#' \dontrun{tweets = clean.tweets(dataframe)}
#' \dontrun{tweets = clean.tweets(dataframe, tz = c("Pacific Time (US & Canada)", "Eastern Time (US & Canada)),
#' stoplist = stoplist))}
#' 
clean.tweets = function(tweets.df, tz = NULL, stoplist = NULL) {
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

#' Labels an array of geo-located tweets with zip codes using shapefiles provided by the Census Bureau. This function
#' should be run after cleaning the tweets, and only works on tweets with a latitude / longitude. Any tweets outside the
#' US are removed before returning the data frame. To run this function the shapefile needs to be downloaded and placed
#' in the working directory. The shapefile is made up of multiple files, and they can all be found at
#' https://www.census.gov/geo/maps-data/data/cbf/cbf_zcta.html
#' 
#' @param located_tweets A cleaned array of tweets with latitude / longitude variables.
#' 
#' @return The tweet data frame with all in US tweets labeled with a zipcode and all non-US tweets removed
#'         
#' @examples
#' \dontrun{tweets = clean.tweets(dataframe)}
#' \dontrun{located_tweets = locate.tweets(tweets)}
#' 
locate.tweets = function(located_tweets) {
  if(is.null(located_tweets$lat) || is.null(located_tweets$lon)) {
    print("Error, can't assign zip codes to tweets with no geo tags")
    return(located_tweets)
  }
  temp = filter(located_tweets, lon != "NA", lat != "NA")
  if(nrow(temp) == 0) {
    print("Error, all tweets non-geolocated")
    return(located_tweets)
  }
  located_tweets = temp
  
  #Shapefile from https://www.census.gov/geo/maps-data/data/cbf/cbf_zcta.html Every file in the folder is part of the shapefile
  #Read the polygons for the zip code areas
  areas = readShapeSpatial("cb_2013_us_zcta510_500k.shp")
  
  #Convert the lon/lat points to coords
  points = data.frame(located_tweets$lon, located_tweets$lat)
  points = SpatialPoints(points)
  
  #Overlay the points into the polygons, mapping the coordinates to zip codes
  zipdata = over(points, areas)
  
  #Filter the zip codes from the data frame, attach them to the tweets, and remove null zip codes
  zip = zipdata$ZCTA5CE10
  zipped_tweets = data.frame(located_tweets, zip)
  zipped_tweets = filter(zipped_tweets, zip != "NA")
  return(zipped_tweets)
}

#' Processes all tweet-files in a directory and saves them in the given directory. This functions filters each
#' files variables, runs clean.tweets on them, and runs locate.tweets on them if desired. 
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
#' \dontrun{process.files("~/Documents/RawTweets", "~/Documents/EditedTweets")}
#' \dontrun{process.files("~/Documents/RawTweets", "~/Documents/EditedTweets", loc = TRUE, 
#' vars = c("text", "time_zone"), tz = c("Jerusalem", "NA"), stoplist = stoplist)}
#'  
process.files = function(tweetdir, outputdir, loc = FALSE, vars = "text", ...) {
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
      etweets.df = clean.tweets(tweets.df, tz = extras$tz, stoplist = extras$stoplist) 
      if(loc == TRUE) {
        etweets.df = locate.tweets(etweets.df)
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
