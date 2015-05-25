# Helper Functions 

#Function to remove links, RT, @names
clean_links = function(x) {
  x = as.character(x)
  y = unlist(strsplit(x, "\\s+"))
  x = paste(y[!grepl("@|\\.com|\\.org|http|RT|-", y)], collapse = " ") #Change these to remove different links
  x = gsub("&|/", " ", x) 
  x = gsub("[[:punct:]]", "", x)
}

#filter html code and emoticons
filter.h = function(x) {
  x = as.character(x)
  x = sub("&amp;#039;", "'", x)
  x = sub("&amp;", "&", x)
  x = sub("&gt;", ">", x)
  x = sub("&lt;", "<", x)
  x = iconv(x, "latin1", "ASCII", sub = "")
  x = gsub("#|_|:|\"|,|'","",x)
  x = gsub("|", "", x, fixed = TRUE)
  x = gsub(".", "", x, fixed = TRUE)
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x) 

# Remove extra information from dates and sort them
clean_dates = function(x) {
  x = paste(x[c(2,3,6,4)], collapse = ".")
}

mywhich <- function(word.vector, stoplist) {
  word.vector = word.vector[!(word.vector %in% stoplist)]
  word.vector[which(word.vector != "")]
}


#Functions

#Variables must be selected before calling this
clean.tweets = function(tweets.df, tz, english.stoplist) {
geo_tweets = tweets.df

if(!is.null(tweets.df$time_zone) & tz) {
  geo_tweets = filter(tweets.df, time_zone != "<NA>")    #CHANGE HERE, put time_zone == "Central Time (US & Canada)" or other time zones
}

#Filter links and html codes from text
geo_tweets$text = filter.h(geo_tweets$text) 
text = unlist(lapply(geo_tweets$text, clean_links))
text = trim(text)
text = tolower(text)
geo_tweets$text = text
linkless_tweets = geo_tweets

#convert factor back to character for future splitting
linkless_tweets$text <- unlist(lapply(linkless_tweets$text, as.character))

#Split the tweets into characters and remove stopwords
linkless_tweets$text = strsplit(linkless_tweets$text, " ")
linkless_tweets$text = lapply(linkless_tweets$text, mywhich, english.stoplist)
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


#Converting tweets with lon/lat into tweets with zip codes, should clean before doing this
locate.tweets = function(located_tweets) {
library(sp)
library(dplyr)
library(maptools)
if(is.null(located_tweets$lat) || is.null(located_tweets$lon)) {
  print("Error, can't assign zip codes to tweets with no geo tags")
  return(NULL)
}
temp = filter(located_tweets, lon != "NA", lat != "NA")
if(nrow(temp) == 0) {
  print("Error, all tweets non-geolocated")
  return(located_tweets)
}
located_tweets = temp
#ShapeFile Assignment: More accurate and now done! 

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

#Filter Zip codes to codes only in a region
filter.zips = function(zipped_tweets, low, high) {
zipped_tweets$zip = as.numeric(as.character(zipped_tweets$zip))
wisconsin_tweets = filter(zipped_tweets, zip >= low & zip <= high)
return(wisconsin_tweets)
}

process.tweets = function(tweets.df, vars = c("text", "created_at", "lang", "time_zone", "lat", "lon"), stoplist = "", 
                          lan = "en", zip = FALSE, tz = FALSE, flan = TRUE) {
  library(plyr)
  library(dplyr)
  tweets = subset(tweets.df, select = vars)
  
if("lang" %in% vars & flan) {
  tweets = filter(tweets, lang %in% lan)
}

  tweets = clean.tweets(tweets, tz, stoplist)

if("lat" %in% vars & "lon" %in% vars & zip) {
  tweets = locate.tweets(tweets)
}

  return(tweets)
}

#Breakdown strings into one large word vector, look into LDA, pablo article, SQL, filter by english dictionary
# 2003, mixed model on google drive
process.files = function(tweetdir, outputdir, consoleout = FALSE) {
filenames = dir(tweetdir)
load("twitterFunctions.Rdata")
filenames = dir(tweetdir)
editedfns = paste(filenames, "e", sep = "")
existing = dir(outputdir)
if(length(existing) > 0) {
filenames = filenames[-which(editedfns %in% existing)]
}
length = length(filenames)
if(length == 0) {
  return("Tweet Editing Up To Date")
}
load("twitterFunctions.Rdata")
for(i in 1:length) {
  filename = filenames[i]
  if(file.info(paste(tweetdir, filename, sep = ""))$size != 0) {
  tweets.df = read.csv(paste(tweetdir, filename, sep = ""), header = T, fileEncoding = "latin1")
  etweets.df = process.tweets(tweets.df, tz = TRUE, stoplist = "")    #CHANGE HERE Arguments to clean tweets
  write.csv(x = etweets.df, file = paste(paste(outputdir, filename, sep = "/"), "e", sep = ""), row.names = FALSE)
  }
}
output = rep("", length(filenames))
output[1] = filenames[1]
for(i in 2:length) {
  output[i] = filenames[i]
}
if(consoleout) {
return(output)
}
}

save(clean_dates, clean_links, clean.tweets, filter.h, filter.zips, locate.tweets, process.tweets, process.files, trim, mywhich, file = "twitterFunctions.Rdata")
