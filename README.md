This is the code for handling tweet files downloaded using the streamR package. This package prepares this files for future topic modeling, and removes undesired variables. If the time stamps are included, they are converted into an R friendly format. If coordinates are included, this package can also label each tweet with zip codes. Finally, this package can also
be used to fit a series of topic models to a dataframe of time stamped Tweets.

###Installation

This package can be downloaded directly from github as follows:

install.packages(c("qtl", "htmlwidgets", "devtools"))  
library(devtools)  
install_github("rturn/parseTweetFiles")  

###Examples

For most uses, this package is easy to use. Just place all tweets you want to edit in one folder, and run the process.files functions. If necessary, clean.tweets and locate.tweets can be used independently.

```{r, eval = FALSE}
process.files("~/Documents/RawTweets", "~/Documents/EditedTweets")
process.files("~/Documents/RawTweets", "~/Documents/EditedTweets", loc = TRUE, 
vars = c("text", "time_zone"), tz = c("Jerusalem", "NA"), stoplist = stoplist)
```

For fitting topics models first a maptpx_model and maptpx_vis folder have to be created in the current directory. The data
frame to fit models to must have a created_at column with time stamps in the posixct format. After that the model fitting
is one command, and the output is saved to the created directories.

```{r, eval = FALSE}
step = 1.5
time = as.POSIXct('2015-04-24 12:11', tz = "GMT")
model_time_points(english.tweets.df, time, step, 96, 2, 20)
```
