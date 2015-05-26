This is the code for handling tweet files downloaded using the streamR package. This package prepares this files for future topic modeling, and removes undesired variables. If the time stamps are included, they are converted into an R friendly format. If coordinates are included, this package can also label each tweet with zip codes. 

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