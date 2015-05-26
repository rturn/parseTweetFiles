This is the code for handling tweet files downloaded using the streamR package. This package prepares this files for future topic modeling, and removes undesired variables. If the time stamps are included, they are converted into an R friendly format. If coordinates are included, this package can also label each tweet with zip codes. 

#Installation  

This package can be downloaded directly from github as follows:  

install.packages(c("qtl", "htmlwidgets", "devtools"))  
library(devtools)  
install_github("rturn/parseTweetFiles")  