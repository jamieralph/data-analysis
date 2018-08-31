

# with a set of patterns and names in a data frame names rules, and a vector of strings called strings,
# this code will produce a data frame with the regex and name and all the strings it matches with for 
# every match between the rules and the strings.

library(plyr)
library(stringr)

rules <- rules
strings = df2$uri 

rules$row = row.names(rules)
m<- merge(strings, rules, stringsAsFactors=F)
m[]<-lapply(m,as.character)
m['match']<-str_detect(m$x, m$rule)
m <- m[m$match == TRUE,]

