


folder = "C:/Users/reedm/UK Parliament/THOMAS, Liz - DAs/Hints/" 
publicFolder = "C:/Users/reedm/UK Parliament/Data & Search - Data and Search (Public)/Search/Result text/"
publicHintsFolder ="C:/Users/reedm/UK Parliament/Data & Search - Data and Search (Public)/Search/Hints/"

rules = read.csv(paste0(publicHintsFolder, "ReferenceHints.csv"), stringsAsFactors = F)
rules$rule <- gsub("\\?<filter>","" ,rules$rule)
rules[substr(rules$rule,1,1) == "(" & substr(rules$rule,nchar(rules$rule),nchar(rules$rule)) == ")",]$rule = # get rid of brackets at start and end
  substr(rules[substr(rules$rule,1,1) == "(" & substr(rules$rule,nchar(rules$rule),nchar(rules$rule)) == ")",]$rule,2,
         nchar(rules[substr(rules$rule,1,1) == "(" & substr(rules$rule,nchar(rules$rule),nchar(rules$rule)) == ")",]$rule)-1)

searchesAndFirstPage <- read.csv(paste0(publicFolder,"termsAndResults.csv"), stringsAsFactors = F)
searchesAndFirstPage$X = NULL


allMatches = read.csv(paste0(folder, "AllHintsMatchedCLeaner.csv") , stringsAsFactors = F)


maxCover = length(unique(allMatches$uri))/length(unique(searchesAndFirstPage$uri))


uniqueMatches = unique(allMatches[,c("uri","regex")])
uniqueMatches = merge(uniqueMatches, rules, by.x="regex", by.y="rule")
#uniqueMatches = subset(uniqueMatches, paste(regex, ruleName) %in% paste(rules$rule, rules$ruleName))

# the loop below needs a set of rules, consisting of regular expressions and desriptions (here called 'rules'),
# and a data frame with all the possible matches between a set of uri's and the regular expressions (here this is uniqueMatches)

#uniqueMatches = subset(uniqueMatches, regex %in% keeps$Var1)

hold = data.frame(uri = unique(uniqueMatches$uri))

uniqueUri = hold # uniqueUri counts how many hints have been assigned to each uri
coverage = 0
maximumCover = 10 # this is the limit which is imposed on uri's as to how many hints they can have.
thisOne = NULL
uniqueUri$coverage = 0
keeps1 = data.frame(Var1 = character(),Freq=numeric(),ruleName=character(), cumulative=numeric())

while (coverage < maxCover-0)
{
  matchesbanned = subset(uniqueMatches, uri %in% subset(uniqueUri, coverage >= maximumCover)$uri)
  
  x2=as.data.frame(table(subset(uniqueMatches, !(regex %in% matchesbanned$regex) & 
                                  !(regex %in% thisOne$regex) & !(uri %in% thisOne$uri) )$regex)) # get occurrence frequency of each rule, where 
                                # using the rule wouldn't lead to a link exceeding the maximumCover and has not already been used.
  
  thisRule = x2[x2$Freq == max(x2$Freq),][1,]
  thisRule=  merge(thisRule,rules, by.x="Var1",by.y="rule")[1,]
  
  if (!grepl("^\\\\\\.\\w+\\??\\$",thisRule$Var1))
  uniqueUri[uniqueUri$uri %in% subset(uniqueMatches, regex== as.character(thisRule$Var1) )$uri,]$coverage= 
  uniqueUri[uniqueUri$uri %in% subset(uniqueMatches, regex== thisRule$Var1 )$uri,]$coverage + 1
  if (!exists("thisOne"))
    thisOne = subset(uniqueMatches, regex == thisRule$Var1 )
  else
    thisOne = rbind(thisOne, subset(uniqueMatches, regex == thisRule$Var1 ))
  coverage = length(unique(thisOne$uri)) / length(unique(searchesAndFirstPage$uri))
  print(paste(coverage, "-",thisRule$Var1[1], thisRule$ruleName[1]  ))
  thisRule$cumulative = coverage
  keeps1=rbind(keeps1, thisRule)
}


row.names(keeps1) = NULL
keeps1$name= paste(row.names(keeps1),keeps1$ruleName)

library(ggplot2)
ggplot(keeps1, aes(x=reorder(name, cumulative), y=cumulative))+geom_point()+theme(axis.text.x= element_text(angle =20,hjust=1  ))+ylim(0,1)















