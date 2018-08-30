


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

# the loop below needs a set of rules, consisting of regular expressions and desriptions (here called 'rules'),
# and a data frame with all the possible matches between a set of uri's and the regular expressions (here this is uniqueMatches)


hold = data.frame(uri = unique(uniqueMatches$uri))

uniqueUri = hold # uniqueUri counts how many hints have been assigned to each uri
coverage = 0
maximumCover = 10 # this is the limit which is imposed on uri's as to how many hints they can have.
thisOne = NULL
uniqueUri$coverage = 0
removed = c()
keeps = data.frame(Var1 = character(),Freq=numeric(),ruleName=character(), cumulative=numeric())
noDocTypesHold = subset(uniqueMatches,!(grepl("^\\\\\\.\\w+\\??\\$",regex)))

while (coverage < maxCover-0 )
{
  
  matchesbanned = subset(allMatches, uri %in% subset(uniqueUri, coverage >= maximumCover)$uri)
  
  x2=as.data.frame(table(subset(uniqueMatches, !(regex %in% matchesbanned$regex) & 
                                  !(regex %in% thisOne$regex) & !(uri %in% thisOne$uri) & !(regex %in% removed))$regex )) # get occurence frequency of each rule, where 
  # using the rule wouldn't lead to a link exceeding the maximumCover and has not already been used.
  if (nrow(x2) == 0)
    break
  thisRule = x2[x2$Freq == max(x2$Freq),][1,]
  thisRule=  merge(thisRule,rules, by.x="Var1",by.y="rule")[1,]
  
  if (!grepl("^\\\\\\.\\w+\\??\\$",thisRule$Var1))
    uniqueUri[uniqueUri$uri %in% subset(uniqueMatches, regex== as.character(thisRule$Var1) )$uri,]$coverage= 
    uniqueUri[uniqueUri$uri %in% subset(uniqueMatches, regex== thisRule$Var1 )$uri,]$coverage + 1
  
  # here we need to add the loop which checks for overlap, then makes a decision on whether the overlap is enough to be a concern or not
  #
  #noDocTypes = rbind( noDocTypes, subset(noDocTypesHold,regex %in% subset(keeps, !(Var1 %in% noDocTypes$regex))$regex &  
  #                regex %in% subset(keeps, !(ruleName %in% noDocTypes$ruleName))$ruleName  ))
  
  
  noDocTypes = subset(noDocTypesHold,regex %in% keeps$Var1 & ruleName %in% keeps$ruleName) #get all Matches with the currently selected rules
  
  #x3 = as.data.frame(table(noDocTypes$uri ))
  x3 = noDocTypes$uri[duplicated(noDocTypes$uri)]
    multiples = subset(noDocTypes, uri %in% x3)
  reOrder = (unique(multiples[,c("regex","ruleName")]))[order(as.character(unique(multiples$regex))),]
  matrix2 = matrix(ncol=length(unique(multiples$regex)),nrow=2 )
  
  if (nrow(reOrder) > 0)
    for (j in 1:nrow(reOrder))
    {
      i=reOrder$regex[j]
      coMatching = subset(noDocTypes, uri %in% subset(noDocTypes,regex == i)$uri & regex %in% reOrder$regex )
      t = as.data.frame(table(coMatching$regex  ))
      notThisRule = length(unique(subset(coMatching, regex != i)$uri))
      matrix2[,j] = c(max(t$Freq),notThisRule)
    }
  
  
  if (length(which(matrix2[2,]/matrix2[1,] > 0.5)) > 0 ){ # if there is something to be gotten rid of, essentially
    removed = c(removed, reOrder[which(matrix2[2,]/matrix2[1,] > 0.5),"regex"] ) # this method would probably break if there was more than one 
    uniqueUri[uniqueUri$uri %in% subset(uniqueMatches, regex %in% removed )$uri,]$coverage= 
      uniqueUri[uniqueUri$uri %in% subset(uniqueMatches, regex %in% removed )$uri,]$coverage - 1
    keeps = keeps[!keeps$Var1 %in% removed,]
    thisOne = thisOne[!(thisOne$regex %in% removed),]
  }
  
  if (!(thisRule$Var1 %in% removed)){ 
    thisOne = rbind(thisOne, subset(uniqueMatches, regex == thisRule$Var1 ))
    coverage = length(unique(thisOne$uri)) / length(unique(searchesAndFirstPage$uri))
    print(paste(coverage, "-",thisRule$Var1[1], thisRule$ruleName[1]  ))
    thisRule$cumulative = coverage
    keeps=rbind(keeps, thisRule)
  }
}


row.names(keeps) = NULL
keeps$Var1 = as.character(keeps$Var1)
keeps$name= paste(row.names(keeps),keeps$ruleName)

library(ggplot2)
ggplot(keeps, aes(x=reorder(name, cumulative), y=cumulative))+geom_point()+theme(axis.text.x= element_text(angle =20,hjust=1  ))+ylim(0,1)


# provides an almost optimal cover, except for 7 links which each have one hint, which has a big crossover with a different hint- 
# enough of a crossover that I decided not to keep these specific hints.

# this method is certainly not optimal - It may be the case that this much larger hint (pretty sure it is the hint for 'join committees)
# if we were to get rid of it, and istead introduce these rules and potentially others that could completely cover for it with less overlap.

# need tothink about a method that would solve this sort of problem?;


oldRules <- readLines(paste0("C:/Users/reedm/Downloads/", "Rules (2).tsv"))
a = laply(oldRules,function(x)strsplit(x,"\t"))
oldRules=data.frame(rule=sapply(a,function(x)x[1]),ruleName=sapply(a,function(x)x[2]))

nrow(oldRules[(tolower(paste(oldRules$rule, oldRules$ruleName)) %in% tolower(paste(keeps$Var1, keeps$ruleName))),])












