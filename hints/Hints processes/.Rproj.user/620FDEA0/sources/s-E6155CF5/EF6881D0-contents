



x  = (unique(searchesAndFirstPage[,c("uri")]))
s  = subset(x,!(x %in% uniqueMatches$uri ))

unmatched = subset(searchesAndFirstPage,(uri %in% s) )

s2 = as.data.frame(table(unmatched$uri ))
s2 =s2[order(-s2$Freq),]
s2$cumsum = cumsum(s2$Freq)
s2$cumpercent = s2$cumsum/sum(s2$Freq)


bigString = paste(s2$Var1, collapse= " ")

wordMatches=  regmatches(unmatched$uri,gregexpr("\\w+",unmatched$uri))

countOfWords = as.data.frame(table(unlist(wordMatches)))
countOfWords=countOfWords[order(-countOfWords$Freq),]




# (?<filter>commonslibrary\.parliament\.uk)	House of Commons Library
# https://www.parliament.uk/mps-lords-and-offices 
# services.parliament.uk/Calendar 
# mpsallowances.parliament.uk/  






