

rules$row = row.names(rules)
m<- merge(strings, rules, stringsAsFactors=F)
m[]<-lapply(m,as.character)
m['match']<-str_detect(m$x, m$rule)
m <- m[m$match == TRUE,]

