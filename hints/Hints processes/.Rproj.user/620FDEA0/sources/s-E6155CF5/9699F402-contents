

# after creating hints, this can check which hints are doubling up


AppliedHints = subset(allMatches, regex %in% keeps$Var1 )#& ruleName %in% keeps$ruleName)

x3 = as.data.frame(table(subset(unique(AppliedHints[,c("uri","regex","ruleName")]), !(grepl("^\\\\\\.\\w+\\??\\$",regex)))$uri ))
x3=x3[order(-x3$Freq),]


noDocTypes = subset(unique(AppliedHints[,c("uri","regex","ruleName")]), !(grepl("^\\\\\\.\\w+\\??\\$",regex)))

multiples = subset(noDocTypes, uri %in% subset(x3,Freq > 1)$Var1)

# how did i make the coocurrence matrix?

reOrder = (unique(multiples[,c("regex")]))[order(as.character(unique(multiples$regex)))]
matrix2 = matrix(ncol=length(unique(multiples$regex)),nrow=2 )
thisMatrix = matrix(nrow = length(unique(multiples$regex)), ncol=length(unique(multiples$regex) ),data=0)
for (j in 1:length(reOrder))
{
  i=reOrder[j]#$regex[j]
  coMatching = subset(noDocTypes, uri %in% subset(noDocTypes,regex == i)$uri & regex %in% reOrder)#$regex )
  t = as.data.frame(table(subset(noDocTypes, uri %in% subset(noDocTypes,regex == i)$uri & regex %in% reOrder#$regex
                                 )$regex  ))
  t= rbind(t, data.frame(Var1=unique(multiples$regex)[!(unique(multiples$regex) %in% t$Var1)],Freq = 0  ))
  t$Var1=as.character(t$Var1)
  t=t[order(t$Var1),]
  thisMatrix[j,]=t$Freq
  notThisRule = length(unique(subset(coMatching, regex != i)$uri))
  matrix2[,j] = c(max(t$Freq),notThisRule)
}
  
# do i look at percentage of total links that have overlap? Discount those with more than, say, 90% overlap, then redo the 
# analysis? At least at the moment they have only one overlap, but what if they had more than one? what if there were three tags
# on one 

#reOrder[c(21,22)]

# if i do this loop within the creatHintslist loop, then i could discount certain hints if adding them would create a large amount of overlap?
# alternatively, this method could remove previously added hints if their overalap becaome too large?




