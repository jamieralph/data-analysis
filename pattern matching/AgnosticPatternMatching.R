

# with a set of patterns and names in a data frame names rules, and a vector of strings called strings,
# this code will produce a data frame with the regex and name and all the strings it matches with for 
# every match between the rules and the strings.

# in theory the for loop could be replaced with instead a ddply function, but this would make it uninterruptable.
# by using a loop, we can stop the process at any point and use what we have so far, potentially at the cost of speed.

library(plyr)

rules <- rules
strings = df2$uri 


bigList = list()
for (i in 1:nrow(rules)) 
{
  rule = rules$rule[i]
  ruleName = rules$ruleName[i]
  s = regmatches(strings, regexec(rule,strings) ) # get matches from strings to the rule - also getting parenthsised substrings
  groups = s[which(lengths(s) > 0)]
  if (sum(lengths(s)) > 0)
  {
    matches =  strings[which(lengths(s) > 0)]
    ruleNames = rep(ruleName, length(matches)) # so we have a vector of names to match matches
    
    if (grepl("\\{",ruleName)) # if the ruleName contains a number in curly braces, this indicates there is a parenthesised part of the rule,
    {                         # which can be substituted into the ruleName. The for loop extracts and replaces these values.
      for (j in 1:lengths(regmatches(ruleName, gregexpr("\\{", ruleName))))
      {
        replacements = sapply(groups, function(x)x[j+1])
        ruleNames = as.character(mapply(gsub, paste0("\\{",j,"\\}"), replacements, ruleNames)) # uses gsub to replace {j} with the replacements in ruleNames
      }
    }
    df = data.frame(strings=matches,ruleName=ruleNames,regex=rule)
    bigList[[paste(rule,"-",ruleName)]] = df # puts each set of matches into a list of data frames, which is rbinded at the end.
  }                               # doing the rbind here each time would take a long time by the end since it isn't too efficient.
  print(paste(rule,"-",ruleName))
}
output=do.call(rbind, bigList)



# below is the same function as above modified to use ddply, only it is slower so I haven't used it.
# time = c(Sys.time())
# a = ddply(head(rules,20), .(rule, ruleName), function(x)
#   {
#   s = regmatches(strings, regexec(x$rule,strings) )
#   groups = s[which(lengths(s) > 0)]
#   if (length(groups) == 0) return (NULL)
#   
#   matches =  strings[which(lengths(s) > 0)]
#   ruleNames = rep(x$ruleName, length(matches))
#   
#   if (grepl("\\{",x$ruleName))
#   {
#     for (j in 1:lengths(regmatches(x$ruleName, gregexpr("\\{", x$ruleName))))
#     {
#       replacements = sapply(groups, function(x)x[j+1])
#       ruleNames = as.character(mapply(gsub, paste0("\\{",j,"\\}"), replacements, ruleNames))
#     }
#   }
#   print(x$ruleName)
#   return(df = data.frame(strings=matches,ruleName=ruleNames,regex=x$rule))
# })
# time = c(time,Sys.time())

