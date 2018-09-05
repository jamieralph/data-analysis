
##### splitting the beta hint into it's subgroups.
library(tidyverse)


folder = "C:/Users/reedm/UK Parliament/THOMAS, Liz - DAs/Hints/" 
allMatches = read_csv(paste0(folder, "AllHintsMatchedCLeaner.csv") )


beta <- subset(allMatches, ruleName == "beta") %>%
         mutate(extract = str_extract(uri,"(?<=\\.uk\\/)[^\\/]+(?=\\/|$)"))

beta2 <- beta %>% 
            group_by(section = extract) %>% 
            summarise(count=n()) %>% 
            arrange(desc(count)) %>%
            transmute(section = section %>% fct_reorder(count), count)
          

ggplot(beta2,aes(section,count)) + geom_bar(stat="identity")+coord_flip()



newRules <- tibble(rule = paste0(beta$regex[1],"\\/",beta2$section),ruleName = beta2$section %>% as.character) %>% 
          mutate(ruleName = gsub("-"," ", ruleName))
# get rid of the NA match and replace with a homepage match.
newRules[is.na(newRules$ruleName),] = c(paste0(beta$regex[1],"\\/$"),"beta homepage")



# testing the newRules are correct. This method could also replace the current regex matcher - it's a little more succinct and uses the tidy packages.
m <- crossing(uri = beta$uri, rule = newRules$rule) %>% 
  filter(str_detect(uri, rule)) %>%
  inner_join(newRules)



m %>% group_by(rule) %>%
  summarize(count=n()) %>%
  ggplot(aes(rule %>% reorder(count), count ))+ geom_bar(stat="identity")+coord_flip()


refLocation = "C:/Users/reedm/UK Parliament/Data & Search - Data and Search (Public)/Search/Hints/ReferenceHints.csv"
referenceHints <- refLocation %>% read_csv %>%
  bind_rows(newRules) %>%
  filter(ruleName != "beta") %>%
  unique()
write_csv(referenceHints , refLocation)





