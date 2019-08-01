library(rvest)
library(Rcrawler)
library(dplyr)

urlstr <- 'http://stats.espncricinfo.com/ci/engine/player/604302.html?class=2;template=results;type=batting;view=innings'

main.page <- read_html(urlstr)

urls <- main.page %>% # feed `main.page` to the next step
  html_nodes(".data1 a") %>% # get the CSS nodes
  html_attr("href") # extract the URLs

# Combine `links` and `urls` into a data.frame
sotu <- data.frame(urls = urls, stringsAsFactors = FALSE)
head(sotu)

#sotu1 <- data.frame(subset(sotu, urls %in% c("match")))

#sotu2 <- filter(sotu, urls %in% c("match"))

df2 <- filter(sotu, grepl("match", urls))

df2 <- paste0("http://www.espncricinfo.com", df2$urls)

params <- lapply(df2, function(x) names(RCurl::getFormParams(x)))
df3 <- unique(unlist(params))

runs <- ContentScraper(Url=df3, CssPatterns=".cscore_score", ManyPerPattern = TRUE)

#over-circle high-score

List <- lapply(runs, '[[', 1)  # This returns a list with only the third element
sp1 <- unlist(lapply(List, '[[', 1))
sp2 <- unlist(lapply(List, '[[', 2))

final1 <- as.numeric(sub("\\/.*", "", sp1))
final2 <- as.numeric(sub("\\(.*|\\/.*", "", sp2))

score1 <- 1:length(final1)
score2 <- 1:length(final1)

agg <- readHTMLTable(urlstr, which = 4, header = TRUE)

agg$Runs <- as.numeric(gsub("[^0-9]", "", agg$Runs))
agg$Inns <- as.numeric(gsub("[^0-9]", "", agg$Inns))

final <- cbind("Runs"=agg$Runs, "Inns"=agg$Inns, "Bat1"=final1, "Bat2"=final2, "Score1"=score1, "Score2"=score2)

final[,'Score1'] <- ifelse(final[,'Inns']==1, final[,'Bat1'], final[,'Bat2'])

final[,'Score2'] <- ifelse(is.na(final[,'Runs']), NA, final[,'Score1'])

cont1 <- sum(final[,'Runs'], na.rm = TRUE)/sum(final[,'Score1'], na.rm = TRUE)
print(paste("Contribution:",round((cont1*100),2),"%"))

cont2 <- sum(final[,'Runs'], na.rm = TRUE)/sum(final[,'Score2'], na.rm = TRUE)
print(paste("Contribution_Adjusted:",round((cont2*100),2),"%"))

#write.csv(final, file = "final.csv", row.names=FALSE, na="", sep=",")
