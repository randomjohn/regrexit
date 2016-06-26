library(RJSONIO)
library(ggplot2)
library(dplyr)

# download and process ----------------------------------------------------

# the petition data is in json format, so download to local and read in
json.file <- paste0("regrexit-",Sys.Date(),".json")
petition <- download.file("https://petition.parliament.uk/petitions/131215.json",destfile=json.file)
regrexit.log <- fromJSON(json.file)

# signatures by country ---------------------------------------------------

# convert signatures by country into a data frame
sig_by_country_m <- as.data.frame(do.call(rbind,regrexit.log$data$attributes$signatures_by_country))
sig_by_country_m$name <- as.character(sig_by_country_m$name)
sig_by_country_m$code <- as.character(sig_by_country_m$code)
sig_by_country_m$signature_count <- as.numeric(sig_by_country_m$signature_count)


# do some analysis --------------------------------------------------------

# top signers
sig_by_country <- sig_by_country_m %>% 
  arrange(desc(signature_count))

# bar chart of the top 10 signers excluding the UK
reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x))))
}
sig_by_country %>% slice(2:10) %>% mutate(name=factor(name,levels=name)) %>% ggplot() + geom_bar(aes(x=name,y=signature_count),stat="identity")+
  ggtitle("Percent of Regrexit Petition Signers by Country\nOnly non-UK in Top 10 Included") +
  xlab("Country") + ylab("Signature count")
sig_by_country %>% mutate(perc_count=signature_count/sum(signature_count)*100) %>% 
  slice(2:10) %>% mutate(name=factor(name,levels=name)) %>% ggplot() + geom_bar(aes(x=name,y=perc_count),stat="identity") +
  ggtitle("Percent of Regrexit Petition Signers by Country\nOnly non-UK in Top 10 Included") +
  xlab("Country") + ylab("Percent of signature count") 
