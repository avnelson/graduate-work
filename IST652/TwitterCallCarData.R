library(rtweet)
library(tidytext)

appname <- "Used Vehicle Search"
key <- "xvQsElcvL60oQHDCd3rY70gek"
secretkey <- "N4Weji6R4LBYwHvvE6s0IllmY7YKS1V9GLMSHlCGUd2RB5ThAa"
accesstoken <- "720693854144344064-v9R26G4SVo0hSKhDiKrjt6myX9JKczL"
accesssecret <- "ArYkMabaKc5CuTQrpl6v8vSccL5vkO6HG40uKEzbSr0re"

twitter.token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secretkey,
  access_token = accesstoken,
  access_secret = accesssecret)

#search tweets
toy86 <- search_tweets(
  "#toyota86", n = 1000, include_rts = FALSE, lang = "en"
)
toy86

subBRZ <- search_tweets(
  "#brz", n = 1000, include_rts = FALSE, lang = "en"
)
subBRZ

genesisCoupe <- search_tweets(
  "#genesiscoupe", n = 1000, include_rts = FALSE, lang = "en"
)
genesisCoupe

Mustang <- search_tweets(
  "#fordmustang", n = 1000, include_rts = FALSE, lang = "en"
)
Mustang

#to JSON
library(jsonlite)
Jtoy86 <- toJSON(toy86)
write(Jtoy86, file="/Users/avanelson/Desktop/IST652 Python/toy86.JSON")

JsubBRZ <- toJSON(subBRZ)
write(JsubBRZ, file="/Users/avanelson/Desktop/IST652 Python/subBRZ.JSON")

JgenCoupe <- toJSON(genesisCoupe)
write(JgenCoupe, file="/Users/avanelson/Desktop/IST652 Python/genCoupe.JSON")

Jmustang <- toJSON(Mustang)
write(Jmustang, file="/Users/avanelson/Desktop/IST652 Python/mustang.JSON")

#export tweet lists
write_as_csv(toy86,"/Users/avanelson/Desktop/IST652 Python/toy86.csv")
write_as_csv(subBRZ,"/Users/avanelson/Desktop/IST652 Python/subBRZ.csv")
write_as_csv(genesisCoupe,"/Users/avanelson/Desktop/IST652 Python/genesisCoupe.csv")
write_as_csv(Mustang,"/Users/avanelson/Desktop/IST652 Python/Mustang.csv")

#call twitter data
toy86 <- search_fullarchive(
  q= "lang:en
    #toyota86",
  n = 100,
  env_name = "search",
  safedir = NULL,
  parse = TRUE,
  token = twitter.token )
#--------------------------------
#--------------------------------
head(toy86)

d2017.07.29 <- data.frame(d2017.07.29)
write_as_csv(d2017.07.29, "/Users/avanelson/Desktop/IST 707 Data Analytics/Project/20170729.csv" )
