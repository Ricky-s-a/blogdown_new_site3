
# library -----------------------------------------------------------------

Packages <- c("tidyverse", "rtweet", "stringr","RMeCab", "wordcloud", "wordcloud2")
lapply(Packages, library, character.only = TRUE)

# import data from twitter
geo_trend <- trends_available() %>% 
  filter(country == "Japan")
head(geo_trend, n = Inf)

tweets_Japan_trend <- get_trends("Japan")
tweets_ww_trend <- get_trends("world")

tweets <- search_tweets("鬼滅", 
                        n = 1000, 
                        type = "recent", # "popular is insufficient in number of tweets. 
                        include_rts = FALSE,
                        lang = "ja"
                        )



# tidy data  --------------------------------------------------------------

tweets_text <- tweets$text

tweets_text_onlyJa <- str_replace_all(tweets_text, "\\p{ASCII}", "")
tweets_text_onlyJa_shiftJis = tweets_text_onlyJa %>% iconv(from = "UTF-8", to = "CP932") %>% na.omit()


tweets_text_all = ""
for (i in 1:length((tweets_text_onlyJa_shiftJis))) {
  tweets_text_all = paste(tweets_text_all, tweets_text_onlyJa_shiftJis[i], sep = "")
}

write.table(tweets_text_all, "tweets_text_all.txt")



# RMeCab ------------------------------------------------------------------


docDF_text = docDF("tweets_text_all.txt", type = 1, pos = c("名詞", "形容詞")) %>% 
  select(everything(), 
         word = TERM,
         freq = starts_with("tweets"))


docDF_text2 = docDF_text %>% 
  filter(!POS2 %in% c("非自立", "サ変接続")) 

# Histogram of Freq --------------------------------------------------------

ggplot(docDF_text2, aes(x = Freq)) +
   geom_boxplot(aes(alpha = 0.2)) +
   coord_cartesian(xlim = c(0, 100))

quantile(docDF_text2$freq)
nrow(docDF_text2)
docDF_text2[docDF_text2$Freq > 200, ]   

# wordcloud ---------------------------------------------------------------

wordcloud(docDF_text2$word, docDF_text2$freq, min.freq=20, max.words = Inf, 
          scale = c(3.5, 0.25), # c(size, width)
          ordered.colors = FALSE,
          random.order = FALSE, random.color = FALSE, rot.per = 3.5, 
          colors = brewer.pal(10, "Set3")) #brewer.pal(8, "Dark2")) 


# wordcloud2 --------------------------------------------------------------

df <- docDF_text2 %>% 
  arrange(desc(freq)) %>% 
  filter(freq < 1000) %>% 
  select(word, freq)

df
wordcloud2(data = df, size = 1, color = "random-light")
wordcloud2(data = df, size = 1, color = "random-dark", 
           shuffle = FALSE, shape = 'circle') 
wordcloud2(data = df, size = 1, 
           minRotation = -pi/6, maxRotation = -pi/6,
           color = ifelse(df[,2] > 100, "red", "skyblue"),
           shuffle = FALSE, shape = 'pentagon') 

