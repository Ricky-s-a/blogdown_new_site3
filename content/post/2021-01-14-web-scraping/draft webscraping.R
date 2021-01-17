
# setup packages ----------------------------------------------------------
install.packages("RMeCab", repos = "http://rmecab.jp/R")

guess_encoding()

Packages <- c("rvest", "tidyverse","RMeCab", "DT", "wordcloud2", "ggplot2")
lapply(Packages, library, character.only = TRUE)

# set url info ------------------------------------------------------------

url <- "https://www.amazon.co.jp/product-reviews/B01MA1Y0OG/ref=acr_dpx_see_all?ie=UTF8&showViewpoints=1"


# extract text data -------------------------------------------------------

data <- read_html(url)
media <- html_nodes(data, ".review-text") %>% 
  html_text() %>% 
  iconv(from = "UTF-8", to = "Shift_JIS")

text_all = ""
for (i in 1:length(media)) {
  text_all = paste(text_all, media[i], sep = "")
}

write.table(text_all, "text_all.txt")



# RMeCab ------------------------------------------------------------------

docDF_text <- docDF("text_all.txt", type = 1, pos = c("名詞", "形容詞")) %>% 
  select(everything(), 
         word = TERM,
         freq = starts_with("text"))

docDF_text2 <- docDF_text %>% 
  filter(!POS2 %in% c("非自立", "サ変接続")) %>% 
  arrange(desc(freq))

datatable(docDF_text2, rownames = FALSE)


# wordcloud2 --------------------------------------------------------------

df <- docDF_text2 %>% 
  arrange(desc(freq)) %>% 
  select(word, freq)


wordcloud2(data = df, size = 1, 
           minRotation = -pi/6, maxRotation = -pi/6,
           color = "random-dark",
           shuffle = FALSE, shape = 'circle') 

## https://qiita.com/katsuki104/items/a0a663dd342fb5b7caf2

# 自分でやってみる。 ---------------------------------------------------------------

# set url info ------------------------------------------------------------


url2 <- "https://www.amazon.co.jp/Huazon-%E3%82%AF%E3%83%AC%E3%82%A4%E3%82%B8%E3%83%BC%E3%82%A2%E3%82%A4%E3%82%BA-%E9%9D%A2%E7%99%BD%E3%81%84%E3%83%A1%E3%82%AC%E3%83%8D-%E3%81%8A%E3%81%8B%E3%81%97%E3%81%84%E3%83%91%E3%83%BC%E3%83%86%E3%82%A3%E3%83%BC%E3%83%A1%E3%82%AC%E3%83%8D-%E3%81%8A%E3%82%82%E3%81%97%E3%82%8D%E3%82%B0%E3%83%83%E3%82%BA%EF%BC%88%E7%AE%B1%E5%85%A5%E3%82%8A%EF%BC%89/dp/B082S3HJ4X/ref=zg_bs_toys_home_1?_encoding=UTF8&psc=1&refRID=GMRST40WMR47WPM5K35Z"

# extract text data -------------------------------------------------------

data <- read_html(url2)
media <- html_nodes(data, css = ".cr-original-review-content") %>% 
  html_text() %>% 
  iconv(from = "UTF-8", to = "Shift_JIS")

text_all = ""
for (i in 1:length(media)) {
  text_all = paste(text_all, media[i], sep = "")
}

write.table(text_all, "text_all.txt")



# RMeCab ------------------------------------------------------------------

docDF_text <- docDF("text_all.txt", type = 1, pos = c("名詞", "形容詞")) %>% 
  select(everything(), 
         word = TERM,
         freq = starts_with("text"))

docDF_text2 <- docDF_text %>% 
  filter(!POS2 %in% c("非自立", "サ変接続")) %>% 
  arrange(desc(freq))

datatable(docDF_text2, rownames = FALSE)


# wordcloud2 --------------------------------------------------------------

df <- docDF_text2 %>% 
  arrange(desc(freq)) %>% 
  select(word, freq)


wordcloud2(data = df, size = 1, 
           minRotation = -pi/6, maxRotation = -pi/6,
           color = "random-dark",
           shuffle = FALSE, shape = 'circle') 



# 沖縄県那覇市のガソリンスタンド ---------------------------------------------------------

#https://qiita.com/Tom-tom-tom/items/998e8282d013fb218490

url3 <- "https://gogo.gs/search/?kw=&ac=47201"

data <- read_html(url3)
gas_name_1 <- html_nodes(data, xpath = "//h5[@class = 'shop-name font-weight-bold']") %>% 
  html_text()
gas_address_1 <- html_nodes(data, xpath = "//p[@class = 'shop-address']") %>% 
  html_text()

name_address <- data.frame(store_name = gas_name_1, 
                       address = gas_address_1)

name_address_all <- data.frame()
for (i in 1:5) {
  URL <- paste("https://gogo.gs/search/?kw=&ac=47201&page=", i, sep = "")
  data <- read_html(URL)
  gas_name <- html_nodes(data, xpath = "//h5[@class = 'shop-name font-weight-bold']") %>% 
    html_text()
  gas_address <- html_nodes(data, xpath = "//p[@class = 'shop-address']") %>% 
    html_text()
  name_address <- data.frame(
    name = gas_name,
    address = gas_address
  )
  name_address_all <- rbind(name_address_all, name_address)
}


# テレワークデイズ ----------------------------------------------------------------

url <- "https://teleworkdays.go.jp/search_2020/practice/"
data <- read_html(url) 
media <- html_nodes(data, xpath = '/descendant-or-self::DT[contains(@class,"o-orgItem__ttl")]/A[1]') %>% 
  html_text() %>% 
  iconv(from = "UTF-8", to = "Shift_JIS")


# webscraping2 ------------------------------------------------------------

url <- "http://carinf.mlit.go.jp/jidosha/carinf/ris/search.html?selCarTp=1&lstCarNo=1060&txtMdlNm=&txtFrDat=2000/01/01&txtToDat=2017/12/31&page=1"
data <- read_html(url, encoding = "UTF-8")
media <- html_nodes(data, xpath = "//table") %>% 
  html_text()
  
src_url  <- "http://carinf.mlit.go.jp/jidosha/carinf/ris/search.html?selCarTp=1&lstCarNo=1060&txtMdlNm=&txtFrDat=2017/01/01&txtToDat=2017/12/31&page="
link_url <- "http://carinf.mlit.go.jp/jidosha/carinf/ris/"

target_column <- c("車名/メーカー名", "不具合装置", "状　況", "リコール開始日", "対象台数")
html_tbl_all  <- data.frame()

for ( i in 1:5) {
  
  target_page <- paste0(src_url, i)
  recall_html <- read_html(target_page, encoding = "UTF-8")
  
  target_url_list <- 
    recall_html %>% 
    html_nodes(xpath = "//a") %>% 
    hml_attr("herf") %>% 
    as_data_frame() %>% 
    filter(grep("detail", .$value))
  
  1 <- nrow(target_url_list)
  

# webscrapingをやるにあたって、xpathの理解と、html、cssの簡単な理解が必要。
# だから、引き続き勉強するときは、まずxpath、html、cssを理解できるようになってからにしよう。  

