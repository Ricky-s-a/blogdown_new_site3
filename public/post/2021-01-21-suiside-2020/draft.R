# 自殺者データを入手
Packages <- c("tidyverse", "openxlsx", "lubridate", "scales", "jpndistrict", "maps", "stringr", "sf")
lapply(Packages, library, character.only = TRUE)

url <- "https://www.npa.go.jp/safetylife/seianki/jisatsu/R02/zantei0211.xlsx"

suiside_df <- read.xlsx(xlsxFile = url)
suiside_df_sex <- suiside_df[3:5, 4:15] %>% 
  t() %>% 
  as.data.frame() 


colnames(suiside_df_sex) <- c("total", "male", "female")
rownames(suiside_df_sex) <- c(1:12)

suiside_df_sex$month <- month(1:12)

for(i in 1:3) {
  suiside_df_sex[,i] <- as.numeric(suiside_df_sex[,i])
}


# １２月分のデータを入力

for(i in 1:3) {
  suiside_df_sex[12, i] <- mean(suiside_df_sex[ ,i], na.rm = TRUE)
}


# データフレームの方変更
suiside_df_sex <- suiside_df_sex %>% 
  pivot_longer(cols = -month, names_to = "種別", values_to = "amount")


# グラフ化

ggplot(suiside_df_sex, aes(x = month, y = amount, color = 種別)) +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 12, by = 1)) +
  scale_y_continuous(labels = comma) +
  ylab("自殺者数") +
  xlab("月")


# 県ごと
suiside_df_prefs <- suiside_df[8:53, c(1, 3)] %>% 
  as.data.frame() 

# col & rowデータを整える
colnames(suiside_df_prefs) <- c("prefecture", "suiside_amount")
rownames(suiside_df_prefs) <- c(1:46)

# prefectureを整える
suiside_df_prefs$prefecture <- str_replace(suiside_df_prefs$prefecture, pattern = "　", replacement = "")




# 地図データをダウンロード

head(jpnprefs)
data("jpnprefs")
jpnprefs$prefecture <- str_replace(jpnprefs$prefecture, pattern = "県", replacement = "")
jpnprefs$prefecture <- str_replace(jpnprefs$prefecture, pattern = "府", replacement = "")
jpnprefs[13, 2] <- "東京"


suiside_df_prefs <- left_join(jpnprefs, suiside_df_prefs, by = "prefecture")
suiside_df_prefs <- suiside_df_prefs[1:46,]

# 地図を表示させる

ggplot() +
  geom_point(data = suiside_df_prefs, aes(capital_longitude, capital_latitude))

Japan <- world %>% 
  filter(region == "Japan") %>% 
  ggplot(aes(x = long, lat, group = group)) +
  geom_polygon(fill = "lightgray", colour = "black", size = 0.1) +
  coord_sf(crs = st_crs(3112)) 

Japan +
  ggplot() +
  geom_point(data = suiside_df_prefs, aes(capital_longitude, capital_latitude))

