# welch -------------------------------------------------------------------

# welch検定

# 等分散か不分散をF検定で確かめる　
# →等分散の場合は、対立仮説（P値<0.05）
# →不分散の場合は、帰無仮説（P値<0.05)

Legend <- filter(pokemon, Legendary == TRUE) 
Not_Legend <- filter(pokemon, Legendary == FALSE) 

# test for 'Attack'
var.test(Legend$Attack, Not_Legend$Attack)

# test for 'Defence'
var.test(Not_Legend$Attack, Not_Legend$Defense)



library(ggplot2)
library(readr)
library(tidyverse)
library(stringr)
url <- "https://gist.github.com/armgilles/194bcff35001e7eb53a2a8b441e8b2c6/raw/92200bc0a673d5ce2110aaad4544ed6c4010f687/pokemon.csv"
pokemon <- read_csv(url)


colnames(pokemon)[3] <- "Type_1"
colnames(pokemon)[4] <- "Type_2"
colnames(pokemon)[9] <- "Sp.Atk" 
colnames(pokemon)[10] <- "Sp.Def"
  
names(pokemon)

# 前提：両者は等分散で正規分布に近似すると考える。
# 伝説ポケモンと通常ポケモン（伝説ポケモンでない）の平均の差は存在するのか。
## 帰無仮説：平均の差がない
## 対立仮説：平均の差がある

# 可視化する
ggplot(pokemon, aes(Attack, Defense, colour = Legendary)) +
  geom_point(alpha = .5) +
  gghighlight::gghighlight() +
  facet_wrap(vars(Legendary)) +
  labs(title = "伝説ポケモンと通常ポケモンの攻撃力と守備力の分布",
       subtitle = "伝説ポケモンの方が攻守ともに能力が高く、分散が大きいようにみえる",
       caption = paste("データソース:", url)) 

sd(pokemon[, "Legendary" == TRUE], na.rm = TRUE)

pokemon %>% 
  filter(Legendary == TRUE) %>% 
  summarise(
    Attack = sd(Attack),
    Defence = sd(Defense))

pokemon %>% 
  filter(Legendary == FALSE) %>% 
  summarise(
    Attack = sd(Attack),
    Defence = sd(Defense))

ggplot(pokemon, aes(Atk, Def, colour = Type_1)) +
  geom_point(alpha = .5) +
  gghighlight::gghighlight() +
  facet_wrap(vars(Type_1))


ggplot(pokemon, aes(Attack, Defense, colour = Type_2)) +
  geom_point() +
  gghighlight::gghighlight() +
  facet_wrap(vars(Type_2))

# Type_2 == NAを調べる
## 386 / 800 = 0.4825のポケモンがType_2を持っていない

pokemon %>% 
  filter(is.na(Type_2)) %>% 
  head(n = Inf)



# geom_text
pokemon %>% 
  filter(Legendary == TRUE) %>% 
  ggplot(aes(Attack, Defense)) +
  geom_text(aes(label = Name), check_overlap = FALSE) 

str_subset(pokemon$Name, pattern = "Deoxys")

# generationごとにポケモンの能力は飛躍しているのか？


ggplot(pokemon, aes(Attack, Defense, group = Generation, colour = Generation)) +
  geom_point() +
  gghighlight::gghighlight() +
  facet_wrap(vars(Generation)) 

ggplot(pokemon, aes(Sp.Atk, Sp.Def, group = Generation, colour = Generation)) +
  geom_point() +
  gghighlight::gghighlight() +
  facet_wrap(vars(Generation)) 

ggplot(pokemon, aes(Attack, group = Generation, colour = Generation, fill = Generation)) +
  geom_density(alpha = 0.1)


 # 没集
ggplot(pokemon, aes(Attack, Defense)) +
  geom_bin2d(binwidth = 7) 

ggplot(pokemon, aes(Attack, Defense, colour = Type_1)) +
  geom_point(show.legend = FALSE) +
  directlabels::geom_dl(aes(label = Type_1), method = "smart.grid")

ggplot(pokemon, aes(Attack, Defense)) +
  geom_point(show.legend = TRUE) +
  ggforce::geom_mark_ellipse(aes(label = Type_1, group = Type_1))

