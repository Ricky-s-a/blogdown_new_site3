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

pokemon$Type_1 <- pokemon$`Type 1`
pokemon$Type_2 <- pokemon$`Type 2`

names(pokemon)

# 前提：両者は等分散で正規分布に近似すると考える。
# 伝説ポケモンと通常ポケモン（伝説ポケモンでない）の平均の差は存在するのか。
## 帰無仮説：平均の差がない
## 対立仮説：平均の差がある

# 可視化する
ggplot(pokemon, aes(Attack, Defense, colour = Legendary)) +
  geom_point() +
  gghighlight::gghighlight() +
  facet_wrap(vars(Legendary))

Plot <- function(x) {
ggplot(pokemon, aes(Attack, Defense, colour = x)) +
  geom_point() +
  gghighlight::gghighlight() +
  facet_wrap(vars(x))
}

ggplot(pokemon, aes(Attack, Defense, colour = Type_1)) +
  geom_point() +
  gghighlight::gghighlight() +
  facet_wrap(vars(Type_1))


ggplot(pokemon, aes(Attack, Defense, colour = Type_2)) +
  geom_point() +
  gghighlight::gghighlight() +
  facet_wrap(vars(Type_2))

# geom_text
pokemon %>% 
  filter(Legendary == TRUE) %>% 
  ggplot(aes(Attack, Defense)) +
  geom_text(aes(label = Name), check_overlap = FALSE) 

str_subset(pokemon$Name, pattern = "Deoxys")


# 没集
ggplot(pokemon, aes(Attack, Defense)) +
  geom_bin2d(binwidth = 7) 

ggplot(pokemon, aes(Attack, Defense, colour = Type_1)) +
  geom_point(show.legend = FALSE) +
  directlabels::geom_dl(aes(label = Type_1), method = "smart.grid")

ggplot(pokemon, aes(Attack, Defense)) +
  geom_point(show.legend = TRUE) +
  ggforce::geom_mark_ellipse(aes(label = Type_1, group = Type_1))

