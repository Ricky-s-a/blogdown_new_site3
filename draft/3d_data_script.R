# setup library()

library(tidyverse)
library(rgl)
library(threejs)


# plot 3d graph with `threejs`

z <- seq(-10, 10, by = 0.1)
x <- cos(z)
y <- sin(z)
scatterplot3js(x, y, z, 
               color = rainbow(length(z)),
               num.tricks = c(6, 6, 6),
               x.tricklabs = NULL,
               y.tricklabs = NULL,
               z.tricklabs = NULL,
               size = 1,
               )

# animation with rgl & htmltools

# setup library()

library(rgl)
library(htmltools)

# theta を定義
theta <- seq(0, 6*pi, len = 100) 

# xyz軸を定義
xyz <- cbind(sin(theta), cos(theta), theta)

# plot3dで図を表示させる
lineid <- plot3d(xyz, type = "l", alpha = 1:0,
                 lwd = 5, col = "blue")["data"]
lineid

# 図を動かす
browsable(tagList( # tagを作成（たぶん）
  rglwidget(elementId = "example", # ここでrglwidgetの名称をつける 
            width = 500, height = 400, # 表示するピクセル数
            controllers = "player"), # コントローラーの名称をつける
  playwidget("example",
             ageControl(births = theta, 
                        ages = c(0, 0, 10), # xyzの順
                        objids = lineid, 
                        alpha = c(0, 1, 0)),
                        start = 1, stop = 6*pi, # スタートする位置の値 
                        step = 0.1, # 0.1ずつ動く
                        rate = 6, # 単位を実世界の毎秒あたりいくつ動かすか 
                        elementId = "player") # ??
))

