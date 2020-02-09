library(data.table)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(mgcv)
library(plotly)

# Load data:
title = read.delim2("title.basics.tsv")
ratings = read.delim("title.ratings.tsv")

title = subset(title, titleType == "movie" )
movie = merge(title, ratings, by = "tconst", all.x = TRUE)
movie = na.omit(movie)

movie$runtimeMinutes = as.numeric(as.character(movie$runtimeMinutes))
movie$startYear = as.numeric(as.character(movie$startYear))

movie = movie[which(movie$runtimeMinutes<=200),]
movie = na.omit(movie)

movie.gam = gam(averageRating ~ s(runtimeMinutes, startYear), data = movie)

movie.grid = expand.grid(startYear = 1900:2010, runtimeMinutes = 0:200)
movie.gam.pred = as.vector(predict(movie.gam, newdata = movie.grid))
movie.gam.pred.df = data.frame(movie.grid, averageRating = movie.gam.pred)

ggplot(movie.gam.pred.df, aes(x = startYear, y = averageRating)) + 
  geom_point() + 
  scale_size_area() + 
  geom_smooth(method.args = list(degree = 1), color = 'orange', se = FALSE) + 
  facet_wrap(~cut_number(runtimeMinutes, n = 10),) + 
  xlab("Year of Movie was Released") + 
  ylab("Average Movie Ratings") + 
  ggtitle("Movie Ratings") + 
  labs(subtitle="Based Year Released Grouped by Length of Movie(minutes)")

ggplot(movie.gam.pred.df, aes(x = runtimeMinutes, y = startYear, fill = averageRating)) + 
  geom_raster() + 
  scale_fill_distiller(palette = "RdYlBu") + 
  coord_fixed() + 
  geom_contour(aes(z = averageRating))

plot_ly(movie.gam.pred.df, x = ~runtimeMinutes, y = ~startYear, z = ~averageRating, type = "scatter3d", marker = list(size = .5))