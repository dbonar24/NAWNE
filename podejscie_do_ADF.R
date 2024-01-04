#Biblioteki
library(readxl)
library(ggplot2)
library(tidyverse)

#Czyszcze pamiec i ustawiam obszar roboczy

rm(list=ls())
setwd("C:/Users/Dawid/Desktop/NAWNE/NAWNE")
getwd()

#wgrywam dane

file_ = "notowania_gield.xlsx"
SP500 = read_xlsx(path = file_, sheet = 1, range = "A1:C253") #Aby zbadać inny szereg należy zmienić sheet c(1,2,3)
SP500 = SP500 %>%
  arrange(Date, desc = TRUE)

#Na podstawie wykresu wybieramy hipoteze o istenieniu trendu i stałej (delta i phi 0)
#Przechodzimy do testu ADF

szereg = SP500[,2]

#Pętla ADF

for (l in 1:1) {
  
  #Wektor w - przyrosty y(t)

w = szereg - lag(szereg, l+1)
w = as.matrix(w[-(1:(l+1)),])  

    #Macierz z
const_ = as.matrix(rep(1, nrow(w)), nrow = nrow(w), ncol = 1)
t_ = as.matrix(seq(from = 1, to = nrow(w)), nrow(w), 1)
y_t_1 = szereg[-(1:l),]

z_prz = {}
    for (x in 1:l) {
      przy_yt_1 = lag(w, x)
      z_prz = cbind(z_prz, przy_yt_1)
  }
z_prz = as.matrix(z_prz[-(1:l),])

}

#Z przyr chcemy łaczyc z "z"
#w łaczymy z "z" pełnym i w ucinamy o "l" pierwszych wierszy
