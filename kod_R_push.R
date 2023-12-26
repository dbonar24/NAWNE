#Biblioteki
library(readxl)
library(ggplot2)
library(tidyverse)
#Czyszcze pamiec i ustawiam obszar roboczy
rm(list=ls())
setwd("C:/Users/Dawid/Desktop/NAWNE")
getwd()
#wgrywam dane
file_ = "notowania_gield.xlsx"
SP500 = read_xlsx(path = file_, sheet = 1, range = "A1:C253")
SP500 = SP500 %>%
  arrange(Date, desc = TRUE)
#Wykres
ggplot(data = SP500, mapping = aes(x = Date, y = Price)) + 
  geom_line()
#Na podstawie wykresu wybieramy hipoteze o istenieniu trendu i stałej (delta i phi 0)
#Przechodzimy do testu ADF
szereg = SP500[,2]
#Wektor w
w = as.matrix(seq(from = 1, to = nrow(szereg) - 1), nrow(szereg) - 1, 1)
for (i in 1:(nrow(szereg) - 1)) {
  w[i,] = as.numeric(szereg[i+1,] - szereg[i,])
  
}
#Kolumna z
const_ = as.matrix(rep(1, nrow(w)), nrow = nrow(w), ncol = 1)
t_ = as.matrix(seq(from = 1, to = nrow(w)), nrow(w), 1)
y_lag = as.matrix(lag(szereg, 1))
y_lag = as.matrix(y_lag[-1,])
z = cbind(const_, t_, y_lag)
#Macierz ADF
ADF_ = as.data.frame(cbind(w,z))
colnames(ADF_) = c("prz", "const", "t", "y_lag")
#Regresja ADF
reg_ADF = lm(prz ~ t + y_lag, data = ADF_)
reg_ADF
#Plik z macierzami
plik_out = "macierze.csv"
sink(file = plik_out, append = FALSE)
ADF_
sink()