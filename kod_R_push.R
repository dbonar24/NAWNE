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


#Test DW

theta_hat = as.vector(reg_ADF$coefficients)
theta_hat
w_hat = z %*% theta_hat
eps_hat = w - w_hat

#Przyrosty eps

e = as.matrix(seq(from = 1, to = nrow(eps_hat) - 1), nrow = nrow(eps_hat) - 1, ncol = 1)
for (x in 1:(nrow(eps_hat) - 1)) {
  e[x,] = as.numeric(eps_hat[x+1,] - eps_hat[x,])
}
e_prz_cube = e^2
eps_hat_cube = eps_hat^2

DW = sum(e_prz_cube)/sum(eps_hat_cube)
DW

#Dla wszystkich szeregów statystyka DW nalezy do przedziału (1,7;2,3) możemy przechodzić do ADF
#Statystyka DF

mac_kow = vcov(reg_ADF)
blad_theta = as.matrix(diag(mac_kow)^(0.5))
blad_theta

delta_hat = theta_hat[3]
blad_delta_hat = as.vector(blad_theta[3])

DF_emp = delta_hat/blad_delta_hat
DF_emp
