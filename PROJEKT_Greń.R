#PROJEKT
#Statystyczna Analiza Danych
#Piotr Greń - 169783

#Analiza danych państw - średni wzrost mężczyzn w krajach, średni wzrost kobiet, wielkość populacji

setwd("C:/Users/Piotrek/Desktop/Uczelnia/II rok/Semestr II/Statystyczna Analiza Danych/Projekt")

#Pobranie Danych
Data <- read.csv("countries_data.csv")
df <- data.frame(cbind(Data$Country, Data$Average.Height.Male..cm., Data$Average.Height.Female..cm.))
colnames(df) <- c("Country", "AVG M Height", "AVG F Height")
male_height <- Data$Average.Height.Male..cm.
female_height <- Data$Average.Height.Female..cm.

#### Obliczanie podstawowych parametrów opisowych
#Średnia
m_mean <- mean(male_height)
f_mean <- mean(female_height)

#Odchylenie Standardowe
m_sd <- sd(male_height)
f_sd <- sd(female_height)

#Współczynnik zmienności
m_cv <- m_sd/m_mean
f_cv <- f_sd/f_mean

#Mediana
m_median <- median(male_height)
f_median <- median(female_height)

#Kwartyle
m_quant <- quantile(male_height, probs = c(0.25, 0.75))
m_Q1 <- m_quant[1]
m_Q3 <- m_quant[2]
f_quant <- quantile(female_height, probs = c(0.25, 0.75))
f_Q1 <- f_quant[1]
f_Q3 <- f_quant[2]

#Minimalna wartość
m_min <- min(male_height)
f_min <- min(female_height)

#Maksymalna wartość
m_max <- max(male_height)
f_max <- max(female_height)

#Dominanta
m_mcv <- names(which.max(table(male_height)))
m_mcv <- as.numeric(m_mcv)
f_mcv <- names(which.max(table(female_height)))
f_mcv <- as.numeric(f_mcv)

#Wariancja
m_var <- var(male_height)
f_var <- var(female_height)

#Rozstęp danych (range)
m_range <- m_max - m_min
f_range <- f_max - f_min

#Skośność danych
install.packages("moments")
library("moments")
m_skew <- skewness(male_height)
f_skew <- skewness(female_height)


#Podsumowanie wyników
options(scipen = 999)
results <- data.frame(
  Parametr = c("Średnia", "Odchylenie standardowe", "Współczynnik zmienności", "Mediana", "Kwartyl 25%", "Kwartyl 75%", "Min", "Max", "Dominanta", "Wariancja", "Rozstęp Danych", "Skośność Danych"),
  AVG_Male_Height = round(c(m_mean, m_sd, m_cv, m_median, m_Q1, m_Q3, m_min, m_max, m_mcv, m_var, m_range, m_skew), 3),
  AVG_Female_Height = round(c(f_mean, f_sd, f_cv, f_median, f_Q1, f_Q3, f_min, f_max, f_mcv, f_var, f_range, f_skew), 3)
)
results <- data.frame(results)
results




####Graficzna prezentacja danych
#Wykres pudełkowy
combined <- c(male_height, female_height)
gender <- c(rep("Male", length(male_height)), rep("Female", length(female_height)))
boxplot(combined ~ gender, col = c("mediumpurple1", "firebrick4"), main = "Rozkład średniego wzrostu mężczyzn i kobiet w państwach świata", xlab = "Płeć", ylab = "Wzrost [cm]")


#Wykres skrzypcowy
install.packages("vioplot")
library("vioplot")
vioplot(combined ~ gender, col = c("mediumpurple1", "firebrick4"), main = "Wykres skrzypcowy średniej wartości wzrostu", xlab = "Płeć", ylab = "Wzrost [cm]")

#Wykres Estymatora jądrowego gęstości
d1 <- density(male_height)
plot(d1, main = "Estymator jądrowy gęstości średniego wzrostu mężczyzn", xlab = "Wzrost [cm]", ylab = "Gęstość")
polygon(d1, col = "tan3", border = "black")

d2 <- density(female_height)
plot(d2, main = "Estymator jądrowy gęstości średniego wzrostu kobiet", xlab = "Wzrost [cm]", ylab = "Gęstość")
polygon(d2, col = "darkseagreen3", border = "black")

#Histogram
library("ks")
h <- hist(male_height, breaks = seq(min(male_height), max(male_height), length.out = 20), main = "Rozkład średniego wzrostu mężczyzn", xlab = "Wzrost [cm]", ylab = "Częstość", col = "firebrick4", border = "white")
xfit <- seq(min(male_height), max(male_height), length.out = 20)
yfit <- dnorm(xfit,mean = m_mean, sd = m_sd)
yfit <- yfit*diff(h$mids[1:2])*length(male_height)
lines(xfit, yfit, col = "darkgoldenrod2", lwd = 2)

h1 <- hist(female_height, breaks = seq(f_min, f_max, length.out = 20), main = "Rozkład średniego wzorstu kobiet", xlab = "Wzrost [cm]", ylab = "Częstość", col = "mediumpurple1", border = "white")
xfitf <- seq(f_min, f_max, length.out = 20)
yfitf <- dnorm(xfitf, mean = f_mean, sd = f_sd)
yfitf <- yfitf*diff(h1$mids[1:2])*length(female_height)
lines(xfitf, yfitf, col = "dodgerblue4", lwd = 2)

#Wykres dystrybuanty empirycznej
emp_distm <- ecdf(male_height)
emp_distf <- ecdf(female_height)
plot(emp_distm, main = "Wykrest dystrybuanty empirycznej dla średniego wzrostu mężczyzn",
     xlab = 'Wartości', ylab = "Wartośc Dystrybuanty",
     pch = 19, cex = 0.5, col = "firebrick4")
plot(emp_distf, main = "Wykres dystrybuanty empirycznej dla średniego wzrostu kobiet",
     xlab = "Wartości", ylab = "Wartość Dystrybuanty",
     pch = 19, cex = 0.5, col = "dodgerblue4")

#Korelacja pomiędzy wzorstem, a wskaźnikiem rozwoju kraju
wskaznik <- Data$GDP.per.capita..current.US..
model <- lm(male_height ~ poly(wskaznik, 3, raw = TRUE))
cf <- coef(model)
poly_func <- function(x) {
  y <- cf[1] + cf[2] * x + cf[3] * x^2 + cf[4] * x^3
  return(y)
}
x_pred <- seq(min(wskaznik), max(wskaznik), length.out = 1000)
y_pred <- poly_func(x_pred)
plot(wskaznik, male_height, col = "darkgreen", xlab = "Współczynnik rozwoju GDP", ylab = "Średni wzrost mężczyzn w kraju [cm]", main = "Zależność wzrostu od współczynnik razwoju", pch = 20)
lines(x_pred, y_pred, col = "firebrick3", lwd = 2)
lines(seq(min(wskaznik), max(wskaznik), length.out=100), rep(m_mean, 100), col = "black")



#Testowanie hipotez statystycznych
#Hipoteza 1:
#https://rpubs.com/amachno/R_HT

#H0: Średni wzrost na świecie wynosi 172.5 cm   |   Mi = Mi0
#H1: Średni wzrost na świecie jest większy od 172.5 cm   |   Mi > Mi0

#Testujemy hipotezę na poziomie istotności α = 0.05
#Pobieramy próbę o wielkości 70 z naszej populacji danych nt. średniego wzrostu mężczyzn
set.seed(42)
proba <- sample(male_height, 70, replace=FALSE)

#Nie znamy odchylenie standardowego dlatego korzystamy ze statystyki T o rozkładzie T-Studenta
p_mean <- mean(proba)
p_sd <- sd(proba)
n <- length(proba)
mi <- 172.5

alfa <- 0.05
p_kwantyl <- qt(1 - alfa, n - 1)


# Funkcja gęstości rozkładu T-studenta
funkcja_gestosci <- function(x) dt(x, n - 1)

curve(funkcja_gestosci, from = -5, to = 5, ylab = "Gęstość prawdopodobieństwa",
      main = "Rozkład T-studenta df = 69", xlab = "Wartość statystyki T")
abline(h = 0, col = "gray")  # Linia bazowa

x_values <- seq(-5, 5, length.out = 1000)
y_values <- funkcja_gestosci(x_values)

#obszar_krytyczny_lewy <- x_values[x_values <= l_kwantyl]
obszar_krytyczny_prawy <- x_values[x_values >= p_kwantyl]
#obszar_y_lewy <- y_values[x_values <= l_kwantyl]
obszar_y_prawy <- y_values[x_values >= p_kwantyl]

polygon(c(obszar_krytyczny_prawy, p_kwantyl), c(obszar_y_prawy, 0), col = "firebrick4", border = NA)
legend(x = c(1.9, 5), y = c(0.4, 0.32), legend = c("Funkcja gęstości", "Obszar krytyczny"),
       col = c("black", "firebrick4"),
       pch = 15, cex = 0.9, border = NA, y.intersp = 0.8)

#Obliczamy wartość statystyki:
T1 <- ((p_mean - mi)/p_sd)*sqrt(n - 1)
if (T1 < p_kwantyl){
  print(paste("Wartość statystyki - ", T1, " < ", p_kwantyl, "- Granica obszaru krytycznego"))
  print("Nie ma podstaw do odrzucenia hipotezy zerowej!")
  print("Wartość statystyki nie należy do obszaru krytycznego!")
}else if (T1 > p_kwantyl){
  print(paste("Wartość statystyki - ", T1, " > ", p_kwantyl, "- Granica obszaru krytycznego"))
  print("Wartość statystyki należy do obszaru krytycznego!")
  print("Hipoteze zerową odrzucamy, na korzyść hipotezy alternatywnej")
}else{
  print(paste("Wartość statystyki - ", T1, " = ", p_kwantyl, "- Granica obszaru krytycznego"))
  print("Wartość statystyki należy do obszaru krytycznego!")
  print("Hipoteze zerową odrzucamy, na korzyść hipotezy alternatywnej")
}

x_value <- T1
y_value <- funkcja_gestosci(T1)

# Dodanie pionowej kreski na wykresie
segments(x0 = x_value, y0 = 0, x1 = x_value, y1 = y_value, col = "darkgoldenrod2", lwd = 2)
legend(x = c(1.9, 5), y = c(0.4, 0.25), legend = c("Funkcja gęstości", "Obszar krytyczny", "Wartość statystyki"),
       col = c("black", "firebrick4", "darkgoldenrod2"),
       pch = 15, cex = 0.75)



#Hipoteza 2:
#H0: Odchylenie standardowe średniego wzrostu kobiet na świecie wynosi 4  |   σ = σ0
#H1: Odchylenie standardowe średniego wzrostu kobiet na świecie rózni się od 4   |   σ != σ0
#przyjęty poziom istotności: α = 0.06

set.seed(42)
proba_f <- sample(female_height, 100, replace = FALSE)

sigma <- 4
alfa <- 0.06
pf_sd <- sd(proba_f)
n <- length(proba_f)

#Przedziały krytyczne
l_kwantyl_chi <- qchisq(alfa/2, n - 1)
p_kwantyl_chi <- qchisq(1 - alfa/2, n - 1)

#Wykres
funkcja_chi <- function(x) dchisq(x, n - 1)
curve(funkcja_chi, from = 0, to = 200, xlab = "Wartość satystyki V", ylab = "Gęstość prawdopodobieństwa", main = "Rozkład Chi-kwadrat df = 99")
abline(h = 0, col = "grey")

x_values <- seq(0, 200, length.out = 1000)
y_values <- funkcja_chi(x_values)

obszar_krytyczny_lewy <- x_values[x_values <= l_kwantyl_chi]
obszar_krytyczny_prawy <- x_values[x_values >= p_kwantyl_chi]
obszar_y_lewy <- y_values[x_values <= l_kwantyl_chi]
obszar_y_prawy <- y_values[x_values >= p_kwantyl_chi]

polygon(c(obszar_krytyczny_prawy, p_kwantyl_chi), c(obszar_y_prawy, 0), col = "firebrick4", border = NA)
polygon(c(obszar_krytyczny_lewy, l_kwantyl_chi), c(obszar_y_lewy, 0), col = "firebrick4", border = NA)
legend(x = c(125, 200), y = c(0.032, 0.028), legend = c("Funkcja gęstości", "Obszar krytyczny"),
       col = c("black", "firebrick4"),
       pch = 15, cex = 0.8, y.intersp = 0.8)


V1 <- (n*(pf_sd)^2)/(sigma)^2
if (V1 < p_kwantyl_chi & V1 > l_kwantyl_chi){
  print(paste("Wartość statystyki: ", V1))
  print(paste("Granica dolnego obszaru krytycznego: ", l_kwantyl_chi))
  print(paste("Granica górnego obszaru krytycznego: ", p_kwantyl_chi))
  print("Nie ma podstaw do odrzucenia hipotezy zerowej!")
  print("Wartość statystyki nie należy do obszaru krytycznego!")
}else if (V1 >= p_kwantyl_chi){
  print(paste("Wartość statystyki: ", V1))
  print(paste("Granica górnego obszaru krytycznego: ", p_kwantyl_chi))
  print("Wartość statystyki należy do górnego obszaru krytycznego!")
  print("Hipoteze zerową odrzucamy, na korzyść hipotezy alternatywnej")
}else{
  print(paste("Wartość statystyki: ", V1))
  print(paste("Granica dolnego obszaru krytycznego: ", l_kwantyl_chi))
  print("Wartość statystyki należy do dolnego obszaru krytycznego!")
  print("Hipoteze zerową odrzucamy, na korzyść hipotezy alternatywnej")
}

x_value <- V1
y_value <- funkcja_chi(V1)

# Dodanie pionowej kreski na wykresie
segments(x0 = x_value, y0 = 0, x1 = x_value, y1 = y_value, col = "darkseagreen3", lwd = 2)
legend(x = c(225, 300), y = c(0.022, 0.016), legend = c("Funkcja gęstości", "Obszar krytyczny", "Wartość statystyki"),
       col = c("black", "firebrick4", "darkseagreen3"),
       pch = 15, cex = 0.8, y.intersp = 0.8)






