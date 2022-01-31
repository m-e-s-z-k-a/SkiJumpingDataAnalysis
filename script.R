library(tidyr)
library(ggplot2)
library(moments)
library(dplyr)
library(nortest)
library(ggcorrplot)

fpath<-"C:\\Users\\dorot\\Documents\\STUDIA_materialy\\SEMESTR3\\RPiS\\all_results.csv"
results<-read.csv(fpath,header=TRUE,stringsAsFactors = FALSE)
competitions<-read.csv("C:\\Users\\dorot\\Documents\\STUDIA_materialy\\SEMESTR3\\RPiS\\all_comps.csv", header=TRUE,stringsAsFactors = FALSE)


#sprawdzenie podstawowych informacji na temat danych
dim(results)
colnames(results)
str(results)
head(results)
summary(results)
dim(competitions)
colnames(competitions)
#ile wartoœci nieznanych w ka¿dej z kolumn
colSums(is.na(results))

#data cleaning - usuniêcie kolumny Unnamed..0 oraz usuniêcie wierszy, w których jest co najmniej 1 wartoœæ NA
results = subset(results, select =-c(Unnamed..0))
row.has.na <- apply(results, 1, function(x){any(is.na(x))})
results <- results[!row.has.na,]
competitions<-competitions[!(competitions$gender=="Women" | competitions$hill_size_y<120 | competitions$hill_size_y>150 ),]
dim(competitions)
results <- results[ results$id %in% competitions$id, ]
results <- results[!(results$speed == 0),]
dim(results)

#sprawdzenie informacji nt danych po data cleaning
summary(results)
colSums(is.na(results))
dim(results)

#rysowanie wykresów
#³¹czenie wartoœci note_1..5 w jeden zestaw
notes_data <- data.frame(results$note_1, results$note_2, results$note_3, 
                         results$note_4, results$note_5)
s_notes_data <- stack(notes_data)

#wykresy - boxplot
boxplot <- ggplot(results, aes(y = speed)) + 
  geom_boxplot(color="black", fill="mediumspringgreen", alpha=0.5, outlier.size=5) + 
  labs(title="speed boxplot")
boxplot
boxplot <- ggplot(results, aes(y = points)) + 
  geom_boxplot(color="black", fill="lightslateblue", alpha=0.5, outlier.size=5) + 
  labs(title="points boxplot")
boxplot
boxplot <- ggplot(results, aes(y = dist)) + 
  geom_boxplot(color="black", fill="turquoise1", alpha=0.5, outlier.size=5) + 
  labs(title="distance boxplot")
boxplot
boxplot <- ggplot(results, aes(y = dist_points)) + 
  geom_boxplot(color="black", fill="lightskyblue", alpha=0.5, outlier.size=5) + 
  labs(title="distance points boxplot")
boxplot
boxplot <- ggplot(s_notes_data, aes(y = values)) + 
  geom_boxplot(color="black", fill="pink1", alpha=0.5, outlier.size=5) + 
  labs(title="singular style value boxplot")
boxplot
boxplot <- ggplot(results, aes(y = note_points)) + 
  geom_boxplot(color="black", fill="plum1", alpha=0.5, outlier.size=5) + 
  labs(title="sum style value boxplot")
boxplot
boxplot <- ggplot(results, aes(y = gate)) + 
  geom_boxplot(color="black", fill="indianred1", alpha=0.5, outlier.size=5) + 
  labs(title="starting gate boxplot")
boxplot
boxplot <- ggplot(results, aes(y = wind)) + 
  geom_boxplot(color="black", fill="yellow1", alpha=0.5, outlier.size=5) + 
  labs(title="wind strength boxplot")
boxplot
boxplot <- ggplot(results, aes(y = wind_comp)) + 
  geom_boxplot(color="black", fill="goldenrod1", alpha=0.5, outlier.size=5) + 
  labs(title="wind compensation points boxplot")
boxplot
boxplot <- ggplot(results, aes(y = gate_points)) + 
  geom_boxplot(color="black", fill="darkorchid1", alpha=0.5, outlier.size=5) + 
  labs(title="gate points boxplot")
boxplot


#wykresy - histogramy
histogram <- ggplot(results, aes(x = speed)) + 
  geom_histogram(binwidth=0.2, color="black", fill="mediumspringgreen") + 
  labs(title="speed histogram")
histogram
histogram <- ggplot(results, aes(x = points)) + 
  geom_histogram(binwidth=0.5, color="black", fill="lightslateblue") + 
  labs(title="points histogram")
histogram
histogram <- ggplot(results, aes(x = dist)) + 
  geom_histogram(binwidth=0.5, color="black", fill="turquoise1") + 
  labs(title="distance histogram")
histogram
histogram <- ggplot(results, aes(x = dist_points)) + 
  geom_histogram(binwidth=0.9, color="black", fill="lightskyblue") + 
  labs(title="distance points histogram")
histogram
histogram <- ggplot(s_notes_data, aes(x = values)) + 
  geom_histogram(binwidth=0.5, color="black", fill="pink1") + 
  labs(title="singular style value histogram")
histogram
histogram <- ggplot(results, aes(x = note_points)) + 
  geom_histogram(binwidth=0.5, color="black", fill="plum1") + 
  labs(title="sum style value histogram")
histogram
histogram <- ggplot(results, aes(x = gate)) + 
  geom_histogram(binwidth=1, color="black", fill="indianred1") + 
  labs(title="gate histogram")
histogram
histogram <- ggplot(results, aes(x = wind)) + 
  geom_histogram(binwidth=0.1, color="black", fill="yellow1") + 
  labs(title="wind strength histogram")
histogram
histogram <- ggplot(results, aes(x = wind_comp)) + 
  geom_histogram(binwidth=0.5, color="black", fill="goldenrod1") + 
  labs(title="wind compensation points histogram")
histogram
histogram <- ggplot(results, aes(x = gate_points)) + 
  geom_histogram(binwidth=0.1, color="black", fill="darkorchid1") + 
  labs(title="gate points histogram")
histogram

#zakres, œrednia, kwantyle, wariancja, odchylenie standardowe,skoœnoœæ, kurtoza
#poszczególnych zmiennych
range(results$points)
mean(results$points)
quantile(results$points, c(0.25, 0.5, 0.75)) 
var(results$points)
sd(results$points)
skewness(results$points)
kurtosis(results$points)


range(results$speed)
mean(results$speed)
quantile(results$speed, c(0.25, 0.5, 0.75)) 
var(results$speed)
sd(results$speed)
skewness(results$speed)
kurtosis(results$speed)

range(results$dist)
mean(results$dist)
quantile(results$dist, c(0.25, 0.5, 0.75)) 
var(results$dist)
sd(results$dist)
skewness(results$dist)
kurtosis

range(results$dist_points)
mean(results$dist_points)
quantile(results$dist_points, c(0.25, 0.5, 0.75)) 
var(results$dist_points)
sd(results$dist_points)
skewness(results$dist_points)
kurtosis(results$dist_points)

range(results$gate_points)
mean(results$gate_points)
quantile(results$gate_points, c(0.25, 0.5, 0.75)) 
var(results$gate_points)
sd(results$gate_points)
skewness(results$gate_points)
kurtosis(results$gate_points)

range(results$gate)
mean(results$gate)
quantile(results$gate, c(0.25, 0.5, 0.75)) 
var(results$gate)
sd(results$gate)
skewness(results$gate)
kurtosis(results$gate)

range(results$wind)
mean(results$wind)
quantile(results$wind, c(0.25, 0.5, 0.75)) 
var(results$wind)
sd(results$wind)
skewness(results$wind)
kurtosis(results$wind)

range(results$wind_comp)
mean(results$wind_comp)
quantile(results$wind_comp, c(0.25, 0.5, 0.75)) 
var(results$wind_comp)
sd(results$wind_comp)
skewness(results$wind_comp)
kurtosis(results$wind_comp)

range(results$note_points)
mean(results$note_points)
quantile(results$note_points, c(0.25, 0.5, 0.75)) 
var(results$note_points)
sd(results$note_points)
skewness(results$note_points)
kurtosis(results$note_points)

range(s_notes_data$values)
mean(s_notes_data$values)
quantile(s_notes_data$values, c(0.25, 0.5, 0.75)) 
var(s_notes_data$values)
sd(s_notes_data$values)
skewness(s_notes_data$values)
kurtosis(s_notes_data$values)

#funkcje do wyliczania przedzia³ów ufnoœci:
getCI_mean_start <- function(x){return (c(mean(x) - qt(1-0.05/2, length(x)-1)*sd(x)/ sqrt(length(x))))}
getCI_mean_end <- function(x) {return (c(mean(x) + qt(1-0.05/2, length(x)-1)*sd(x)/ sqrt(length(x))))}
getCI_var_start <- function(x) {return (c(var(x)*(length(x)-1)/qchisq(1-0.05/2, length(x)-1)))}
getCI_var_end <- function(x) {return (c(var(x)*(length(x)-1)/qchisq(0.05/2, length(x)-1)))}

#testy normalnoœci cech - qq plot

qqnorm(results$points)
qqline(results$points)

qqnorm(results$speed)
qqline(results$speed)

qqnorm(results$dist)
qqline(results$dist)

qqnorm(results$dist_points)
qqline(results$dist_points)


qqnorm(results$note_points)
qqline(results$note_points)

qqnorm(results$gate_points)
qqline(results$gate_points)

qqnorm(results$gate)
qqline(results$gate)

qqnorm(results$wind)
qqline(results$wind)

qqnorm(s_notes_data$values)
qqline(s_notes_data$values)

qqnorm(results$wind_comp)
qqline(results$wind_comp)

#testy normalnosci cech - test Andersona-Darlinga

ad.test(results$points)
ad.test(results$speed)
ad.test(results$dist)
ad.test(results$dist_points)
ad.test(s_notes_data$values)
ad.test(results$note_points)
ad.test(results$gate)
ad.test(results$wind)
ad.test(results$wind_comp)
ad.test(results$gate_points)


#macierz kowariancji, korelacji
cov(results[,c(1,2, 3, 4, 10, 11, 16, 17, 18)])
cor(results[,c(1,2, 3, 4, 10, 11, 16, 17, 18)])

#wykresy sprawdzajace zaleznosc liniowa
wind_cor_plot <- ggplot(results, aes(x = wind, y = wind_comp), xlab="wind",
                        ylab="comp") +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  labs(title="Wykres zale¿noœci wartoœci przelicznika za wiatr od prêdkoœci wiatru")
wind_cor_plot 

wind_speed_plot <- ggplot(results, aes(x = wind, y = speed), xlab="wind",
                        ylab="speed") +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  labs(title="Wykres zale¿noœci szybkoœci na progu od prêdkoœci wiatru")
wind_speed_plot 

dist_notepoints_plot <- ggplot(results, aes(x = dist, y = note_points), 
                               xlab="dist",
                          ylab="note_points") +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "lm") +
  labs(title="Wykres zale¿noœci sumy not sêdziowskich od odleg³oœci")
dist_notepoints_plot


#regresja liniowa

summary(lm(results$wind_comp ~ results$wind))
summary(lm(results$note_points ~ results$dist))
summary(lm(results$wind_comp ~ results$wind))


