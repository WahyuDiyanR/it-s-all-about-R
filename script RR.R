names(dataset)
mean(dataset$Produksi)
View(dataset)
summary(dataset)
stem(dataset[,Produksi])
hist(dataset[,Produksi])
attach(data.untuk.tugas.besar)
boxplot(data.untuk.tugas.besar$Produksi)
summary(data.untuk.tugas.besar)
hist(data.untuk.tugas.besar$Produksi)
attach(istriku)
summary(istriku)
datasaya <- read.table("D:/dataP1.txt",header=TRUE)
datasaya3 <- datasaya[-1,] (menghapus baris)
datasaya2 <- datasaya[,-1] (menghapus kolom)
dataku <- read.csv("D:/dataP2.csv",header=TRUE)
dataku <- read.csv("D:/dataP2.csv",header=TRUE,sep=";")
library(foreign)
dataku <- read.spss("D:/dataP3.sav",to.data.frame=TRUE)
pakan <- c(rep("pakan 1", 5), rep("pakan 2", 5),rep("pakan 3", 5))
