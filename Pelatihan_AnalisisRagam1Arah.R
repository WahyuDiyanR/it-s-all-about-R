## ----paket-------------------------------------
# Jangan lupa pasang paket di bawah ini terlebih dahulu sebelum menjalankan kode library
library(car) 
library(agricolae) 
library(ggpubr) 


## ----dataset-----------------------------------
# Menyusun dataset
# Jika ulangan berbeda tiap kelompok maka ubah nilai ulangan di setiap kelompok)
Pakan <- c(rep("Pakan 1", 5), rep("Pakan 2", 5), rep("Pakan 3", 5)) 

# Nilai berat dimasukkan melalui kode ini
Berat <- c(122,  131,  128,  120, 134, 133,  137,  141,  147, 149, 137,  142,  147,  151, 153) 

# Kode untuk menentukan Pakan sebagai faktor
Pakan <- factor(Pakan) 

# Kode untuk menyusun dataset
data <- data.frame(Pakan, Berat) 

# Mengubah dataset ke dalam format .csv dan menyimpannya di salah satu folder di komputer atau laptop
write.csv(data, "C:/DATA/Untan/Lab Ekologi/Pelatihan R/sapi.csv")


## ----data--------------------------------------
df <- read.csv("C:/DATA/Untan/Lab Ekologi/Pelatihan R/sapi.csv")


## ----Levene------------------------------------
# leveneTest : kode untuk instruksi pemeriksaan kehomogenan ragam dengan Uji Levene. 
# Jika nilai p > 0.05, asumsi kehomogenan ragam terpenuhi.
leveneTest(Berat ~ Pakan, data = df) 


## ----normal------------------------------------
# shapiro.test: kode untuk instruksi pemeriksaan normalitas data dengan Uji Shapiro. 
# Jika nilai p > 0.05, asumsi kenormalan distribusi data terpenuhi.
shapiro.test(df$Berat) 

# qqPlot: kode untuk menyusun Q-Q plot untuk mengetahui apakah data memenuhi asumsi atau tidak. 
# Jika titik data berada di antara garis putus-putus, data memenuhi asumsi.
qqPlot(df$Berat) 


## ----Anova-------------------------------------
# lm: kode instruksi untuk penyusunan model linier dalam analisa data
model = lm(Berat ~ Pakan, data = df) 
 
# anova: kode instruksi untuk analisa ragam satu jalur
anova(model) 

# Jika nilai p < 0,05, berarti bahwa ada perbedaan yang signifikan antar kelompok/perlakuan.


## ----Tukey-------------------------------------
# HSD.test: kode instruksi untuk uji lanjut - Tukey HSD)
(HSD.test(model, "Pakan", unbalanced = FALSE)) 


## ----grafik------------------------------------
ggbarplot(df, x="Pakan", y="Berat", ylim= c(0, 170), add = "mean_se", xlab= "Jenis Pakan", ylab="Berat (kg") +
  geom_text(x = 1, y = 140, label ="b") +
  geom_text(x = 2, y = 155, label ="a") +
  geom_text(x = 3, y = 160, label ="a") +
  geom_text(x = 1.1, y = 160, label ="ANOVA 1 Jalur, p < 0,05")

