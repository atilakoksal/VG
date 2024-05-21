#########VERİ GÖRSELLEŞTİRME FİNAL ÖDEVİ  -  Atila Köksal  ######################

##################################################################################
#Gerekli Paket ve Kütüphaneler
##################################################################################

install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("corrplot")
library(corrplot)
install.packages("ggridges")
library(ggridges)

#Veri dosyasını alma

ds = read.csv("heart_statlog_cleveland_hungary_final.csv")
df = data.frame(ds)

# NA Değerleri Ortalama ile Doldurma (df1)
ort1 = mean(df$cholesterol [df$cholesterol  != 0], na.rm = TRUE)
df$cholesterol [df$cholesterol  == 0] <- ort1 

ort2 = mean(df$resting.bp.s [df$resting.bp.s  != 0], na.rm = TRUE)
df$resting.bp.s [df$resting.bp.s  == 0] <- ort2 
df1 = df 

# Veri setine, target_yüzde isminde yuzdelik gosteren sutun eklemek  (df2)

toplam <- sum(df1$target, na.rm = TRUE)
df2 <- df1 %>%  mutate(target_yuzde = (target / toplam) * 100)

# Kategorik değişkenleri  cevirme (df3)

df2$sex <- factor(df2$sex, levels = c(0, 1), labels = c("Female", "Male"))
df2$chest.pain.type <- factor(df2$chest.pain.type, levels = c(1, 2, 3, 4), labels = c("Type 1", "Type 2", "Type 3", "Type 4"))
df2$fasting.blood.sugar <- factor(df2$fasting.blood.sugar, levels = c(0, 1), labels = c("Normal", "High"))
df2$resting.ecg <- factor(df2$resting.ecg, levels = c(0, 1, 2), labels = c("Normal", "Abnormality", "Hypertrophy"))
df2$exercise.angina <- factor(df2$exercise.angina, levels = c(0, 1), labels = c("No", "Yes"))
df2$ST.slope <- factor(df2$ST.slope, levels = c(1, 2, 3), labels = c("Upsloping", "Flat", "Downsloping"))
df2$target <- factor(df2$target, levels = c(0, 1), labels = c("No Disease", "Disease"))

df3 = df2

#Hasta ve Sağlıklı iki ayri sutun olusturmak (df4_hasta, df4_saglikli)

df4_hasta <- subset(df3, target == "Disease")
df4_saglikli <- subset(df3, target == "No Disease")



########################################################################################
########************* ÇİZGİ GRAFİKLER  ***********************
#####################################################################################*


### 1 Yaş -Ortalama Kolesterol   ++++++++++++++++++ ************************************  1

df4_hasta_summary <- aggregate(cholesterol ~ age, data = df4_hasta, FUN = mean)
df4_saglikli_summary <- aggregate(cholesterol ~ age, data = df4_saglikli, FUN = mean)

ggplot() +
  geom_line(data = df4_hasta_summary, aes(x = age, y = cholesterol, color = "Hasta"), size = 1.5) +
  geom_line(data = df4_saglikli_summary, aes(x = age, y = cholesterol, color = "Sağlıklı"), size = 1.5) +
  labs(x = "Yaş", y = "", color = "", title = "Yaşa Ortalama Kolesterol") +
  scale_color_manual(values = c("Hasta" = "orangered2", "Sağlıklı" = "palegreen4")) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),  # Tüm metinleri 14 punto olarak ayarlar
    title = element_text(size = 16),  # Başlık metnini 16 punto olarak ayarlar
    axis.title.x = element_text(size = 16),  # x eksen başlık font boyutu
    axis.title.y = element_text(size = 16),  # y eksen başlık font boyutu
    axis.text.x = element_text(size = 14),  # x eksenindeki rakamların font boyutu
    axis.text.y = element_text(size = 14),  # y eksenindeki rakamların font boyutu
    legend.title = element_text(size = 16),  # lejant başlık font boyutu
    legend.text = element_text(size = 14)  # lejant metin font boyutu
  )

### 2 ortalama yaş-tansiyon   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++  2

df4_hasta_summary <- aggregate(resting.bp.s ~ age, data = df4_hasta, FUN = mean)
df4_saglikli_summary <- aggregate(resting.bp.s ~ age, data = df4_saglikli, FUN = mean)

ggplot() +
  geom_line(data = df4_hasta_summary, aes(x = age, y = resting.bp.s, color = "Hasta"), size = 1.5) +
  geom_line(data = df4_saglikli_summary, aes(x = age, y = resting.bp.s, color = "Sağlıklı"), size = 1.5) +
  labs(x = "Yaş", y = "", color = "", title = "Ortalama Tansiyon") +
  scale_color_manual(values = c("Hasta" = "orangered2", "Sağlıklı" = "palegreen4")) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),  
    title = element_text(size = 16),  
    axis.title.x = element_text(size = 16),  
    axis.title.y = element_text(size = 16),  
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 14),  
    legend.title = element_text(size = 16),  
    legend.text = element_text(size = 14)  
  )

### 3 ortalama yaş-max nabız   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 3

df4_hasta_summary <- aggregate(max.heart.rate ~ age, data = df4_hasta, FUN = mean)
df4_saglikli_summary <- aggregate(max.heart.rate ~ age, data = df4_saglikli, FUN = mean)

ggplot() +
  geom_line(data = df4_hasta_summary, aes(x = age, y = max.heart.rate, color = "Hasta"), size = 1.5) +
  geom_line(data = df4_saglikli_summary, aes(x = age, y = max.heart.rate, color = "Sağlıklı"), size = 1.5) +
  labs(x = "Yaş", y = "", color = "", title = "Ortalama Nabız") +
  scale_color_manual(values = c("Hasta" = "orangered2", "Sağlıklı" = "palegreen4")) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),  
    title = element_text(size = 16),  
    axis.title.x = element_text(size = 16),  
    axis.title.y = element_text(size = 16),  
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 14),  
    legend.title = element_text(size = 16),  
    legend.text = element_text(size = 14)  
  )

### 4 ortalama yaş -oldpeak   +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 4

df4_hasta_summary <- aggregate(oldpeak ~ age, data = df4_hasta, FUN = mean)
df4_saglikli_summary <- aggregate(oldpeak ~ age, data = df4_saglikli, FUN = mean)

ggplot() +
  geom_line(data = df4_hasta_summary, aes(x = age, y = oldpeak, color = "Hasta"), size = 1.5) +
  geom_line(data = df4_saglikli_summary, aes(x = age, y = oldpeak, color = "Sağlıklı"), size = 1.5) +
  labs(x = "Yaş", y = "", color = "", title = "Ortalama EKG Oldpeak") +
  scale_color_manual(values = c("Hasta" = "orangered2", "Sağlıklı" = "palegreen4")) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),  
    title = element_text(size = 16),  
    axis.title.x = element_text(size = 16),  
    axis.title.y = element_text(size = 16),  
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 14),  
    legend.title = element_text(size = 16),  
    legend.text = element_text(size = 14)  
  )


##################################################################################
########################### KERNEL YOĞUNLUK TAHMİNİ GRAGİKLERİ ###############
##################################################################################
##   1- Kolesterol##-------------------------------------------------------------------  5

ggplot() +
  geom_density(data = df4_hasta, aes(x = cholesterol, fill = "Hasta"), alpha = 0.5, adjust = 1.5) +
  geom_density(data = df4_saglikli, aes(x = cholesterol, fill = "Sağlıklı"), alpha = 0.5, adjust = 1.5) +
  labs(title = "Kolesterol düzeyi karşılaştırması",
       x = "Kolesterol",
       y = "Yoğunluk") +
  scale_fill_manual(values = c("Hasta" = "red", "Sağlıklı" = "blue"),
                    labels = c("Hasta", "Sağlıklı")) +
  guides(fill = guide_legend(title = NULL)) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),  
    plot.title = element_text(size = 20),  
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),  
    axis.text.x = element_text(size = 12),  
    axis.text.y = element_text(size = 12),  
    legend.title = element_text(size = 14),  
    legend.text = element_text(size = 12)  
  )

##   2- Tansiyon##-------------------------------------------------------------------  6

ggplot() +
  geom_density(data = df4_hasta, aes(x = resting.bp.s, fill = "Hasta"), alpha = 0.5, adjust = 1.5) +
  geom_density(data = df4_saglikli, aes(x = resting.bp.s, fill = "Sağlıklı"), alpha = 0.5, adjust = 1.5) +
  labs(title = "Tansiyon düzeyi karşılaştırması",
       x = "Tansiyon",
       y = "Yoğunluk") +
  scale_fill_manual(values = c("Hasta" = "red", "Sağlıklı" = "blue"),
                    labels = c("Hasta", "Sağlıklı")) +
  guides(fill = guide_legend(title = NULL)) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),  
    plot.title = element_text(size = 20),  
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),  
    axis.text.x = element_text(size = 12),  
    axis.text.y = element_text(size = 12),  
    legend.title = element_text(size = 14),  
    legend.text = element_text(size = 12)  
  )

##   3- NABIZ ##-------------------------------------------------------------------  7

ggplot() +
  geom_density(data = df4_hasta, aes(x = max.heart.rate, fill = "Hasta"), alpha = 0.5, adjust = 1.5) +
  geom_density(data = df4_saglikli, aes(x = max.heart.rate, fill = "Sağlıklı"), alpha = 0.5, adjust = 1.5) +
  labs(title = "Nabız düzeyi karşılaştırması",
       x = "Nabız",
       y = "Yoğunluk") +
  scale_fill_manual(values = c("Hasta" = "red", "Sağlıklı" = "blue"),
                    labels = c("Hasta", "Sağlıklı")) +
  guides(fill = guide_legend(title = NULL)) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),  
    plot.title = element_text(size = 20),  
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),  
    axis.text.x = element_text(size = 12),  
    axis.text.y = element_text(size = 12),  
    legend.title = element_text(size = 14),  
    legend.text = element_text(size = 12)  
  )

##   4- Yaş ##-------------------------------------------------------------------  8

ggplot() +
  geom_density(data = df4_hasta, aes(x = age, fill = "Hasta"), alpha = 0.5, adjust = 1.5) +
  geom_density(data = df4_saglikli, aes(x = age, fill = "Sağlıklı"), alpha = 0.5, adjust = 1.5) +
  labs(title = "Yaş karşılaştırması",
       x = "Yaş",
       y = "Yoğunluk") +
  scale_fill_manual(values = c("Hasta" = "red", "Sağlıklı" = "blue"),
                    labels = c("Hasta", "Sağlıklı")) +
  guides(fill = guide_legend(title = NULL)) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),  
    plot.title = element_text(size = 20),  
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),  
    axis.text.x = element_text(size = 12),  
    axis.text.y = element_text(size = 12),  
    legend.title = element_text(size = 14),  
    legend.text = element_text(size = 12)  
  )

##   5- EKG Oldpeak Karşılaştırması ##------------------------------------------  9

ggplot() +
  geom_density(data = df4_hasta, aes(x = oldpeak  , fill = "Hasta"), alpha = 0.5, adjust = 1.5) +
  geom_density(data = df4_saglikli, aes(x = oldpeak  , fill = "Sağlıklı"), alpha = 0.5, adjust = 1.5) +
  labs(title = "EKG Oldpeak karşılaştırması",
       x = "EKG Oldpeak",
       y = "Yoğunluk") +
  scale_fill_manual(values = c("Hasta" = "red", "Sağlıklı" = "blue"),
                    labels = c("Hasta", "Sağlıklı")) +
  guides(fill = guide_legend(title = NULL)) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),  
    plot.title = element_text(size = 20),  
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),  
    axis.text.x = element_text(size = 12),  
    axis.text.y = element_text(size = 12),  
    legend.title = element_text(size = 14),  
    legend.text = element_text(size = 12)  
  )

##***************************************************************
#######################################################################################
#######CİNSİYETE GÖRE KALP HASTALIĞIORANI ÇUBUK GRAFİK+++++++++++++++++++

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 10

# Cinsiyet ve hedef değişkenlerini değiştirme
df_gender <- df3 %>% 
  mutate(sex = ifelse(sex == "Female", "Kadın", "Erkek"),
         target = ifelse(target == "No Disease", "Sağlıklı", "Kalp Hastası")) %>% 
  group_by(sex, target) %>% 
  summarise(count = n()) %>% 
  mutate(percent = count / sum(count) * 100)

# Çubuk grafiği oluşturma
ggplot(data = df_gender, aes(x = sex, y = percent, fill = target)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "% Oran", fill = "",
       title = "Cinsiyete Göre Kalp Hastalığı Oranları") +
  scale_fill_manual(values = c("Sağlıklı" = "palegreen3", "Kalp Hastası" = "orangered2")) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),  
    plot.title = element_text(size = 20),  
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),  
    axis.text.x = element_text(size = 12),  
    axis.text.y = element_text(size = 12),  
    legend.title = element_text(size = 14),  
    legend.text = element_text(size = 12)  
  )

###############################################################################
# *************   KORELASYON MATRİSİ  ***********++++++++++++++++++++++++++++++++++++  11
##################################################################################

# Korelasyon matrisini hesapla
correlation_matrix <- cor(df3[, sapply(df3, is.numeric)])

# Satır ve sütun isimlerini güncelle
colnames(correlation_matrix) <- c("Yaş", "Tansiyon", "Kolesterol", "Maks. Nabız", "Oldpeak", "Hastalık Oranı")  
rownames(correlation_matrix) <- c("Yaş", "Tansiyon", "Kolesterol", "Maks. Nabız", "Oldpeak", "Hastalık Oranı")  

# corrplot ile grafik oluştur
corrplot(correlation_matrix, method = "color", colnames = colnames(correlation_matrix), 
         rownames = rownames(correlation_matrix), 
         tl.col = "black", tl.srt = 45, tl.cex = 1,
         legend.cex = 5)  # Lejant içindeki rakamların büyüklüğünü ayarla

###############################################################################

####################### KEMAN GRAFİĞİ - OLDPEAK+++++++++++++++++++++++++++++++++++++++ 12
###############################################################################

df3_1 <- df3 #Türkçe yazdırabilmek için
df3_1$target <- ifelse(df3_1$target == "Disease", "Kalp Hastası", "Sağlıklı")

ggplot(df3_1, aes(x = target, y = oldpeak, fill = target)) +
  geom_violin(alpha = 0.6) +
  scale_fill_manual(values = c("red", "blue"), labels = c("No Disease", "Disease")) +
  labs(title = "Sağlık Durumuna Göre EKG Oldpeak Dağılımı",
       x = "",
       y = "") +
  theme_minimal()+
  theme(legend.position = "none",
        text = element_text(size = 20),  
        plot.title = element_text(size = 20),  
        axis.title.x = element_text(size = 18),  
        axis.title.y = element_text(size = 18))  

##################################################################################

###############################################################################
