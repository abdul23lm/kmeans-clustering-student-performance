#import package
library(openxlsx)
library(bpa)
library(ggplot2)
library(stats)
library(factoextra)
library(NbClust)

#import dataset
performa.siswa <- read.xlsx("D:/Document/Remember ME/Github/R/kmeans-clustering-student-performance/Dataset/StudentsPerformance.xlsx")

#view tipe data
View(performa.siswa)
str(performa.siswa)

#konsistensi kolom
names(performa.siswa)
names(performa.siswa)[c(1:8)] <- c("jenis.kelamin","ras","pendidikan.ortu","makan.siang","kursus.persiapan.ujian","nilai.mtk","nilai.membaca","nilai.menulis")

#cek jumlah missing value NA 
sum(is.na(performa.siswa[1:8])==TRUE)


#cek keragaman data jenis kelamin
basic_pattern_analysis(performa.siswa, unique_only = TRUE)

#cek jenis kelamin tidak lazim menggunakan function grepl
grepl(pattern="[^Aaw.,]",basic_pattern_analysis(performa.siswa$jenis.kelamin))
performa.siswa[grepl(pattern="[^Aaw.,]", basic_pattern_analysis(performa.siswa$jenis.kelamin)),]

#cek ras tidak lazim menggunakan function grepl
grepl(pattern="[^Aaw.,]",basic_pattern_analysis(performa.siswa$ras))
performa.siswa[grepl(pattern="[^Aaw.,]", basic_pattern_analysis(performa.siswa$ras)),]

#cek pendidikan ortu tidak lazim menggunakan function grepl
grepl(pattern="[^Aaw.,]",basic_pattern_analysis(performa.siswa$pendidikan.ortu))
performa.siswa[grepl(pattern="[^Aaw.,]", basic_pattern_analysis(performa.siswa$pendidikan.ortu)),]

#cek makan siang tidak lazim menggunakan function grepl
grepl(pattern="[^Aaw.,]",basic_pattern_analysis(performa.siswa$makan.siang))
performa.siswa[grepl(pattern="[^Aaw.,]", basic_pattern_analysis(performa.siswa$makan.siang)),]

#cek makan siang tidak lazim menggunakan function grepl
grepl(pattern="[^Aaw.,]",basic_pattern_analysis(performa.siswa$kursus.persiapan.ujian))
performa.siswa[grepl(pattern="[^Aaw.,]", basic_pattern_analysis(performa.siswa$kursus.persiapan.ujian)),]

#konvert character to factor
performa.siswa$jenis.kelamin <- as.factor(performa.siswa$jenis.kelamin)
performa.siswa$ras <- as.factor(performa.siswa$ras)
performa.siswa$pendidikan.ortu <- as.factor(performa.siswa$pendidikan.ortu)
performa.siswa$makan.siang <- as.factor(performa.siswa$makan.siang)
performa.siswa$kursus.persiapan.ujian <- as.factor(performa.siswa$kursus.persiapan.ujian)


#mengubah menjadi factor dan beri levels

performa.siswa$jenis.kelamin <- factor(performa.siswa$jenis.kelamin, levels = c("male","female"))

performa.siswa$ras <- factor(performa.siswa$ras, levels = c("group A","group B","group C","group D","group E"))

performa.siswa$pendidikan.ortu <- factor(performa.siswa$pendidikan.ortu, levels = c("high school","some high school","some college","associate's degree","bachelor's degree","master's degree"))

performa.siswa$makan.siang <- factor(performa.siswa$makan.siang, levels = c("free/reduced","standard"))

performa.siswa$kursus.persiapan.ujian <- factor(performa.siswa$kursus.persiapan.ujian, levels = c("none","completed"))

#konnvert to integer
performa.siswa$jenis.kelamin <- as.integer(performa.siswa$jenis.kelamin)

performa.siswa$ras <- as.integer(performa.siswa$ras)

performa.siswa$pendidikan.ortu <- as.integer(performa.siswa$pendidikan.ortu)

performa.siswa$makan.siang <- as.integer(performa.siswa$makan.siang)

performa.siswa$kursus.persiapan.ujian <- as.integer(performa.siswa$kursus.persiapan.ujian)

View(performa.siswa)

write.xlsx(file = "D:/Document/Remember ME/Github/R/kmeans-clustering-student-performance/Dataset/StudentsPerformanceNormalisasi.xlsx", performa.siswa)

#summery
summary(performa.siswa)

#menetukan nilai k optimal dengan method elbow effect
performa.siswa
#field_yang_digunakan = c("jenis.kelamin","ras","pendidikan.ortu","makan.siang","kursus.persiapan.ujian","nilai.mtk","nilai.menulis","nilai.membaca")
field_yang_digunakan = c("nilai.mtk","nilai.menulis","nilai.membaca")
##melihat nilai sse
set.seed(100)
sse <- sapply(1:10,
              function(param_k)
              {
                kmeans(performa.siswa[field_yang_digunakan],param_k, nstart=25)$tot.withinss
              }
)

#grafik elbow effect
jumlah_cluster_max <- 10

ssdata = data.frame(cluster=c(1:jumlah_cluster_max), sse)
ggplot(ssdata, aes(x=cluster, y=sse)) + geom_line(color="red") + geom_point() + ylab("Within Cluster Sum of Squares") + xlab("Jumlah Cluster") + geom_text(aes(label=format(round(sse,2), nsmall = 2)), hjust=-0.2, vjust=-0.5) + scale_x_discrete(limits=c(1:jumlah_cluster_max))

#metode kmeans
segmentasi <- kmeans(x=performa.siswa[field_yang_digunakan], centers = 5, nstart = 25)
#tampilkan hasil kmeans
segmentasi

set.seed(123)             # produce the same sample
km.res <- kmeans(performa.siswa[field_yang_digunakan], 5, iter.max = 10, nstart = 25)


fviz_nbclust(performa.siswa[field_yang_digunakan], kmeans, method = "silhouette") + labs(subtitle = "Silhouette Method")

p1 <- fviz_cluster(km.res, geom = "point", data = performa.siswa[field_yang_digunakan]) + ggtitle("k = 5")
p1
