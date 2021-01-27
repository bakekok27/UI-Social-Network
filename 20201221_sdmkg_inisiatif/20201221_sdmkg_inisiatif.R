#---------------------------------------------------------------------------------
# 20201210_sdmkg_inisiatif.R 
# Seksi: Library, load data, objek igraph, statistik 
#        deskriptif jaringan sosial, visualisasi igraph, uji hipotesis
# Data: vmrad.csv, emrada.csv, emradnon.csv
# N = 20
# Variabel: inisiatif, efikasi, betweenness & pagerank centrality
# Library : Igraph v1.2.5, tidyverse v1.3.0, psych v2.0.9
# Author: ARMAND WIRJAWAN, DHIRA KOMALA NURBANI, KAMALIA
# 21 Desember 2020
#---------------------------------------------------------------------------------

########################
# (1) LIBRARY
########################

### Load Packages
library(tidyverse)
library(igraph)
library(psych)

### Set seed angka random untuk memastikan reproducibility.
set.seed (12345)

########################
# (2) Load Data
########################

### Memasukkan Dataframe Vertex

# Jika tidak menulis 'header = True,' maka R tidak akan menganggap
# baris pertama sebagai header dan akan digunakan dalam analisis data.

vmrad <- read.csv("vmrad.csv", header=TRUE, nrows = 20) 
str(vmrad)

## Memasukkan Dataframe Edgelist Jaringan Akademik
emrada <- read.csv("emrada.csv", header=TRUE)
str(emrada)

## Memasukkan Dataframe Edgelist Jaringan Non-Akademik
emradnon <- read.csv("emradnon.csv", header=TRUE)
str(emradnon)


########################
# (3) Objek Igraph
########################

### Membuat Objek Igraph

## Buat Objek Igraph Interaksi Jaringan Akademik
gmrada <- graph_from_data_frame(d=emrada, vertices=vmrad, directed=T)

# Atribut 'weight' dibuat untuk memudahkan analisis data centrality
# dan analisis komunitas
gmrada <- set_edge_attr(gmrada, 
                        name = "weight", 
                        value = E(gmrada)$tota)

# Data raw memiliki banyak baris yang tidak mengandung interaksi
# Igraph akan menghitung semua interaksi, walaupun interaksi
# sebenarnya tidak ada. Oleh karena itu, baris-baris tanpa interaksi
# harus dihapus terlebih dahulu dengan kode delete_edges di bawah.

gmrada <- delete_edges(gmrada, which(E(gmrada)$weight == 0))

## Buat Objek Igraph Interaksi Jaringan Non-Akademik
gmradnon <- graph_from_data_frame(d=emradnon, vertices=vmrad, directed=T)

gmradnon <- set_edge_attr(gmradnon, 
                          name = "weight", 
                          value = emradnon$totn)

gmradnon <- delete_edges(gmradnon, which(E(gmradnon)$weight == 0))

## Identifikasi Karakteristik Objek Igraph

# Memastikan bahwa setiap jaringan memiliki bobot pada edge
is_weighted(gmrada)
is_weighted(gmradnon)

# Memastikan bahwa jaringan sudah memiliki arah interaksi
is_directed(gmrada) 
is_directed(gmradnon)

# Analisis cek objek Igraph menunjukkan bahwa objek igraph gmrada dan 
# gmradnon sudah memiliki bobot dan arah interaksi. 

########################
# (4) Statistik Deskriptif Jaringan Sosial
########################

### Size

## Jaringan Akademik

# Jumlah edges total pada jaringan akademik
gsize(gmrada)
# Size, atau jumlah edges (interaksi) dalam jaringan akademik 
# selama 8 hari adalah 150

## Jaringan Non-Akademik

gsize(gmradnon) 
# Terdapat 39 edges (interaksi) dalam jaringan non-akademik selama 8 hari.
# Jumlah interaksi akademik empat kali lebih banyak dibanding interaksi 
# non-akademik.

### Density

## Jaringan Akademik

# Persentase edges yang terbentuk
edge_density(gmrada)
# Dari semua edges yang dapat terbentuk, ~39.47% dari semua edges terbentuk

## Jaringan Non-Akademik

### Diameter & Mean Distance

## Jaringan Akademik

diameter(gmrada) # Jarak terdekat (geodesic) antara 2 node terjauh
get_diameter(gmrada) # jalur edges 2 node terjauh
mean_distance(gmrada) # jarak rata-rata antara 2 node

# Jarak antara 2 node terjauh dalam jaringan akademik adalah 11 edges, 
# dari node 9 ke 17. 
# Secara rata-rata, jarak antara 2 node adalah 1,74 edges.

# Untuk mengetahui signifikansi dari mean distance 1,74,
# erdos-renyi game dilakukan untuk membanding mean distance
# jaringan akademik dengan 1.000 jaringan random dengan density yang sama

# Membuat daftar yang berisi 1.000 item
gmrada.list <- vector('list',1000)

# Mengisi setiap item dengan jaringan sosial yang dibuat secara acak
# Density setiap jaringan sama dengan jaringan sosial akademik
for(i in 1:1000){
  gmrada.list[[i]] <- erdos.renyi.game(n = gorder(gmrada), 
                                       p.or.m = edge_density(gmrada), 
                                       type = "gnp") 
}

# Mengaplikasikan formula mean_distance kepada setiap jaringan sosial ciptaan
# erdos-renyi game
gmrada.erd <- unlist(lapply(gmrada.list, mean_distance, directed = T))

# Melihat persentase jaringan sosial acak yang memiliki mean distance 
# lebih rendah (i.e. lebih terhubung) dibanding jaringan akademik MRAD 2020.
mean(gmrada.erd< mean_distance(gmrada))

# Keterhubungan antara 2 node dalam jaringan akademik rendah dibanding
# ~95% dari jaringan yang dibentuk erdos-renyi game, mengindikasikan
# keterhubungan yang sangat buruk. 

# Memvisualisasi persebaran mean distance

## Menyiapkan Tema Visualisasi

tema <- theme(panel.background = element_rect(fill = "linen"), 
              plot.title = element_text(size = 16, face = "bold"))

# Objek erdos-renyi diubah menjadi data frame terlebih dahulu
# supaya dapat dibaca oleh fungsi ggplot. 

gmrada.erd.df <- data.frame(gmrada.erd)

ggplot(gmrada.erd.df, aes(gmrada.erd)) +
  geom_histogram(bins = 40) +
  geom_vline(xintercept = mean_distance(gmrada), 
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "Persebaran Erdos Renyi Game (1000x)", 
       subtitle = "Jaringan Akademik",
       x = "Mean Distance", y = "Frekuensi") +
  geom_text(x = mean_distance(gmrada) + 0.05, 
            y = 80, label= "Mean Dist. Aktual", size = 3) +
  tema

## Jaringan Non-Akademik

gmradnon.list <- vector('list',1000)

for(i in 1:1000){
  gmradnon.list[[i]] <- erdos.renyi.game(n = gorder(gmradnon), 
                                         p.or.m = edge_density(gmradnon), 
                                         type = "gnp") 
}

gmradnon.erd <- unlist(lapply(gmradnon.list, mean_distance, directed = T))
mean(gmradnon.erd< mean_distance(gmradnon))

gmradnon.erd.df <- data.frame(gmradnon.erd)

ggplot(gmradnon.erd.df, aes(gmradnon.erd)) +
  geom_histogram(bins = 40) +
  geom_vline(xintercept = mean_distance(gmradnon), 
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "Persebaran Erdos Renyi Game (1000x)",
       subtitle = "Jaringan Non-Akademik",
       x = "Mean Distance", y = "Frekuensi") +
  geom_text(x = mean_distance(gmradnon) + 0.45, 
            y = 80, label= "Mean Dist. Aktual", size = 3) +
  tema

### Component

## Jaringan Akademik

# Jumlah kelompok jaringan yang sama sekali tidak terhubung
components(gmrada)
# Hanya ada 1 komponen dalam jaringan non-akademik, semua node terhubung
# baik secara langsung maupun tidak langsung.

## Jaringan Non-Akademik

components(gmradnon)
# Hanya ada 1 komponen dalam jaringan non-akademik, semua node terhubung
# baik secara langsung maupun tidak langsung.

### Cluster

## Jaringan Akademik

# Mengartikan subjaringan yang berisi nodes yang saling terkoneksi satu sama lain
# Analisis komunitas dilakukan menggunakan metode cluster Louvain, 
# karena dianggap lebih efektif untuk mencari komunitas dalam waktu singkat
# (Rahiminejad et al., 2019)

# Objek Igraph gmrada harus dibuat tidak terarah untuk analisis cluster louvain

gmrada.c <- cluster_louvain(as.undirected(gmrada))
length(gmrada.c) # jumlah komunitas
sizes(gmrada.c) # jumlah node dalam setiap komunitas

# 1 2 3 4 
# 4 8 2 6 

# Tabel di atas adalah jumlah orang dalam setiap cluster jaringan akademik
# Berdasarkan hasil analisis cluster, terdapat 4 cluster dalam jaringan akademik.

## Jaringan Non-Akademik

gmradnon.c <- cluster_louvain(as.undirected(gmradnon))

sizes(gmradnon.c)
# 1 2 3 4 
# 6 3 8 3 
# Berdasarkan hasil analisis cluster, terdapat 4 cluster 
# dalam jaringan non-akademik

### Centrality Jaringan Sosial

## Betweenness Centrality Jaringan Akademik

# Betweenness centrality menggambarkan sejauh mana suatu node
# berdiri di antara node-node lainnya.

bet.aka <- round(betweenness(gmrada),2)
bet.aka

#     1      2      3      4      5      6      7      8      9     10    
# 37.03   3.90   3.17   4.53  23.02  38.54  18.00 113.02   0.00  22.51   
#    11     12     13    14     15     16     17     18     19     20 
#  0.00   0.00  30.52  17.00   0.00   0.00  23.09   2.50 117.31  58.03 

# Node 19 memiliki betweenness centrality tertinggi, yaitu 117,31.
# Hal tersebut mengindikasikan bahwa node 19 memiliki kontrol informasi
# tertingi, karena banyak node melewati 

## Betweenness Centrality Jaringan Non-Akademik

bet.non <- betweenness(gmradnon)
bet.non

#   1   2   3   4   5   6   7   8   9  10  
#  42  96   0 105   0  28  24  91   0   0   

#  11  12  13  14  15  16  17  18  19  20
#   0  49   0   0   0  76   0   0 110   3 

# Node 19 dan 4 memiliki betweenness centrality yang tinggi 
# dibanding lainnya, yaitu 110 dan 105.

## PageRank Centrality Jaringan Akademik

# Seberapa brepengaruhnya suatu node berdasarkan jumlah node
# yang mengarah kepada dirinya, dan jumlah node yang mengarah 
# kepada node-node yang mengarah pada dirinya.
# Skor PageRank berbentuk persentase

pr.aka <- round(page_rank(gmrada)$vector*100, 2)
pr.aka

#     1    2    3    4    5    6    7    8    9   10    
#  5.23 4.50 7.64 7.36 3.01 2.94 4.82 5.29 6.73 3.30 
#    11   12   13   14   15   16   17   18   19  20 
#  3.77 8.00 3.31 5.16 2.52 4.94 3.91 2.53 9.99  5.04 

# Dalam jaringan ini, node dengan pagerank centrality tertinggi 
# adalah node 19, dengan 10%, menandakan node ini paling berpengaruh 
# di dalam jaringan akademiik

## PageRank Centrality Jaringan Non-Akademik
pr.non <- round(page_rank(gmradnon)$vector*100, 2)
pr.non

#       1     2     3     4     5     6     7     8     9    10          
#   12.24  7.39 12.97  8.18  3.35  1.85  1.97  3.78  2.02  3.50 
#       11   12    13    14    15    16    17    18    19    20  
#     1.96  9.56  1.64  3.73  3.35  4.54 1.82  2.53 10.65  2.96 

# Dalam jaringan ini, pagerank centrality tertinggi adalah node 3, yaitu 12.9% 
# yang menandakan node ini paling berpengaruh di dalam jaringan ini.


########################
# (5) Visualisasi Igraph
########################

### Setting Warna

## Setting warna variabel sex

# Warna lavenderblush untuk lelaki; hotpink untuk perempuan

color.sex <- c("lightskyblue", "hotpink") 

## Setting warna variabel peminatan

# Warna plum untuk sosial, rosybrown untuk perkembangan, 
# skyblue untuk PIO, peachpuff untuk pendidikan

color.peminatan <- c("plum", "rosybrown", 
                     "skyblue", "peachpuff") 

# Memasukkan setting warna sex ke objek Igraph 'gmrada' dan 'gmradnon'

V(gmrada)$color.sex <- color.sex[V(gmrada)$sex] 
V(gmradnon)$color.sex <- color.sex[V(gmrada)$sex]

# Memasukkan setting warna peminantan ke objek Igraph 'gmrada' dan 'gmradnon'

V(gmrada)$color.peminatan <- color.peminatan[V(gmrada)$peminatan]
V(gmradnon)$color.peminatan <- color.peminatan[V(gmrada)$peminatan] 

### Membuat Grafik

## Visualisasi Diameter Akademik

E(gmrada)$color <- "grey60"
E(gmrada)$width <- 1
E(gmrada, path = unlist(get_diameter(gmrada, weights = NA)))$color <- "tan1"
E(gmrada, path = unlist(get_diameter(gmrada, weights = NA)))$width <- 3
par(mar = c(4, 1, 3, 1), bg = "seashell")
plot(gmrada, edge.arrow.size=.2,
     vertex.size = pr.aka*3,
     vertex.color = V(gmrada)$color.peminatan,
     layout = layout_with_fr(gmrada),
     main = "Visualisasi Jaringan Akademik")
legend('bottomleft', 'Jalur 2 Node Terjauh', 
       lty=1, col='black', lwd = 3, 
       box.lty = 0, xpd = T, inset = c(0,-0.15), pt.cex=2, cex=.8)
legend('bottomright', c("Psikologi Sosial","Psikologi Perkembangan", 
                        "Psikologi Industri dan Organisasi", 
                        "Psikologi Pendidikan"), pch=21,
       pt.bg = color.peminatan, pt.cex=2, cex=.8, bty="n",
       xpd = T, inset = c(0,-0.15))

## Visualisasi Diameter Non-Akademik

E(gmradnon)$color <- "grey60"
E(gmradnon)$width <- 1
E(gmradnon, path = unlist(get_diameter(gmradnon, weights = NA)))$color <- "tan1"
E(gmradnon, path = unlist(get_diameter(gmradnon, weights = NA)))$width <- 3
par(mar = c(4, 1, 3, 1), bg = "seashell")
plot(gmradnon, edge.arrow.size=.2,
     vertex.size = pr.non*2.5,
     vertex.color = V(gmradnon)$color.peminatan,
     layout = layout_with_gem(gmradnon),
     main = "Visualisasi Jaringan Non-Akademik")
legend('bottomleft', 'Jalur 2 Node Terjauh', 
       lty=1, col='black', lwd = 3, 
       box.lty = 0, xpd = T, inset = c(0,-0.15), pt.cex=2, cex=.8)
legend('bottomright', c("Psikologi Sosial","Psikologi Perkembangan", 
                         "Psikologi Industri dan Organisasi", 
                         "Psikologi Pendidikan"), pch=21,
       pt.bg = color.peminatan, pt.cex=2, cex=.8, bty="n",
       xpd = T, inset = c(0,-0.15))


## Visualisasi Analisis Komunitas Jaringan Akademik

par(mar = c(4, 1, 3, 1), bg = "seashell")
plot(gmrada.c, 
     gmrada, 
     edge.arrow.size=.3, 
     edge.width = 1.2,
     col = V(gmrada)$color.peminatan,
     main= "Grafik Komunitas Jaringan Akademik")
legend(x=-1.5, y=-1.1, c("Psikologi Sosial","Psikologi Perkembangan", 
                         "Psikologi Industri dan Organisasi", 
                         "Psikologi Pendidikan"), pch=21,
       pt.bg = color.peminatan, pt.cex=2, cex=.8, bty="n")

# Pada plot komunitas berdasarkan jaringan akademik, secara umum 
# cluster terbentuk sesuai peminatan. Namun terdapat outlier pada node 
# ke-14 yang berasal dari peminatan sosial, yang berada pada cluster perkembangan.

## Visualisasi Analisis Komunitas Jaringan Non-Akademik

par(mar = c(4, 1, 3, 1), bg = "seashell")
plot(gmradnon.c, 
     gmradnon, 
     edge.arrow.size=.3,  
     edge.width = 1.2,
     col = V(gmradnon)$color.peminatan, 
     main= "Grafik Komunitas Jaringan Non-Akademik")
legend(x=-1.5, y=-1.1, c("Psikologi Sosial","Psikologi Perkembangan", 
                         "Psikologi Industri dan Organisasi", 
                         "Psikologi Pendidikan"), pch=21,
       pt.bg = color.peminatan, pt.cex=2, cex=.8, bty="n")

# Pada plot komunitas jaringan non-akademik, cluster 
# lebih bervariasi antara peminatan. Hubungan antara 
# cluster dan peminatan tidak sekuat jaringan akademik.
# Ini mungkin terjadi karena mahasiswa sudah saling kenal 
# sebelum memilih peminatan, seperti node 12 dan 19 yang sudah 
# menikah. Node 8, 16 dan 17 juga terhubung walaupun 
# tidak sama peminatan. Ketiga node ini tidak memiliki latar 
# belakang psikologi waktu S1; mereka mungkin saling kenal 
# saat mengambil semester matrikulasi.

## Visualisasi Betweenness Centrality Jaringan Akademik

# Ukuran node dalam visualisasi ditentukan oleh betweenness centrality, 
# Warna setiap node ditentukan oleh jenis kelamin

# Plot diberi edit vertex size, arrow size dan layout agar terlihat lebih rapih

par(mar = c(4, 1, 3, 1), bg = "seashell")
plot(
  gmrada, 
  vertex.size=bet.aka/3 + 5,  
  edge.arrow.width = 1, 
  vertex.label.color="black", 
  edge.color = "dimgrey",
  edge.arrow.size=.2, 
  vertex.color = V(gmrada)$color.sex, 
  layout = layout_with_fr(gmrada),
  main = "Betweenness Centrality Kelas MRAD",
  sub="Interaksi Akademik"
)
legend(x=-1.5, y=-1.1, c("Pria", "Wanita"), pch=21,
       pt.bg = color.sex, pt.cex=2, cex=.8, bty="n")

## Visualisasi Betweenness Centrality Jaringan Non-Akademik

par(mar = c(4, 1, 3, 1), bg = "seashell")
plot(gmradnon, 
     vertex.size=bet.non/4 + 5,  
     edge.arrow.width = 1, 
     vertex.label.color="black", 
     edge.color = "dimgrey",
     edge.arrow.size=.2, 
     vertex.color = V(gmrada)$color.sex, 
     layout = layout_with_gem(gmradnon), 
     main = "Betweenness Centrality Kelas MRAD", 
     sub = "Interaksi Non-Akademik")
legend(x=-1.5, y=-1.1, c("Pria", "Wanita"), pch=21,
       pt.bg = color.sex, pt.cex=2, cex=.8, bty="n")

# Visualisasi Plot berdasarkan PageRank Centrality
# Ukuran node dalam visualisasi ditentukan oleh pagerank centrality, 
# dan warna setiap node ditentukan oleh peminatan mahasiswa

## Visualisasi PageRank Centrality Jaringan Akademik

par(mar = c(4, 1, 3, 1), bg = "seashell")
plot(
  gmrada, 
  vertex.size=pr.aka*3, 
  vertex.label.color="black", 
  edge.arrow.size=.2,
  edge.color = "dimgrey",
  vertex.color = V(gmrada)$color.peminatan, 
  layout = layout_with_fr(gmrada),
  main = "PageRank Centrality Kelas MRAD",
  sub = "Interaksi Akademik"
)
legend(x=-1.5, y=-1.1, 
       c("Psikologi Sosial","Psikologi Perkembangan", 
         "Psikologi Industri dan Organisasi", 
         "Psikologi Pendidikan"), pch=21,
       pt.bg = color.peminatan, pt.cex=2, cex=.8, bty="n")

## Visualisasi PageRank Centrality Jaringan Non-Akademik

par(mar = c(4, 1, 3, 1), bg = "seashell")
plot(
  gmradnon, 
  vertex.size=pr.non*3, 
  vertex.label.color="black", 
  edge.arrow.size=.2, 
  edge.color = "dimgrey",
  vertex.color = V(gmrada)$color.peminatan, 
  layout = layout_with_gem(gmradnon),
  main = "PageRank Centrality Kelas MRAD",
  sub = "Interaksi Non-Akademik"
)
legend(x=-1.5, y=-1.1, 
       c("Psikologi Sosial","Psikologi Perkembangan", 
         "Psikologi Industri dan Organisasi", 
         "Psikologi Pendidikan"), pch=21,pt.bg = 
         color.peminatan, pt.cex=2, cex=.8, bty="n")

########################
# (6) Uji Hipotesis
########################

### Persiapan Uji Hipotesis

## Memasukkan Data Centrality dalam Dataframe VMRAD

# Agar dapat melakukan uji hipotesis menggunakan 1 dataframe
# dan agar peneliti dapat menggunakan satu pintu untuk membandingkan 
# skor centrality dengan variabel-variabel lain.

vmrad$bet.aka <- bet.aka
vmrad$pr.aka <- pr.aka
vmrad$bet.non <- bet.non
vmrad$pr.non <- pr.non

## Megubah format data sex, jurusan s1 dan universitas s1 menjadi categorical

# Ini dilakukan untuk mempermudah visualisasi hipotesis,
# dan karena variabel-variabel tersebut memang dapat dianggap categorical

vmrad$sex <- as.factor(vmrad$sex)
vmrad$jurusans1 <- as.factor(vmrad$jurusans1)
vmrad$univs1 <- as.factor(vmrad$univs1)

# Karena banyaknya pilihan universitas S1, dan karena hipotesis kami hanya membedakan
# S1 UI dan non-UI, maka univs1 akan diubah menjadi 1 = UI, 2 = non-UI

vmrad$univs1new <- ifelse(vmrad$univs1 == "1", "1","2")

## Membuat objek r mengandung daftar variabel yang ingin diuji

varb <- c(
  "inisiatif", 
  "efikasi", 
  "dayatarik", 
  "sex", 
  "jurusans1", 
  "univs1new", 
  "bet.aka", 
  "pr.aka")

## Analisis deskriptif variabel yang ingin diuji

describe(vmrad[, varb])[,c(3:5,8:13)]

#            vars  n  mean    sd median trimmed   mad  min    max  range  skew kurtosis   se
# inisiatif     1 20  4.65  0.75   4.50    4.56  0.74 4.00   6.00   2.00  0.60    -1.08 0.17
# efikasi       2 20  4.35  0.81   4.00    4.38  1.48 3.00   6.00   3.00 -0.10    -0.83 0.18
# dayatarik     3 20  2.50  0.51   2.50    2.45  0.74 2.00   3.50   1.50  0.47    -1.23 0.11
# sex*          3 20  1.90  0.31   2.00    2.00  0.00 1.00   2.00   1.00 -2.47     4.32 0.07
# jurusans1*    4 20  1.20  0.41   1.00    1.12  0.00 1.00   2.00   1.00  1.39    -0.07 0.09
# univs1*       5 20  5.70  3.84   5.50    5.44  5.19 1.00  13.00  12.00  0.38    -1.21 0.86
# bet.aka       6 20 25.61 34.65  17.50   17.61 24.09 0.00 117.31 117.31  1.62     1.63 7.75
# pr.aka        7 20  5.00  2.04   4.88    4.81  2.34 2.52   9.99   7.47  0.76    -0.33 0.46

### Pembuatan Objek Linear Model

# Linear model tidak menganalisis jaringan non-akademik
# karena jaringan non-akademik tidak termasuk bagian dari
# hipotesis penelitian

## Pengaruh Inisiatif terhadap Betweenness Centrality (H1a)

inisa.bet <- lm(bet.aka ~ inisiatif, data = vmrad)

## Pengaruh Inisiatif terhadap PageRank Centrality (H1b)

inisa.pr <- lm(pr.aka ~ inisiatif, data = vmrad)

## Pengaruh Sex sebagai Moderator Hubungan Inisiatif - Betweenness (H1c)

inisia.bet.sex <- lm(bet.aka ~ inisiatif + sex + inisiatif*sex, data = vmrad)

## Pengaruh Sex sebagai Moderator Hubungan Inisiatif - PageRank (H1d)

inisa.pr.sex <- lm(pr.aka ~ inisiatif + sex + inisiatif*sex, data = vmrad)

## Pengaruh Efikasi terhadap Betweenness Centrality (H2a)

efika.bet <- lm(bet.aka ~ efikasi, data = vmrad)

## Pengaruh Jurusan S1 sebagai Moderator Hubungan Efikasi - Betweenness (H2b)

efika.bet.js1 <- lm(bet.aka ~ efikasi + jurusans1 + efikasi*jurusans1, data = vmrad)

## Pengaruh Universitas S1 sebagai Moderator Hubungan Efikasi - Betweenness (H2c)

efika.bet.us1 <- lm(bet.aka ~ efikasi + univs1new + efikasi*univs1new, data = vmrad)

## Pengaruh Daya Tarik sebagai Moderator Hubungan Efikasi - Betweenness (H2d)

efika.bet.dt <- lm(bet.aka ~ efikasi + dayatarik + efikasi*dayatarik, data = vmrad)

### Pelaksanaan Uji Hipotesis

## Pengaruh Inisiatif terhadap Betweenness Centrality (H1a)

summary(inisa.bet)$coef

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)
# (Intercept)   64.819     50.725   1.278    0.218
# inisiatif     -8.432     10.778  -0.782    0.444


## Pengaruh Inisiatif terhadap PageRank Centrality (H1b)

summary(inisa.pr)$coef

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept) 0.041185   0.030089   1.369    0.188
# inisiatif   0.001896   0.006393   0.297    0.770

## Pengaruh Sex sebagai Moderator Hubungan Inisiatif - Betweenness (H1c)

summary(inisia.bet.sex)$coef

# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)   
# (Intercept)      703.86     218.16   3.226  0.00528 **
# inisiatif       -117.31      39.50  -2.970  0.00903 **
# sex2            -631.90     222.61  -2.839  0.01186 * 
# inisiatif:sex2   106.33      40.66   2.615  0.01875 * 

## Pengaruh Sex sebagai Moderator Hubungan Inisiatif - PageRank (H1d)

summary(inisa.pr.sex)$coef

# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)   
# (Intercept)     0.45000    0.13479   3.338  0.00417 **
# inisiatif      -0.07000    0.02441  -2.868  0.01116 * 
# sex2           -0.41605    0.13754  -3.025  0.00805 **
# inisiatif:sex2  0.07316    0.02512   2.912  0.01018 * 

## Pengaruh Efikasi terhadap Betweenness Centrality (H2a)

summary(efika.bet)$coef

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)
# (Intercept)   61.833     43.574   1.419    0.173
# efikasi       -8.327      9.855  -0.845    0.409

## Pengaruh Jurusan S1 sebagai Moderator Hubungan Efikasi - Betweenness (H2b)

summary(efika.bet.js1)$coef

# Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)  
# (Intercept)          17.002     43.705   0.389   0.7024  
# efikasi               1.125      9.566   0.118   0.9078  
# jurusans12          388.968    138.910   2.800   0.0128 *
# efikasi:jurusans12  -98.775     36.216  -2.727   0.0149 *

## Pengaruh Universitas S1 sebagai Moderator Hubungan Efikasi - Betweenness (H2c)

round(summary(efika.bet.us1)$coef, 2)

#                    Estimate Std. Error t value Pr(>|t|)
# (Intercept)          -59.16     161.18   -0.37     0.72
# efikasi               20.55      43.60    0.47     0.64
# univs1new2           147.97     168.90    0.88     0.39
# efikasi:univs1new2   -34.32      45.00   -0.76     0.46


# Hasil analisis Universitas S1 sebagai moderator
# tidak signifikan, mungkin karena 2 dari 3 mahasiswa yang S1 di
# Universitas Indonesia memiliki latar belakang jurusan
# non-psikologi.

## Pengaruh Daya Tarik sebagai Moderator Hubungan Efikasi - Betweenness (H2d)

round(summary(efika.bet.dt)$coeff,2)

#                   Estimate Std. Error t value Pr(>|t|)
# (Intercept)         435.81     236.86    1.84     0.08
# efikasi            -101.34      55.42   -1.83     0.09
# dayatarik          -152.89      95.90   -1.59     0.13
# efikasi:dayatarik    37.82      22.31    1.70     0.11

# Tidak signifikan, walaupun daya tarik 
# menghasilkan hubungan positif antara efikasi diri dan
# betweenness centrality.

### Visualisasi Uji Hipotesis

# Hiptesis dengan hasil signifikan akan diberi visualisasi

## Pengaruh Sex sebagai Moderator Hubungan Inisiatif - Betweenness (H1c)

ggplot(data = vmrad, aes(x = inisiatif, y = bet.aka, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm", level = 0.90) +
  scale_color_manual(values = c(color.sex), labels = c("Pria", "Wanita")) +
  labs(title = "Visualisasi Betweenness Centrality dan Inisiatif", 
       subtitle = "Jaringan Akademik dengan Moderator Sex",
       x = "Inisiatif Mahasiswa", y = "Betweenness Centrality",
       color = "Jenis Kelamin") +
  tema

## Pengaruh Sex sebagai Moderator Hubungan Inisiatif - PageRank (H1d)

ggplot(data = vmrad, aes(x = inisiatif, y = pr.aka, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm", level = 0.90) +
  scale_color_manual(values = c(color.sex), labels = c("Pria", "Wanita")) +
  labs(title = "Visualisasi PageRank Centrality dan Inisiatif", 
       subtitle = "Jaringan Akademik dengan Moderator Sex",
       x = "Inisiatif Mahasiswa", y = "PageRank Centrality",
       color = "Jenis Kelamin") +
  tema

## Pengaruh Jurusan S1 sebagai Moderator Hubungan Efikasi - Betweenness (H2b)

ggplot(data = vmrad, aes(x = efikasi, y = bet.aka, color = jurusans1)) +
  geom_point() +
  geom_smooth(method = "lm", level = 0.90) +
  scale_color_manual(values = c("violetred1", "lightgreen"),
                     labels = c("Psikologi", "Non-Psikologi")) +
  labs(title = "Visualisasi PageRank Centrality dan Inisiatif", 
       subtitle = "Jaringan Akademik dengan Moderator Jurusan S1",
       x = "Efikasi Diri Mahasiswa", y = "PageRank Centrality",
       color = "Jurusan S1") +
  tema

### Kesimpulan
# Dalam ketiga model linear ini, alasan mengapa main effect antara
# variabel prediktor dan indeks centrality tidak signifikan, namun
# efek interaksi memiliki hasil signifikan adalah karena crossover
# effect. Dalam kata lain, linear model untuk setiap skor moderator
# (cth: psikologi vs. non-psikologi) memiliki pengaruh yang berbeda
# terhadap interaksi antara variabel predictor dan outcome. Ini dapat
# dilihat pada interaksi antara efikasi diri dan betweenness centrality,
# di mana bagi mahasiswa psikologi, semakin tinggi efikasi diri, semakin
# tinggi betweenness centrality-nya. Sebaliknya mahasiswa non-psikologi
# memiliki hubungan negatif antara efikasi diri dan betweenness centrality.

#########################################################################

### Informasi mengenai versi dan package apa saja yang digunakan
sessionInfo() 

view(vmrad)
view(emrada)
view(emradnon)
