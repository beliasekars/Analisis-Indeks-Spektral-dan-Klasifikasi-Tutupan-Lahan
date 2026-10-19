# ============================================================
# PENGOLAHAN CITRA SENTINEL-2 LEVEL-2A: NDVI, NDBI, NDWI dan NDCI (Cloud)
# Oleh :
# 1. Anis Tika Utami (5016241031)
# 2. Belia Sekar Saraswara (5016241033)
# ============================================================
# CATATAN PERUBAHAN:
# - NDCI sekarang = NDCI AWAN (Cloud Index): (NIR - Merah) / (NIR + Merah)
#   memakai B8A (865 nm) dan B04 (665 nm), dihitung TANPA cloud mask.
# - NDCI klorofil lama (B05 - B04)/(B05 + B04) tetap dihitung, diberi nama
#   NDCI_Klorofil sebagai pembanding.
# ============================================================


# ------------------------------------------------------------
# 1. PERSIAPAN PACKAGES
# ------------------------------------------------------------

packages <- c('terra','sf','dplyr','readr','xml2','shiny','shinydashboard',
              'leaflet','raster','DT','htmltools')
new <- setdiff(packages, rownames(installed.packages()))
if (length(new)) install.packages(new, dependencies = TRUE)
sapply(packages, requireNamespace, quietly = TRUE)



# ------------------------------------------------------------
# 2. KONFIGURASI DIREKTORI DATA
# ------------------------------------------------------------
folder_sentinel <- "D:/Downloads/S2B_MSIL2A_20261014T023639_N0513_R089_T49MDM_20261014T053426.SAFE/GRANULE/L2A_T49MDM_A050165_20261014T025716/IMG_DATA/R20m"

file_shp <- paste0(
  "C:/Users/BELIA SEKAR SARAS/Documents/TEKNIK GEOMATIKA/PEMROGRAMAN SPASIAL/ETS_PEMSPA/ADMINISTRASIKECAMATAN_AR_25K")

folder_output <- "C:/Users/BELIA SEKAR SARAS/Documents/TEKNIK GEOMATIKA/PEMROGRAMAN SPASIAL/ETS_PEMSPA/OUTPUT"

# membuat folder hasil bila belum tersedia
dir.create(
  folder_output,
  recursive = TRUE,
  showWarnings = FALSE
)

# memeriksa data sentinel dan shp
if (!dir.exists(folder_sentinel)) {
  stop("Folder Sentinel-2 tidak ditemukan.")
}

if (!file.exists(file_shp)) {
  stop("File SHP Kabupaten Sleman tidak ditemukan.")
}

cat("\n--- DIREKTORI TERDETEKSI ---\n")



# ------------------------------------------------------------
# 3. MEMBACA FILE BAND SENTINEL-2
# ------------------------------------------------------------
semua_file <- list.files(
  folder_sentinel,
  pattern = "\\.jp2$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)
cari_band <- function(kode_band, resolusi) {
  
  pola <- paste0(
    "_", kode_band, "_",
    resolusi, "m\\.jp2$"
  )
  
  hasil <- grep(
    pola,
    semua_file,
    value = TRUE,
    ignore.case = TRUE
  )
  
  if (length(hasil) == 0) {
    stop(
      paste("Band tidak ditemukan:", kode_band)
    )
  }
  
  if (length(hasil) > 1) {
    stop(paste(
      "Ditemukan lebih dari satu file untuk",
      kode_band,
      "- pastikan folder hanya memuat",
      "satu tile dan satu waktu akuisisi."
    ))
  }
  
  return(hasil)
}

# Identifikasi band
file_B03 <- cari_band("B03", 20)
file_B04 <- cari_band("B04", 20)
file_B05 <- cari_band("B05", 20)
file_B08 <- cari_band("B08", 20)
file_B8A <- cari_band("B8A", 20)   # NIR sempit 865 nm, untuk NDCI (awan)
file_B11 <- cari_band("B11", 20)
file_SCL <- cari_band("SCL", 20)

cat("\n--- BAND BERHASIL DITEMUKAN ---\n")

print(c(
  file_B03, file_B04, file_B05,
  file_B08, file_B8A, file_B11, file_SCL
))




# ------------------------------------------------------------
# 4. MEMBACA BAND MENJADI RASTER
# ------------------------------------------------------------
library(terra)

B03 <- rast(file_B03)
B04 <- rast(file_B04)
B05 <- rast(file_B05)
B08 <- rast(file_B08)
B8A <- rast(file_B8A)
B11 <- rast(file_B11)
SCL <- rast(file_SCL)

cat("\n--- INFORMASI RASTER ---\n")

print(B04)
print(res(B04))
print(crs(B04))




# ------------------------------------------------------------
# 5. MENYATUKAN BAND MENJADI MULTIBAND
# ------------------------------------------------------------

sentinel_stack <- c(
  B03,
  B04,
  B05,
  B08,
  B8A,
  B11,
  SCL
)

names(sentinel_stack) <- c(
  "B03",
  "B04",
  "B05",
  "B08",
  "B8A",
  "B11",
  "SCL"
)

cat("\n--- RASTER STACK ---\n")
print(sentinel_stack)





# ------------------------------------------------
# 6. MENYIMPAN RASTER MULTIBAND
# ------------------------------------------------
folder_output <- "C:/Users/BELIA SEKAR SARAS/Documents/TEKNIK GEOMATIKA/PEMROGRAMAN SPASIAL/ETS_PEMSPA/OUTPUT"
if (!dir.exists(folder_output)) {
  dir.create(
    folder_output,
    recursive = TRUE,
    showWarnings = FALSE
  )
}
if (!dir.exists(folder_output)) {
  stop("Folder output gagal dibuat. Periksa lokasi folder.")
}

file_stack <- file.path(
  folder_output,
  "Sentinel2_Multiband_20m.tif"
)

writeRaster(
  sentinel_stack,
  file_stack,
  overwrite = TRUE,
  gdal = c("COMPRESS=LZW")
)

if (file.exists(file_stack)) {
  cat("\nRaster multiband berhasil disimpan!\n")
  cat("Lokasi:", file_stack, "\n")
} else {
  stop("Raster gagal disimpan.")
}



# ------------------------------------------------------------
# 7. MEMBACA SHAPEFILE KABUPATEN SLEMAN
# ------------------------------------------------------------

sleman <- vect(file_shp)

cat("\n--- INFORMASI SHAPEFILE ---\n")

print(sleman)
print(crs(sleman))
print(names(sleman))

if (!nzchar(crs(sleman))) {
  stop("CRS shapefile tidak diketahui.")
}

if (!nzchar(crs(B04))) {
  stop("CRS raster Sentinel-2 tidak diketahui.")
}

# Cek Geometri Polygon
tipe_geom <- geomtype(sleman)

if (!all(tipe_geom %in% c("polygons"))) {
  stop("Shapefile harus berupa polygon.")
}



# ------------------------------------------------------------
# 8. MENYAMAKAN SISTEM KOORDINAT
# ------------------------------------------------------------
sleman_proj <- project(
  sleman,
  crs(sentinel_stack)
)

# Menggabungkan polygon menjadi satu wilayah (dissolve)
sleman_proj <- aggregate(sleman_proj)

cat("\n--- SHAPEFILE SELESAI DIPROYEKSIKAN ---\n")

print(crs(sleman_proj))





# ------------------------------------------------------------
# 9. MELAKUKAN CROP RASTER
# ------------------------------------------------------------

# Crop berdasarkan wilayah 
sentinel_crop <- crop(
  sentinel_stack,
  sleman_proj
)

cat("\n--- CROP SELESAI ---\n")
print(sentinel_crop)






# ------------------------------------------------------------
# 10. MASK BERDASARKAN POLYGON SLEMAN
# ------------------------------------------------------------
sentinel_sleman <- mask(
  sentinel_crop,
  sleman_proj
)

# Memeriksa validasi data
jumlah_valid <- global(
  !is.na(sentinel_sleman[["B04"]]),
  fun = "sum",
  na.rm = TRUE
)[1, 1]

if (is.na(jumlah_valid) || jumlah_valid == 0) {
  stop("Tidak terdapat piksel valid pada wilayah Sleman.")
}

cat("\n--- MASK SELESAI ---\n")
print(sentinel_sleman)






# ------------------------------------------------------------
# 11. MENYIMPAN HASIL CROP DAN MASK
# ------------------------------------------------------------

file_hasil <- file.path(
  folder_output,
  "Sentinel2_Sleman_20m.tif"
)

writeRaster(
  sentinel_sleman,
  file_hasil,
  overwrite = TRUE,
  gdal = c("COMPRESS=LZW")
)

cat("\n--- FILE BERHASIL DISIMPAN ---\n")
print(file_hasil)




# ------------------------------------------------------------
# 12. VISUALISASI CITRA HASIL CROP
# ------------------------------------------------------------
plot(
  sentinel_sleman[["SCL"]],
  main = "Sentinel-2 SCL - Kabupaten Sleman"
)

# Menambahkan batas wilayah administrasi
lines(
  sleman_proj,
  col = "red",
  lwd = 1.5
)

# Visualisasi band NIR
plot(
  sentinel_sleman[["B08"]],
  main = "Sentinel-2 NIR - Kabupaten Sleman"
)

lines(
  sleman_proj,
  col = "black",
  lwd = 1.5
)



# ------------------------------------------------------------
# 13. PETA 17 KECAMATAN KABUPATEN SLEMAN
# ------------------------------------------------------------

folder_sleman <- "C:/Users/BELIA SEKAR SARAS/Documents/TEKNIK GEOMATIKA/PEMROGRAMAN SPASIAL/ETS_PEMSPA/ADMINISTRASIKECAMATAN_AR_25K"

kec_sleman <- c("Berbah", "Cangkringan", "Depok", "Gamping", "Godean",
                "Kalasan", "Minggir", "Mlati", "Moyudan", "Ngaglik",
                "Ngemplak", "Pakem", "Prambanan", "Seyegan", "Sleman",
                "Tempel", "Turi")

norm <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- gsub("KECAMATAN|KEC\\.?", "", x)
  gsub("[^A-Z]", "", x)
}
target <- norm(kec_sleman)

# Mencari semua daftar file SHP
daftar_shp <- list.files(
  folder_sleman,
  pattern = "\\.shp$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)
print(daftar_shp)

if (length(daftar_shp) == 0) stop("Tidak ada file .shp di folder Kab. Sleman.")

# Memeriksa setiap file dan setiap kolom teks, hitung jumlah nama yang cocok
terbaik <- list(skor = 0, file = NA, kolom = NA)

for (f in daftar_shp) {
  v <- tryCatch(vect(f), error = function(e) NULL)
  if (is.null(v)) next
  df <- as.data.frame(v)
  for (k in names(df)) {
    if (!is.character(df[[k]])) next
    skor <- length(intersect(unique(norm(df[[k]])), target))
    if (skor > terbaik$skor) {
      terbaik <- list(skor = skor, file = f, kolom = k)
    }
  }
}

cat("\nFile terpilih :", terbaik$file, "\n")
cat("Kolom nama    :", terbaik$kolom, "\n")
cat("Nama cocok    :", terbaik$skor, "dari 17\n")

if (terbaik$skor == 0) {
  stop(paste("Tidak ada kolom yang memuat nama kecamatan Sleman.",
             "Jalankan print(names(vect(file))) dan head(as.data.frame(...))",
             "pada tiap file untuk memeriksa isi atributnya."))
}

# Membaca file terpilih dan menyamakan CRS dengan citra
kec        <- vect(terbaik$file)
kolom_nama <- terbaik$kolom
kec_proj   <- project(kec, crs(sentinel_stack))

# Filter 17 Kecamatan saja
kec_proj <- kec_proj[norm(kec_proj[[kolom_nama, drop = TRUE]]) %in% target, ]

cat("Jumlah fitur setelah disaring:", nrow(kec_proj), "\n")
if (nrow(kec_proj) == 0) stop("Hasil saringan kosong, periksa isi kolom nama.")

# Menyatukan polygon
kec_plot <- aggregate(kec_proj, by = kolom_nama)
cat("Jumlah kecamatan:", nrow(kec_plot), "(seharusnya 17)\n")


# ------------------------------------------------------------
# PETA KECAMATAN DENGAN WARNA CITRA SENTINEL-2
# ------------------------------------------------------------

# Pilih band yang ingin ditampilkan: "B08" (NIR) atau "SCL"
band_tampil <- "SCL"

# Potong citra sesuai batas 17 kecamatan
citra_kec <- mask(
  crop(sentinel_sleman[[band_tampil]], kec_plot),
  kec_plot
)

# Warna raster sama seperti visualisasi sebelumnya (palet bawaan terra)
plot(
  citra_kec,
  main = paste("Sentinel-2", band_tampil, "- 17 Kecamatan Kabupaten Sleman")
)

# Batas kecamatan
lines(
  kec_plot,
  col = "black",
  lwd = 1.5
)

# Nama kecamatan
text(
  kec_plot,
  labels = kolom_nama,
  cex = 0.7,
  halo = TRUE
)






# ------------------------------------------------------------
# 14. REFLEKTANSI, CLOUD MASKING, INDEKS SPEKTRAL, DAN KLASIFIKASI
# ------------------------------------------------------------

# ============ A. PARAMETER ==========
offset_boa <- -1000      # BOA_ADD_OFFSET produk N0400 ke atas (N0513 = -1000)
faktor     <- 10000

thr_ndvi_veg  <- 0.3     # NDVI >= 0.3  -> vegetasi
thr_ndbi_bgn  <- 0       # NDBI >  0    -> bangunan
thr_ndwi_air  <- 0       # NDWI >  0    -> perairan

scl_awan  <- c(3, 8, 9, 10)          # bayangan awan, awan sedang, awan tebal, cirrus
scl_buang <- c(0, 1, 3, 8, 9, 10, 11) # dibuang dari perhitungan indeks

ndci_hanya_air <- TRUE   # (tidak dipakai; sisa dari NDCI klorofil)

# Kelas komposit
# Kelas komposit (4-7 = kelas awan dari band SCL)
id_kelas  <- 0:7
nm_kelas  <- c("Tidak terklasifikasi", "Vegetasi", "Bangunan", "Perairan",
               "Bayangan awan (SCL 3)", "Awan sedang (SCL 8)",
               "Awan tinggi (SCL 9)",   "Awan cirrus (SCL 10)")
col_kelas <- c("grey85", "#1A9850", "#D73027", "#2C7BB6",
               "#5E4FA2", "#FDAE61", "#F46D43", "#ABD9E9")



# ============ B. KOREKSI REFLEKTANSI ==========================
ke_reflektansi <- function(x) {
  r <- (x + offset_boa) / faktor
  clamp(r, lower = 0, upper = 1, values = TRUE)
}

R_B03 <- ke_reflektansi(sentinel_sleman[["B03"]])
R_B04 <- ke_reflektansi(sentinel_sleman[["B04"]])
R_B05 <- ke_reflektansi(sentinel_sleman[["B05"]])
R_B08 <- ke_reflektansi(sentinel_sleman[["B08"]])
R_B8A <- ke_reflektansi(sentinel_sleman[["B8A"]])
R_B11 <- ke_reflektansi(sentinel_sleman[["B11"]])


# ============ C. CLOUD MASKING (SCL) ==========================
scl <- sentinel_sleman[["SCL"]]

# Mask piksel yang dibuang (awan, bayangan, no data, salju)
buang_logis <- scl == scl_buang[1]
for (k in scl_buang[-1]) buang_logis <- buang_logis | (scl == k)
mask_awan <- ifel(buang_logis, NA, 1)

# Peta awan terpisah, dipakai untuk kelas "Awan" pada peta komposit
awan_logis <- scl == scl_awan[1]
for (k in scl_awan[-1]) awan_logis <- awan_logis | (scl == k)
peta_awan <- ifel(awan_logis, 1, 0)

n_valid <- global(!is.na(scl), "sum")[1, 1]
n_awan  <- global(peta_awan, "sum", na.rm = TRUE)[1, 1]
cat("Piksel awan/bayangan awan:", round(100 * n_awan / n_valid, 2), "%\n")

# --- Simpan reflektansi B04 dan B8A SEBELUM cloud mask --------
# NDCI (awan) harus dihitung pada piksel berawan juga, sehingga tidak
# boleh memakai raster yang sudah di-mask awan.
R_B04_awan <- R_B04
R_B8A_awan <- R_B8A

R_B03 <- mask(R_B03, mask_awan)
R_B04 <- mask(R_B04, mask_awan)
R_B05 <- mask(R_B05, mask_awan)
R_B08 <- mask(R_B08, mask_awan)
R_B8A <- mask(R_B8A, mask_awan)
R_B11 <- mask(R_B11, mask_awan)

refl_stack <- c(R_B03, R_B04, R_B05, R_B08, R_B8A, R_B11)
names(refl_stack) <- c("B03", "B04", "B05", "B08", "B8A", "B11")

writeRaster(refl_stack,
            file.path(folder_output, "Sentinel2_Reflektansi_Sleman_20m.tif"),
            overwrite = TRUE, gdal = c("COMPRESS=LZW"))


# ============ D. INDEKS SPEKTRAL ==============================
nd <- function(a, b) (a - b) / (a + b)

NDVI <- nd(R_B08, R_B04)
NDBI <- nd(R_B11, R_B08)
NDWI <- nd(R_B03, R_B08)

# NDCI klorofil (rumus lama, piksel bebas awan) -> diganti nama
NDCI_klorofil <- nd(R_B05, R_B04)

# ---- NDCI (AWAN / CLOUD INDEX) --------------------------------
# NDCI = (I(lambda2) - I(lambda1)) / (I(lambda2) + I(lambda1))
#   lambda1 = merah (0,65-0,67 um)  -> B04  (665 nm)
#   lambda2 = NIR   (0,86-0,87 um)  -> B8A  (865 nm)
# Dihitung dari reflektansi TANPA cloud mask (piksel awan tetap ada).
NDCI <- nd(R_B8A_awan, R_B04_awan)

indeks_stack <- c(NDVI, NDBI, NDWI, NDCI, NDCI_klorofil)
names(indeks_stack) <- c("NDVI", "NDBI", "NDWI", "NDCI", "NDCI_Klorofil")

print(global(indeks_stack, c("min", "max", "mean"), na.rm = TRUE))

# Perbandingan nilai NDCI (awan) pada piksel awan dan non-awan (SCL)
cat("\n--- NDCI (AWAN): PIKSEL AWAN vs NON-AWAN (berdasarkan SCL) ---\n")
ndci_awan_val <- mask(NDCI, ifel(peta_awan == 1, 1, NA))
ndci_cerah_val <- mask(NDCI, ifel(peta_awan == 0, 1, NA))
cat("Rata-rata NDCI piksel awan   :",
    round(global(ndci_awan_val,  "mean", na.rm = TRUE)[1, 1], 4), "\n")
cat("Rata-rata NDCI piksel non-awan:",
    round(global(ndci_cerah_val, "mean", na.rm = TRUE)[1, 1], 4), "\n")

writeRaster(indeks_stack,
            file.path(folder_output, "Indeks_NDVI_NDBI_NDWI_NDCI_Sleman.tif"),
            overwrite = TRUE, gdal = c("COMPRESS=LZW"))


# ============ E. KLASIFIKASI KOMPOSIT (4 KELAS) ===============
# Prioritas: awan (SCL 3, 8, 9, 10) -> perairan -> vegetasi -> bangunan -> lainnya
# Kode: 4 = SCL 3, 5 = SCL 8, 6 = SCL 9, 7 = SCL 10
kelas <- ifel(scl == 3,  4,
              ifel(scl == 8,  5,
                   ifel(scl == 9,  6,
                        ifel(scl == 10, 7,
                             ifel(NDWI > thr_ndwi_air, 3,
                                  ifel(NDVI >= thr_ndvi_veg, 1,
                                       ifel(NDBI > thr_ndbi_bgn, 2, 0)))))))
names(kelas) <- "kelas"

# Batasi ke 17 kecamatan
kelas <- mask(kelas, kec_plot)

# Tabel warna tetap, aman walau ada kelas yang tidak muncul
coltab(kelas) <- data.frame(value = id_kelas, col = col_kelas)

writeRaster(kelas,
            file.path(folder_output, "Klasifikasi_Komposit_Sleman.tif"),
            overwrite = TRUE, datatype = "INT1U")

# Luas tiap kelas (1 piksel 20 m = 400 m2 = 0.04 ha)
tab <- freq(kelas)
tab$kelas_nama <- nm_kelas[match(tab$value, id_kelas)]
tab$luas_ha    <- tab$count * 0.04
tab$persen     <- round(100 * tab$count / sum(tab$count), 2)
print(tab[, c("value", "kelas_nama", "count", "luas_ha", "persen")])



# ============ F. FUNGSI PETA + BATAS & NAMA KECAMATAN =========
tambah_kecamatan <- function(cex_label = 0.5, warna_batas = "black") {
  lines(kec_plot, col = warna_batas, lwd = 1)
  text(kec_plot, labels = kolom_nama, cex = cex_label, halo = TRUE)
}

peta_indeks <- function(r, judul, palet, rentang = c(-1, 1)) {
  plot(r, main = judul, col = palet, range = rentang)
  tambah_kecamatan()
}

gambar_indeks <- function() {
  par(mfrow = c(2, 2), mar = c(2, 2, 3, 4))
  
  peta_indeks(NDVI, "NDVI (vegetasi: >= 0.3)",
              hcl.colors(50, "Greens", rev = TRUE))
  peta_indeks(NDBI, "NDBI (bangunan: > 0)",
              hcl.colors(50, "Reds", rev = TRUE))
  peta_indeks(NDWI, "NDWI (perairan: > 0)",
              hcl.colors(50, "Blues", rev = TRUE))
  
  # Panel 4: NDCI (Cloud) berdasarkan kelas awan SCL
  awan_tampil <- mask(peta_awan, kec_plot)
  plot(awan_tampil, legend = FALSE,
       col = c("grey90", "#7F7F7F"),
       main = "NDCI (Cloud: awan & bayangan awan)")
  tambah_kecamatan()
  legend("bottomleft", legend = c("Bebas awan", "Awan/bayangan"),
         fill = c("grey90", "#7F7F7F"), cex = 0.5, bg = "white")
  
  par(mfrow = c(1, 1))
}


gambar_komposit <- function() {
  
  layout(matrix(c(1, 2), nrow = 1), widths = c(3, 2))
  
  # ================= PANEL 1: PETA =================
  par(mar = c(5, 2, 4, 1), xpd = FALSE)
  
  plot(kelas, legend = FALSE, axes = TRUE,
       main = "Peta Komposit NDVI, NDBI, NDWI dan NDCI (Cloud)\nKabupaten Sleman",
       cex.main = 0.95)
  tambah_kecamatan(cex_label = 0.55)
  
  north(xy = "topright", type = 1, label = "U")
  sbar(d = 10000, xy = "bottomleft", type = "bar", divs = 2,
       below = "km", labels = c(0, 5, 10), cex = 0.7)
  
  mtext("Sumber data: Sentinel-2 Level-2A (Copernicus), akuisisi 14 Oktober 2026",
        side = 1, line = 3.2, adj = 0, cex = 0.6)
  mtext("Sistem koordinat: WGS 84 / UTM Zona 49S",
        side = 1, line = 4.2, adj = 0, cex = 0.6)
  
  # ================= PANEL 2: TABEL LEGENDA =================
  par(mar = c(5, 0, 4, 1), xpd = FALSE)
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1), xaxs = "i", yaxs = "i")
  
  # 1-3 = indeks, 4-7 = awan berdasarkan kelas SCL
  id_tabel <- 1:7
  kriteria <- c(
    paste0("NDVI >= ", thr_ndvi_veg),
    paste0("NDBI > ",  thr_ndbi_bgn),
    paste0("NDWI > ",  thr_ndwi_air),
    "SCL kelas 3 (cloud shadow)",
    "SCL kelas 8 (cloud medium probability)",
    "SCL kelas 9 (cloud high probability)",
    "SCL kelas 10 (thin cirrus)"
  )
  pos     <- match(id_tabel, tab$value)
  luas_ha <- tab$luas_ha[pos];  luas_ha[is.na(luas_ha)] <- 0
  persen  <- tab$persen[pos];   persen[is.na(persen)]   <- 0
  
  n_baris <- length(id_tabel)
  y_awal  <- 0.78
  tinggi  <- 0.075
  y_bawah <- y_awal - (n_baris - 1) * tinggi - 0.05
  
  rect(0.01, y_bawah, 0.99, 0.97, border = "black", lwd = 1)
  rect(0.01, 0.89, 0.99, 0.97, col = "grey85", border = "black")
  text(0.50, 0.93, "LEGENDA KLASIFIKASI", font = 2, cex = 0.75)
  text(0.03, 0.85, "Kelas / Kriteria", adj = 0, font = 2, cex = 0.62)
  text(0.97, 0.85, "Luas",             adj = 1, font = 2, cex = 0.62)
  segments(0.01, 0.82, 0.99, 0.82)
  
  for (i in seq_len(n_baris)) {
    y <- y_awal - (i - 1) * tinggi
    j <- match(id_tabel[i], id_kelas)
    
    rect(0.03, y - 0.025, 0.12, y + 0.025, col = col_kelas[j], border = "black")
    text(0.15, y + 0.015, nm_kelas[j], adj = 0, cex = 0.62, font = 2)
    text(0.15, y - 0.018, kriteria[i], adj = 0, cex = 0.48, col = "grey30")
    text(0.97, y + 0.015, paste0(persen[i], "%"), adj = 1, cex = 0.62)
    text(0.97, y - 0.018,
         paste0(format(round(luas_ha[i]), big.mark = ".", decimal.mark = ","), " ha"),
         adj = 1, cex = 0.5, col = "grey30")
    
    if (i < n_baris) {
      # garis lebih tebal untuk memisahkan kelompok indeks dan kelompok awan
      segments(0.01, y - 0.0375, 0.99, y - 0.0375,
               col = ifelse(i == 3, "grey40", "grey80"),
               lwd = ifelse(i == 3, 1.2, 1))
    }
  }
  
  
  
  layout(1)
}


# ============ G. TAMPILKAN DAN SIMPAN =========================
# Tampil di RStudio
gambar_indeks()
gambar_komposit()

# Peta NDCI (awan) kontinu per kecamatan
gambar_ndci_awan <- function() {
  par(mfrow = c(1, 1), mar = c(3, 3, 3, 5))
  plot(mask(NDCI, kec_plot),
       main = "NDCI (Cloud Index): (B8A - B04) / (B8A + B04)",
       col = hcl.colors(50, "Grays", rev = TRUE), range = c(-1, 1))
  tambah_kecamatan()
}
gambar_ndci_awan()

# Simpan ke PNG (300 dpi)
png(file.path(folder_output, "Peta_Indeks_Kecamatan.png"),
    width = 3000, height = 2800, res = 300)
gambar_indeks()
dev.off()

png(file.path(folder_output, "Peta_Komposit_Kecamatan.png"),
    width = 3600, height = 2400, res = 300)
gambar_komposit()
dev.off()

png(file.path(folder_output, "Peta_NDCI_Awan_Kecamatan.png"),
    width = 2400, height = 2000, res = 300)
gambar_ndci_awan()
dev.off()

cat("\nBagian 14 selesai. Hasil tersimpan di:", folder_output, "\n")







# ------------------------------------------------------------
# 15. STATISTIK ZONAL PER KECAMATAN (EXPORT KE EXCEL)
# ------------------------------------------------------------

# ============ A. PERSIAPAN PACKAGE ============================
if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
library(writexl)

# ============ B. FUNGSI STATISTIK =============================
statistik <- list(
  Mean    = function(x) mean(x,   na.rm = TRUE),
  Maks    = function(x) max(x,    na.rm = TRUE),
  Min     = function(x) min(x,    na.rm = TRUE),
  Median  = function(x) median(x, na.rm = TRUE),
  Std_Dev = function(x) sd(x,     na.rm = TRUE)
)

# Pembungkus agar kecamatan tanpa piksel valid menghasilkan NA, bukan Inf/error
aman <- function(f) function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  f(x)
}

nama_kec <- as.data.frame(kec_plot)[[kolom_nama]]

# ============ C. HITUNG STATISTIK ZONAL PER INDEKS ============
# N_Piksel_Valid = jumlah piksel bernilai (bukan NA) pada indeks tsb.
# NDVI/NDBI/NDWI/NDCI_Klorofil: piksel bebas awan.
#NDCI (awan): seluruh piksel, termasuk piksel berawan.
hitung_zonal <- function(r, nama_indeks) {
  hasil <- data.frame(Kecamatan = nama_kec, stringsAsFactors = FALSE)
  for (s in names(statistik)) {
    nilai <- extract(r, kec_plot, fun = aman(statistik[[s]]), ID = FALSE)
    hasil[[s]] <- round(nilai[[1]], 4)
  }
  n <- extract(r, kec_plot, fun = function(x) sum(!is.na(x)), ID = FALSE)
  hasil$N_Piksel_Valid <- n[[1]]
  hasil
}

zonal_NDVI <- hitung_zonal(indeks_stack[["NDVI"]], "NDVI")
zonal_NDBI <- hitung_zonal(indeks_stack[["NDBI"]], "NDBI")
zonal_NDWI <- hitung_zonal(indeks_stack[["NDWI"]], "NDWI")
zonal_NDCI <- hitung_zonal(indeks_stack[["NDCI"]], "NDCI")                      # AWAN
zonal_NDCI_klorofil <- hitung_zonal(indeks_stack[["NDCI_Klorofil"]], "NDCI_Klorofil")

# ============ D. STATISTIK AWAN PER KECAMATAN =================
# peta_awan bernilai 0/1, sehingga rata-ratanya = proporsi piksel berawan
awan_n   <- extract(peta_awan, kec_plot, fun = function(x) sum(x == 1, na.rm = TRUE), ID = FALSE)
awan_tot <- extract(peta_awan, kec_plot, fun = function(x) sum(!is.na(x)),            ID = FALSE)

zonal_awan <- data.frame(
  Kecamatan       = nama_kec,
  N_Piksel_Awan   = awan_n[[1]],
  N_Piksel_Total  = awan_tot[[1]],
  Persen_Awan     = round(100 * awan_n[[1]] / awan_tot[[1]], 2),
  Luas_Awan_ha    = round(awan_n[[1]] * 0.04, 2),   # 1 piksel 20 m = 0.04 ha
  stringsAsFactors = FALSE
)

# ============ E. TABEL RINGKASAN GABUNGAN =====================
gabung <- function(df, indeks) {
  names(df)[2:6] <- paste0(indeks, "_", names(df)[2:6])
  df[, 1:6]
}

# Kolom NDCI_* = NDCI AWAN. Kolom NDCI_Klorofil_* = NDCI klorofil (pembanding).
ringkasan <- Reduce(function(a, b) merge(a, b, by = "Kecamatan", sort = FALSE),
                    list(gabung(zonal_NDVI, "NDVI"),
                         gabung(zonal_NDBI, "NDBI"),
                         gabung(zonal_NDWI, "NDWI"),
                         gabung(zonal_NDCI, "NDCI"),
                         gabung(zonal_NDCI_klorofil, "NDCI_Klorofil")))
ringkasan <- merge(ringkasan, zonal_awan[, c("Kecamatan", "Persen_Awan")],
                   by = "Kecamatan", sort = FALSE)
ringkasan <- ringkasan[order(ringkasan$Kecamatan), ]

print(ringkasan)

# ============ F. SIMPAN KE EXCEL ==============================
file_excel <- file.path(folder_output, "Statistik_Zonal_Kecamatan_Sleman.xlsx")

write_xlsx(
  list(
    Ringkasan     = ringkasan,
    NDVI          = zonal_NDVI[order(zonal_NDVI$Kecamatan), ],
    NDBI          = zonal_NDBI[order(zonal_NDBI$Kecamatan), ],
    NDWI          = zonal_NDWI[order(zonal_NDWI$Kecamatan), ],
    NDCI_Awan     = zonal_NDCI[order(zonal_NDCI$Kecamatan), ],
    NDCI_Klorofil = zonal_NDCI_klorofil[order(zonal_NDCI_klorofil$Kecamatan), ],
    Awan          = zonal_awan[order(zonal_awan$Kecamatan), ]
  ),
  path = file_excel
)

if (file.exists(file_excel)) {
  cat("\nStatistik zonal berhasil disimpan!\nLokasi:", file_excel, "\n")
} else {
  stop("File Excel gagal disimpan.")
}





# ------------------------------------------------------------
# 16.EKSPOR UNTUK WEBGIS: GeoTIFF, GeoPackage, CSV
# ------------------------------------------------------------

# ============ A. FOLDER KELUARAN ==============================
folder_webgis <- file.path(folder_output, "WEBGIS")
dir.create(file.path(folder_webgis, "raster"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(folder_webgis, "vektor"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(folder_webgis, "tabel"),  recursive = TRUE, showWarnings = FALSE)

# ============ B. GEOTIFF (satu file per layer) ================
# Satu file per layer memudahkan checkbox layer di Leaflet/Shiny.
# Semua layer sudah terpotong ke 17 kecamatan.
mask_kec <- function(r) mask(crop(r, kec_plot), kec_plot)

layer_raster <- list(
  NDVI        = mask_kec(indeks_stack[["NDVI"]]),
  NDBI        = mask_kec(indeks_stack[["NDBI"]]),
  NDWI        = mask_kec(indeks_stack[["NDWI"]]),
  NDCI        = mask_kec(indeks_stack[["NDCI"]]),   # NDCI awan (kontinu)
  NDCI_awan   = mask_kec(peta_awan),   # 0 = bebas awan, 1 = awan/bayangan awan (SCL)
  Klasifikasi = kelas                  # 1 veg, 2 bangunan, 3 air, 4 awan, 0 lainnya
)

for (nm in names(layer_raster)) {
  r <- layer_raster[[nm]]
  names(r) <- nm
  
  tipe <- if (nm %in% c("NDCI_awan", "Klasifikasi")) "INT1U" else "FLT4S"
  
  writeRaster(
    r,
    file.path(folder_webgis, "raster", paste0(nm, "_Sleman_20m.tif")),
    overwrite = TRUE,
    datatype  = tipe,
    gdal      = c("COMPRESS=LZW")
  )
}

# ============ C. TABEL ATRIBUT LENGKAP ========================
# Gabungkan statistik zonal indeks + statistik awan per kecamatan
nama_kec <- as.data.frame(kec_plot)[[kolom_nama]]

tabel_lengkap <- data.frame(Kecamatan = nama_kec, stringsAsFactors = FALSE)
tabel_lengkap <- cbind(
  tabel_lengkap,
  ringkasan[match(nama_kec, ringkasan$Kecamatan),
            setdiff(names(ringkasan), c("Kecamatan", "Persen_Awan"))]
)
idx_awan <- match(nama_kec, zonal_awan$Kecamatan)
tabel_lengkap$Persen_Awan   <- zonal_awan$Persen_Awan[idx_awan]
tabel_lengkap$Luas_Awan_ha  <- zonal_awan$Luas_Awan_ha[idx_awan]
rownames(tabel_lengkap) <- NULL

# ============ D. GEOPACKAGE (poligon kecamatan + statistik) ===
# Disimpan dalam EPSG:4326 karena Leaflet bekerja pada koordinat lon/lat.
kec_gpkg <- project(kec_plot, "EPSG:4326")

# Buang atribut lama, ganti dengan tabel lengkap (urutan baris sama)
kec_gpkg <- kec_gpkg[, 0]
for (k in names(tabel_lengkap)) kec_gpkg[[k]] <- tabel_lengkap[[k]]

file_gpkg <- file.path(folder_webgis, "vektor", "Sleman_Kecamatan_Zonal.gpkg")

writeVector(
  kec_gpkg,
  file_gpkg,
  filetype  = "GPKG",
  layer     = "kecamatan_zonal",
  overwrite = TRUE
)

# ============ E. CSV ==========================================
# UTF-8 agar aman dibaca R/Shiny; write.csv memakai titik sebagai desimal.
write.csv(tabel_lengkap,
          file.path(folder_webgis, "tabel", "Statistik_Zonal_Kecamatan_Sleman.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

write.csv(zonal_awan,
          file.path(folder_webgis, "tabel", "Statistik_Awan_Kecamatan_Sleman.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# Luas tiap kelas komposit (dari bagian 12c)
write.csv(tab[, c("value", "kelas_nama", "count", "luas_ha", "persen")],
          file.path(folder_webgis, "tabel", "Luas_Kelas_Komposit_Sleman.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# ============ F. PEMERIKSAAN HASIL ============================
cat("\n--- FILE WEBGIS TERSIMPAN ---\n")
print(list.files(folder_webgis, recursive = TRUE))

cat("\n--- ISI GEOPACKAGE ---\n")
print(sf::st_layers(file_gpkg))

cat("\nBagian 16 selesai. Folder:", folder_webgis, "\n")






# ------------------------------------------------------------
# 17. INFORMASI AKHIR RASTER
# ------------------------------------------------------------

cat("\n=====================================\n")
cat("HASIL PENGOLAHAN SENTINEL-2 SLEMAN\n")
cat("=====================================\n")

cat("Jumlah band :", nlyr(sentinel_sleman), "\n")
cat("Resolusi    :", res(sentinel_sleman), "\n")
cat("Dimensi     :", dim(sentinel_sleman), "\n")
cat("Nama band   :", names(sentinel_sleman), "\n")
cat("CRS         :", crs(sentinel_sleman), "\n")

cat("\nPROSES SELESAI.\n")