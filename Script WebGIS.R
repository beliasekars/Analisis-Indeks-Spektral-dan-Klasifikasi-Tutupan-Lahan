# ============================================================
# WEBGIS KABUPATEN SLEMAN
# ============================================================

library(shiny)
library(leaflet)
library(sf)
library(terra)
library(raster)
library(DT)


# ------------------------------------------------------------
# 0. LOKASI FOLDER WEBGIS
# ------------------------------------------------------------
folder_webgis <- "C:/Users/BELIA SEKAR SARAS/Documents/TEKNIK GEOMATIKA/PEMROGRAMAN SPASIAL/ETS_PEMSPA/OUTPUT/WEBGIS"

if (!file.exists("vektor/Sleman_Kecamatan_Zonal.gpkg") && dir.exists(folder_webgis)) {
  setwd(folder_webgis)
}
if (!file.exists("vektor/Sleman_Kecamatan_Zonal.gpkg")) {
  stop("File 'vektor/Sleman_Kecamatan_Zonal.gpkg' tidak ditemukan.\n",
       "Folder kerja saat ini: ", getwd(), "\n",
       "Periksa nilai 'folder_webgis' di bagian 0, atau jalankan ulang ",
       "script pengolahan agar folder WEBGIS terbentuk.")
}

# ------------------------------------------------------------
# 1. DATA VEKTOR
# ------------------------------------------------------------
kec <- st_read("vektor/Sleman_Kecamatan_Zonal.gpkg",
               layer = "kecamatan_zonal", quiet = TRUE)
kec <- st_transform(kec, 4326)
kec <- st_make_valid(kec)
stopifnot("Kecamatan" %in% names(kec))

kec$Luas_ha  <- round(as.numeric(st_area(st_transform(kec, 32749))) / 10000, 2)
kec$Luas_km2 <- round(kec$Luas_ha / 100, 2)

bb <- as.numeric(st_bbox(kec))
kab <- st_sf(geometry = st_union(st_geometry(kec)))
kab_3857 <- terra::vect(st_transform(kab, 3857))

# ------------------------------------------------------------
# 2. DATA RASTER
# ------------------------------------------------------------
baca_terra <- function(file, metode, agregat = "mean") {
  lokasi <- file.path("raster", file)
  if (!file.exists(lokasi)) stop("Raster tidak ditemukan: ", lokasi)
  r <- terra::rast(lokasi)
  if (terra::nlyr(r) != 1) stop("Harus satu layer: ", file)
  if (!nzchar(terra::crs(r))) stop("Raster tidak punya CRS: ", file)
  terra::coltab(r) <- NULL          # buang tabel warna bawaan file
  r <- terra::project(r, "EPSG:3857", method = metode)
  r <- terra::crop(r, kab_3857)
  r <- terra::mask(r, kab_3857)
  # Kurangi resolusi tampilan bila terlalu besar (data asli tidak berubah)
  faktor <- max(1L, ceiling(max(terra::nrow(r), terra::ncol(r)) / 1800))
  if (faktor > 1) r <- terra::aggregate(r, fact = faktor, fun = agregat, na.rm = TRUE)
  r
}

ada_piksel <- function(r) {
  isTRUE(terra::global(!is.na(r), "sum", na.rm = TRUE)[1, 1] > 0)
}

# Baca Raster dan Cek
t_ndvi <- baca_terra("NDVI_Sleman_20m.tif", "bilinear")
t_ndbi <- baca_terra("NDBI_Sleman_20m.tif", "bilinear")
t_ndwi <- baca_terra("NDWI_Sleman_20m.tif", "bilinear")
t_ndci <- baca_terra("NDCI_Sleman_20m.tif", "bilinear")          # NDCI kontinu (awan)
t_klas <- baca_terra("Klasifikasi_Sleman_20m.tif", "near", "modal")

for (nm in c("t_ndvi", "t_ndbi", "t_ndwi", "t_ndci", "t_klas")) {
  if (!ada_piksel(get(nm))) {
    stop("Tidak ada piksel valid pada ", nm,
         ". Periksa extent dan CRS raster (terra::ext, terra::crs).")
  }
}

t_awan     <- terra::ifel(t_klas >= 4, t_klas, NA)   # kelas awan SCL 3, 8, 9, 10
valid_awan <- ada_piksel(t_awan)
t_klas4    <- terra::ifel(t_klas == 0, NA, t_klas)   # tanpa kelas "lainnya"

rast_layer <- list(
  NDVI    = raster::raster(t_ndvi),
  NDBI    = raster::raster(t_ndbi),
  NDWI    = raster::raster(t_ndwi),
  NDCI    = raster::raster(t_ndci),
  AWAN    = raster::raster(t_awan),
  KLAS_4  = raster::raster(t_klas4),
  KLAS_0  = raster::raster(t_klas)
)

# ------------------------------------------------------------
# 3. PENGATURAN KELAS, PALET, DAN PETA DASAR
# ------------------------------------------------------------
kelas_info <- data.frame(
  id    = 0:7,
  nama  = c("Tidak terklasifikasi", "Vegetasi", "Bangunan", "Perairan",
            "Bayangan awan (SCL 3)", "Awan sedang (SCL 8)",
            "Awan tinggi (SCL 9)",   "Cirrus tipis (SCL 10)"),
  warna = c("#D9D4C7", "#1A9850", "#D73027", "#3FA9F5",
            "#01082D", "#266CA9", "#0F2573", "#ABD9E9"),
  stringsAsFactors = FALSE
)

pengaturan <- list(
  NDVI = list(judul = "NDVI", pal = "Greens", dom = c(-1, 1)),
  NDBI = list(judul = "NDBI", pal = "Reds",   dom = c(-1, 1)),
  NDWI = list(judul = "NDWI", pal = "Blues",  dom = c(-1, 1)),
  NDCI = list(judul = "NDCI (Cloud Index)", pal = "Grays", dom = c(-1, 1))
)

semua_layer <- c("KLAS", "NDVI", "NDBI", "NDWI", "NDCI", "AWAN")

pilihan_dasar <- c("Peta terang" = "terang",
                   "OpenStreetMap" = "osm",
                   "Citra satelit" = "satelit")

tambah_dasar <- function(peta, kode) {
  prov <- switch(kode,
                 terang  = providers$CartoDB.Positron,
                 osm     = providers$OpenStreetMap.Mapnik,
                 satelit = providers$Esri.WorldImagery,
                 providers$CartoDB.Positron)
  addProviderTiles(peta, prov)
}

kolom_stat <- function(indeks) {
  paste0(indeks, "_", c("Mean", "Maks", "Min", "Median", "Std_Dev"))
}

fmt <- function(x, d = 3) {
  ifelse(is.na(x), "-", formatC(x, format = "f", digits = d, decimal.mark = ","))
}

sw <- function(css) span(class = "sw", style = paste0("background:", css, ";"))

# ------------------------------------------------------------
# 4. TAMPILAN (CSS)
# ------------------------------------------------------------
css <- "
@import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;700&family=DM+Mono:wght@400;500&display=swap');

html, body { background:#F1ECE2; }
body { font-family:'Arial','Segoe UI',system-ui,arial; color:#26302D; font-size:14px; }
body > .container-fluid { padding-left:0; padding-right:0; }

/* ---- bilah atas ---- */
.navbar { background:#45280A; border:0; border-radius:0; margin-bottom:0; min-height:52px; }
.navbar-default .navbar-brand { color:#E3CEA3; height:52px; padding:0 22px; display:flex;
  align-items:baseline; gap:10px; font-weight:700; letter-spacing:.01em; line-height:52px; }
.navbar-default .navbar-brand small { font-weight:400; font-size:12px; color:#B9C4BE; letter-spacing:0; }
.navbar-default .navbar-nav > li > a { color:#C9D3CD; padding:16px 18px; font-size:13px; font-weight:500;
  border-bottom:3px solid transparent; }
.navbar-default .navbar-nav > li > a:hover { color:#fff; background:transparent; }
.navbar-default .navbar-nav > .active > a,
.navbar-default .navbar-nav > .active > a:hover,
.navbar-default .navbar-nav > .active > a:focus { color:#fff; background:transparent; border-bottom-color:#D9A441; }
.navbar-default .navbar-toggle { border-color:#4A6159; }
.navbar-default .navbar-toggle .icon-bar { background:#C9D3CD; }

/* ---- peta dan panel mengambang ---- */
.map-wrap { position:relative; height:calc(100vh - 52px); }
.panel-f { position:absolute; z-index:1100; background:#FBF8F2; border:1px solid #CFC6B4;
  border-radius:3px; box-shadow:0 1px 3px rgba(22,48,43,.22); padding:14px 16px; }
.panel-kiri { top:14px; left:14px; width:290px; max-height:calc(100% - 28px); overflow-y:auto; }
.leaflet-top.leaflet-left { left:318px; }
.leaflet-container { font-family:'Arial','Segoe UI',system-ui,arial; background:#E8E3D8; }
.leaflet-bar a { border-radius:3px !important; }
.info.legend { background:#FBF8F2; border:1px solid #CFC6B4; border-radius:3px; box-shadow:none;
  font-size:12px; line-height:18px; padding:8px 10px; }
.info.legend i { opacity:1 !important; border:1px solid rgba(0,0,0,.25); }

.sec { font-size:10.5px; font-weight:700; letter-spacing:.09em; text-transform:uppercase;
  color:#7A7364; margin:16px 0 7px; }
.sec.first { margin-top:0; }
.hint { color:#7A7364; font-size:12px; line-height:1.4; margin:8px 0 0; }
.note { color:#7A7364; font-size:11px; line-height:1.45; margin:10px 0 0; }

.panel-f .form-group { margin-bottom:6px; }
.panel-f label.control-label { display:none; }
.panel-f .checkbox, .panel-f .radio { margin:5px 0; }
.panel-f .checkbox label, .panel-f .radio label { font-size:13px; }
.panel-f .checkbox label { padding-left:22px; }
.sw { display:inline-block; width:18px; height:10px; margin-right:8px; vertical-align:-1px;
  border:1px solid rgba(0,0,0,.28); }
.selectize-input { border-radius:3px; border-color:#CFC6B4; box-shadow:none; padding:7px 10px; background:#fff; }
.selectize-input.focus { border-color:#16302B; box-shadow:none; }
.selectize-dropdown { z-index:2000; border-color:#CFC6B4; border-radius:0 0 3px 3px; }
.selectize-dropdown .active { background:#EDE6D6; color:#16302B; }

.btn-ghost { background:transparent; border:1px solid #16302B; color:#16302B; border-radius:3px;
  font-size:12px; font-weight:500; padding:5px 12px; }
.btn-ghost:hover, .btn-ghost:focus { background:#16302B; color:#fff; }

.irs--shiny .irs-bar, .irs-bar { background:#B8472F; border-color:#B8472F; }
.irs--shiny .irs-handle > i:first-child { background:#B8472F; }
.irs--shiny .irs-single, .irs-single { background:#B8472F; }
.irs--shiny .irs-line, .irs-line { background:#DDD5C4; border-color:#DDD5C4; }

/* ---- statistik kecamatan (dipakai di pop up) ---- */
.kec-nama { font-size:19px; font-weight:700; margin:0 0 2px; color:#16302B; }
.kec-luas { font-size:12.5px; color:#5E5A4E; margin:0 0 10px; }
table.stat { width:100%; border-collapse:collapse; font-size:12px; }
table.stat th { font-weight:700; color:#615112; font-size:10.5px; letter-spacing:.05em;
  text-transform:uppercase; padding:4px 3px; border-bottom:1.5px solid #16302B; text-align:right; }
table.stat th:first-child { text-align:left; }
table.stat td, table.stat tbody th { padding:5px 3px; border-bottom:1px solid #E4DDCC; }
table.stat tbody th { text-align:left; color:#16302B; font-size:12px; letter-spacing:0; text-transform:none; }
table.stat td.n { font-family:'Arial',Helvetica,sans-serif; text-align:right; font-size:11.5px; }
.awan-box { margin-top:12px; padding:8px 10px; background:#EFE9DA; border-left:3px solid #7F7F7F; font-size:12.5px; }

/* ---- halaman tabel ---- */
.page { max-width:1280px; margin:0 auto; padding:24px 26px 40px; }
.page-title { font-size:22px; font-weight:700; color:#16302B; margin:0 0 4px; }
.page-sub { color:#5E5A4E; margin:0 0 14px; max-width:820px; line-height:1.5; }
table.dataTable.compact tbody td { font-size:12.5px; }
table.dataTable thead th { font-size:11.5px; color:#16302B; background:#EDE6D6; }
.dt-button { background:#FBF8F2 !important; border:1px solid #16302B !important; color:#16302B !important;
  border-radius:3px !important; font-size:12px !important; box-shadow:none !important; }

/* ---- popup statistik kecamatan ---- */
.leaflet-popup-content-wrapper { background:#FBF8F2; color:#26302D; border-radius:3px;
  box-shadow:0 1px 4px rgba(22,48,43,.3); }
.leaflet-popup-tip { background:#FBF8F2; }
.leaflet-popup-content { margin:14px 16px; font-family:'Arial','Segoe UI',system-ui,sans-serif; font-size:13px; }
.popup-kec .kec-nama { margin-top:0; }

/* ---- halaman Information ---- */
.info-card { background:#FBF8F2; border:1px solid #CFC6B4; border-radius:3px;
  padding:18px 22px; margin-bottom:18px; }
.info-card h3 { font-size:16px; font-weight:700; color:#16302B; margin:0 0 10px; }
.info-card h4 { font-size:13.5px; font-weight:700; color:#16302B; margin:16px 0 6px; }
.info-card p, .info-card li { line-height:1.6; max-width:900px; }
.info-card table.stat th, .info-card table.stat td { text-align:left; vertical-align:top;
  padding:7px 8px; font-size:13px; }

@media (max-width: 820px) {
  .navbar-default .navbar-brand small { display:none; }
  .panel-kiri { width:calc(100% - 28px); max-height:38%; }
  .leaflet-top.leaflet-left { left:10px; top:auto; bottom:14px; }
}
"

# ------------------------------------------------------------
# 5. ANTARMUKA (UI)
# ------------------------------------------------------------
ui <- navbarPage(
  title = div(HTML("Visible: WebGIS Kabupaten Sleman <small>untuk Analisis Spektral dan Klasifikasi Tutupan Lahan</small>")),
  id = "nav",
  windowTitle = "Visible: WebGIS Kabupaten Sleman untuk Analisis Spektral dan Klasifikasi Tutupan Lahan",
  collapsible = TRUE,
  header = tags$head(tags$style(HTML(css))),
  
  # ------------------ TAB PETA ------------------
  tabPanel(
    "Peta", value = "peta",
    div(class = "map-wrap",
        leafletOutput("peta", width = "100%", height = "100%"),
        
        # Panel kiri: pencarian dan seluruh pengaturan layer
        div(class = "panel-f panel-kiri",
            div(class = "sec first", "Cari kecamatan"),
            selectizeInput(
              "cari_kec", label = NULL,
              choices  = c("Ketik nama kecamatan" = "", sort(kec$Kecamatan)),
              selected = "", width = "100%",
              options  = list(placeholder = "Ketik nama kecamatan",
                              dropdownParent = "body")
            ),
            actionButton("reset", "Tampilkan seluruh Sleman",
                         class = "btn-ghost", width = "100%"),
            p(class = "hint",
              "Klik wilayah kecamatan pada peta, atau cari namanya, untuk menampilkan pop up statistik."),
            
            div(class = "sec", "Layer peta"),
            checkboxGroupInput(
              "layer_aktif", label = NULL,
              choiceNames = list(
                tagList(sw(paste0("linear-gradient(90deg,",
                                  paste(kelas_info$warna[2:8], collapse = ","), ")")),
                        "Peta komposit (klasifikasi)"),
                tagList(sw("linear-gradient(90deg,#e5f5e0,#00441b)"), "NDVI (vegetasi)"),
                tagList(sw("linear-gradient(90deg,#fee0d2,#99000d)"), "NDBI (bangunan)"),
                tagList(sw("linear-gradient(90deg,#deebf7,#08306b)"), "NDWI (perairan)"),
                tagList(sw("linear-gradient(90deg,#f2f2f2,#252525)"), "NDCI (Cloud Index)"),
                tagList(sw(paste0("linear-gradient(90deg,",
                                  paste(kelas_info$warna[5:8], collapse = ","), ")")),
                        "Awan (kelas SCL)")
              ),
              choiceValues = semua_layer,
              selected = "KLAS"
            ),
            checkboxInput("tampil_lain", "Tampilkan area tidak terklasifikasi",
                          value = FALSE),
            div(style = "display:flex; gap:6px; margin-top:4px;",
                actionButton("semua",  "Semua layer", class = "btn-ghost", width = "50%"),
                actionButton("kosong", "Kosongkan",   class = "btn-ghost", width = "50%")),
            
            div(class = "sec", "Transparansi layer"),
            sliderInput("opasitas", label = NULL, min = 0.2, max = 1,
                        value = 0.75, step = 0.05, ticks = FALSE, width = "100%"),
            
            div(class = "sec", "Peta dasar"),
            radioButtons("dasar", label = NULL, choices = pilihan_dasar,
                         selected = "terang"),
            
            div(class = "sec", "Batas dan nama"),
            checkboxGroupInput("tampil_vektor", label = NULL,
                               choices  = c("Batas kecamatan", "Nama kecamatan"),
                               selected = c("Batas kecamatan", "Nama kecamatan"))
        )
        # Statistik kecamatan tampil sebagai pop up di peta
    )
  ),
  
  # ------------------ TAB TABEL ------------------
  tabPanel(
    "Tabel statistik", value = "tabel",
    div(class = "page",
        h2(class = "page-title", "Statistik zonal per kecamatan"),
        p(class = "page-sub",
          "Klik satu baris untuk membuka kecamatan tersebut di peta."),
        div(style = "margin-bottom:12px;",
            downloadButton("unduh_csv", "Simpan seluruh tabel (CSV)", class = "btn-ghost")),
        DTOutput("tabel")
    )
  ),
  
  # ------------------ TAB INFORMATION ------------------
  tabPanel(
    "Information", value = "info",
    div(class = "page",
        h2(class = "page-title", "Information"),
        p(class = "page-sub",
          "Profil pembuat WebGIS dan gambaran lokasi peta, yaitu Kabupaten Sleman."),
        
        div(class = "info-card",
            h3("1. Pembuat"),
            tags$table(class = "stat",
                       tags$thead(tags$tr(tags$th("No"), tags$th("Nama"), tags$th("NRP"))),
                       tags$tbody(
                         tags$tr(tags$td("1"), tags$td("Anis Tika Utami"),       tags$td("5016241031")),
                         tags$tr(tags$td("2"), tags$td("Belia Sekar Saraswara"), tags$td("5016241033"))
                       ))
        ),
        
        div(class = "info-card",
            h3("2. Informasi Lokasi Peta"),
            p("Kabupaten Sleman berada di bagian utara Daerah Istimewa Yogyakarta, ",
              "dengan luas sekitar 574,8 km\u00B2 dan terbagi menjadi 17 kecamatan. ",
              "Letaknya kurang lebih pada 110\u00B015' - 110\u00B030' BT dan 7\u00B034' - 7\u00B047' LS. ",
              "Di sebelah utara berbatasan dengan Kabupaten Magelang dan Boyolali (Jawa Tengah), ",
              "di timur dengan Kabupaten Klaten, di selatan dengan Kabupaten Bantul dan Kota Yogyakarta, ",
              "serta di barat dengan Kabupaten Kulon Progo dan Magelang."),
            
            h4("Topografi"),
            p("Topografi Sleman membentang dari puncak Gunung Merapi di utara hingga dataran rendah ",
              "di selatan. Ketinggian wilayah berkisar dari sekitar 100 mdpl di bagian selatan ",
              "hingga sekitar 2.500 mdpl di lereng atas Merapi. Secara umum wilayahnya ",
              "dibagi menjadi tiga zona."),
            tags$table(class = "stat",
                       tags$thead(tags$tr(tags$th("Zona"), tags$th("Wilayah"), tags$th("Karakteristik"))),
                       tags$tbody(
                         tags$tr(tags$td(tags$b("Lereng dan kaki Gunung Merapi (utara)")),
                                 tags$td("Cangkringan, Pakem, Turi, dan sebagian Tempel serta Ngemplak"),
                                 tags$td("Bergelombang hingga curam, ketinggian tertinggi, curah hujan lebih besar, ",
                                         "tanah regosol dari material vulkanik. Sebagian wilayah masuk kawasan ",
                                         "Taman Nasional Gunung Merapi dan rawan erupsi serta lahar.")),
                         tags$tr(tags$td(tags$b("Dataran fluvio-vulkanik (tengah hingga selatan)")),
                                 tags$td("Sleman, Ngaglik, Mlati, Depok, Godean, Seyegan, Minggir, Moyudan, Kalasan, dan Berbah"),
                                 tags$td("Datar hingga landai, tersusun dari endapan material vulkanik Merapi yang ",
                                         "dibawa sungai. Lahan subur dengan sawah beririgasi, serta permukiman dan ",
                                         "kawasan perkotaan yang padat di sekitar Depok, Mlati, dan Ngaglik.")),
                         tags$tr(tags$td(tags$b("Perbukitan (timur dan selatan)")),
                                 tags$td("Prambanan, serta sebagian Berbah dan Gamping"),
                                 tags$td("Perbukitan bergelombang dengan batuan sedimen dan vulkanik tua, ",
                                         "tanah relatif lebih tipis, dan lahan lebih kering dibanding dataran."))
                       )),
            
            h4("Hidrologi dan iklim"),
            tags$ul(
              tags$li("Sungai-sungai berhulu di Merapi dan mengalir ke arah selatan, antara lain Kali Krasak, ",
                      "Bedog, Code, Gajahwong, Boyong, Kuning, Opak, dan Gendol. Kali Progo membatasi ",
                      "wilayah di sisi barat."),
              tags$li("Iklim tropis dengan musim hujan dan kemarau. Curah hujan cenderung lebih tinggi ",
                      "ke arah lereng Merapi.")
            ),
            
            h4("Indeks spektral"),
            tags$table(class = "stat",
                       tags$thead(tags$tr(tags$th("Indeks"), tags$th("Rumus"), tags$th("Keterangan"))),
                       tags$tbody(
                         tags$tr(tags$td(tags$b("NDVI")), tags$td("(B08 - B04) / (B08 + B04)"),
                                 tags$td("Kerapatan vegetasi. Piksel vegetasi: NDVI \u2265 0,3.")),
                         tags$tr(tags$td(tags$b("NDBI")), tags$td("(B11 - B08) / (B11 + B08)"),
                                 tags$td("Lahan terbangun. Piksel bangunan: NDBI > 0.")),
                         tags$tr(tags$td(tags$b("NDWI")), tags$td("(B03 - B08) / (B03 + B08)"),
                                 tags$td("Badan air. Piksel perairan: NDWI > 0.")),
                         tags$tr(tags$td(tags$b("NDCI (Cloud Index)")), tags$td("(B8A - B04) / (B8A + B04)"),
                                 tags$td("Indeks awan dari NIR sempit (865 nm) dan merah (665 nm), ",
                                         "dihitung tanpa cloud mask sehingga piksel berawan tetap ada.")),
                         tags$tr(tags$td(tags$b("Kelas awan SCL")), tags$td("SCL 3, 8, 9, 10"),
                                 tags$td("Bayangan awan, awan probabilitas sedang, awan probabilitas tinggi, ",
                                         "dan cirrus tipis. Piksel ini dibuang saat NDVI, NDBI, dan NDWI dihitung."))
                       ))
        )
    )
  )
)

# ------------------------------------------------------------
# 6. SERVER
# ------------------------------------------------------------
server <- function(input, output, session) {
  
  opasitas <- debounce(reactive(input$opasitas), 250)
  
  # Kecamatan terpilih (dari pencarian, klik peta, atau baris tabel)
  terpilih <- reactiveVal("")
  
  # Penanda: pemilihan berasal dari klik peta (pop up sudah dibuat di titik klik)
  st <- new.env()
  st$klik <- FALSE
  
  # ---------- Isi pop up statistik kecamatan ----------
  popup_kec <- function(nm) {
    d <- st_drop_geometry(kec[kec$Kecamatan == nm, ])
    
    baris <- function(indeks) {
      v <- unlist(d[1, kolom_stat(indeks)], use.names = FALSE)
      tags$tr(tags$th(indeks), lapply(v, function(x) tags$td(class = "n", fmt(x))))
    }
    
    as.character(
      div(class = "popup-kec",
          h3(class = "kec-nama", nm),
          p(class = "kec-luas",
            sprintf("Luas %s ha (%s km\u00B2)",
                    format(d$Luas_ha, big.mark = ".", decimal.mark = ",", nsmall = 2),
                    format(d$Luas_km2, big.mark = ".", decimal.mark = ",", nsmall = 2))),
          tags$table(class = "stat",
                     tags$thead(tags$tr(tags$th("Indeks"), tags$th("Mean"), tags$th("Maks"),
                                        tags$th("Min"), tags$th("Median"), tags$th("SD"))),
                     tags$tbody(baris("NDVI"), baris("NDBI"), baris("NDWI"), baris("NDCI"))
          ),
          div(class = "awan-box",
              tags$b("Awan (SCL): "),
              sprintf("%s%% wilayah (%s ha)", fmt(d$Persen_Awan, 2), fmt(d$Luas_Awan_ha, 2)))
      )
    )
  }
  
  tampil_popup <- function(nm, lng, lat) {
    leafletProxy("peta") %>%
      clearPopups() %>%
      addPopups(lng, lat, popup_kec(nm),
                options = popupOptions(minWidth = 330, maxWidth = 380,
                                       closeOnClick = FALSE,
                                       autoPanPadding = c(20, 20)))
  }
  
  pilih_kosong <- function() {
    terpilih("")
    updateSelectizeInput(session, "cari_kec", selected = "")
    leafletProxy("peta") %>%
      clearGroup("Sorot") %>%
      clearPopups() %>%
      flyToBounds(bb[1], bb[2], bb[3], bb[4])
  }
  
  observeEvent(input$cari_kec, {
    terpilih(input$cari_kec)
  }, ignoreInit = TRUE)
  
  observeEvent(input$peta_shape_click, {
    klik <- input$peta_shape_click
    id   <- klik$id
    if (!is.null(id) && id %in% kec$Kecamatan) {
      if (!identical(id, terpilih())) st$klik <- TRUE
      terpilih(id)
      updateSelectizeInput(session, "cari_kec", selected = id)
      tampil_popup(id, klik$lng, klik$lat)
    }
  })
  
  observeEvent(input$tabel_rows_selected, {
    i <- input$tabel_rows_selected
    if (length(i)) {
      nm <- tabel_data()$Kecamatan[i]
      terpilih(nm)
      updateSelectizeInput(session, "cari_kec", selected = nm)
      updateNavbarPage(session, "nav", selected = "peta")
    }
  })
  
  observeEvent(input$reset, pilih_kosong())
  
  observeEvent(input$semua, {
    updateCheckboxGroupInput(session, "layer_aktif", selected = semua_layer)
  })
  observeEvent(input$kosong, {
    updateCheckboxGroupInput(session, "layer_aktif", selected = character(0))
  })
  
  # ---------- Peta interaktif  ----------
  output$peta <- renderLeaflet({
    leaflet(options = leafletOptions(zoomSnap = 0.25)) %>%
      addMapPane("batas", zIndex = 450) %>%
      tambah_dasar("terang") %>%
      addPolygons(
        data = kec, layerId = ~Kecamatan, group = "Batas kecamatan",
        color = "#1B1B1B", weight = 1.2, opacity = 0.9,
        fill = TRUE, fillOpacity = 0,
        options = pathOptions(pane = "batas"),
        highlightOptions = highlightOptions(weight = 2.5, color = "#B8472F"),
        label = ~Kecamatan
      ) %>%
      addLabelOnlyMarkers(
        data = suppressWarnings(st_point_on_surface(kec)),
        group = "Nama kecamatan", label = ~Kecamatan,
        labelOptions = labelOptions(
          noHide = TRUE, direction = "center", textOnly = TRUE,
          style = list("font-size" = "10.5px", "font-weight" = "700",
                       "color" = "#1B1B1B",
                       "text-shadow" = "0 0 3px #fff, 0 0 3px #fff, 0 0 3px #fff"))
      ) %>%
      addScaleBar(position = "bottomright",
                  options = scaleBarOptions(imperial = FALSE)) %>%
      fitBounds(bb[1], bb[2], bb[3], bb[4])
  })
  
  # ---------- Ganti peta dasar ----------
  observeEvent(input$dasar, {
    leafletProxy("peta") %>%
      clearTiles() %>%
      tambah_dasar(input$dasar)
  }, ignoreInit = TRUE, priority = 10)
  
  # ---------- Tampil/sembunyi batas dan nama kecamatan ----------
  observeEvent(input$tampil_vektor, {
    p <- leafletProxy("peta")
    for (g in c("Batas kecamatan", "Nama kecamatan")) {
      p <- if (g %in% input$tampil_vektor) showGroup(p, g) else hideGroup(p, g)
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  # ---------- Layer raster ----------
  observe({
    aktif <- input$layer_aktif
    alpha <- opasitas()
    lain  <- isTRUE(input$tampil_lain)
    invisible(input$dasar)
    
    proxy <- leafletProxy("peta")
    proxy <- clearImages(proxy)
    for (id in c("leg_klas", "leg_NDVI", "leg_NDBI", "leg_NDWI",
                 "leg_NDCI", "leg_AWAN")) {
      proxy <- removeControl(proxy, id)
    }
    
    # 1) Peta komposit 
    if ("KLAS" %in% aktif) {
      ids  <- if (lain) 0:7 else 1:7
      info <- kelas_info[kelas_info$id %in% ids, ]
      pal_klas <- leaflet::colorFactor(palette = info$warna, domain = ids,
                                       na.color = "transparent")
      proxy <- proxy %>%
        addRasterImage(if (lain) rast_layer$KLAS_0 else rast_layer$KLAS_4,
                       colors = pal_klas, opacity = alpha, project = FALSE,
                       maxBytes = 30 * 1024 * 1024) %>%
        addLegend(position = "bottomright", colors = info$warna,
                  labels = info$nama, title = "Peta komposit",
                  opacity = 1, layerId = "leg_klas")
    }
    
    # 2) Indeks  NDVI, NDBI, NDWI, NDCI 
    for (k in c("NDVI", "NDBI", "NDWI", "NDCI")) {
      if (k %in% aktif) {
        p   <- pengaturan[[k]]
        pal <- leaflet::colorNumeric(palette = hcl.colors(50, p$pal, rev = TRUE),
                                     domain = p$dom, na.color = "transparent")
        proxy <- proxy %>%
          addRasterImage(rast_layer[[k]], colors = pal, opacity = alpha,
                         project = FALSE, maxBytes = 30 * 1024 * 1024) %>%
          addLegend(position = "bottomright", pal = pal,
                    values = seq(-1, 1, length.out = 30), title = p$judul,
                    opacity = 0.95, layerId = paste0("leg_", k))
      }
    }
    
    # 3) Klasifikasi awan
    if ("AWAN" %in% aktif) {
      if (valid_awan) {
        info_awan <- kelas_info[kelas_info$id >= 4, ]
        pal_awan  <- leaflet::colorFactor(palette = info_awan$warna,
                                          domain = info_awan$id,
                                          na.color = "transparent")
        proxy <- proxy %>%
          addRasterImage(rast_layer$AWAN, colors = pal_awan, opacity = alpha,
                         project = FALSE, maxBytes = 30 * 1024 * 1024) %>%
          addLegend(position = "bottomright", colors = info_awan$warna,
                    labels = info_awan$nama, title = "Awan - kelas SCL",
                    opacity = 1, layerId = "leg_AWAN")
      } else {
        showNotification("Tidak ada piksel awan pada wilayah Sleman di citra ini.",
                         type = "message", id = "notif_awan", duration = 5)
      }
    }
  })
  
  
  observeEvent(terpilih(), {
    proxy <- leafletProxy("peta") %>% clearGroup("Sorot")
    nm <- terpilih()
    if (nzchar(nm) && nm %in% kec$Kecamatan) {
      poly <- kec[kec$Kecamatan == nm, ]
      b <- as.numeric(st_bbox(poly))
      proxy %>%
        addPolygons(data = poly, group = "Sorot", color = "#B8472F",
                    weight = 3.5, fill = FALSE,
                    options = pathOptions(pane = "batas", interactive = FALSE)) %>%
        flyToBounds(b[1], b[2], b[3], b[4])
      
      # Dari pencarian/tabel: pop up di titik tengah kecamatan
      if (!st$klik) {
        pt <- suppressWarnings(st_coordinates(st_point_on_surface(poly)))
        tampil_popup(nm, pt[1, 1], pt[1, 2])
      }
    }
    st$klik <- FALSE
  }, ignoreInit = TRUE)
  
  # ---------- Tabel statistik semua kecamatan ----------
  tabel_data <- reactive({
    d <- st_drop_geometry(kec)
    kol <- c("Kecamatan", "Luas_ha", "Luas_km2",
             kolom_stat("NDVI"), kolom_stat("NDBI"),
             kolom_stat("NDWI"), kolom_stat("NDCI"),
             "Persen_Awan", "Luas_Awan_ha")
    d <- d[, intersect(kol, names(d))]
    d[order(d$Kecamatan), ]
  })
  
  output$tabel <- renderDT({
    d <- tabel_data()
    tampil <- d
    nm_tampil <- names(d)
    nm_tampil <- sub("^Luas_ha$", "Luas (ha)", nm_tampil)
    nm_tampil <- sub("^Luas_km2$", "Luas (km2)", nm_tampil)
    nm_tampil <- sub("^Persen_Awan$", "Awan (%)", nm_tampil)
    nm_tampil <- sub("^Luas_Awan_ha$", "Luas awan (ha)", nm_tampil)
    names(tampil) <- gsub("_", " ", nm_tampil)
    
    idx_stat <- grep("^(NDVI|NDBI|NDWI|NDCI) ", names(tampil), value = TRUE)
    idx_lain <- intersect(c("Luas (ha)", "Luas (km2)", "Awan (%)", "Luas awan (ha)"),
                          names(tampil))
    
    datatable(
      tampil,
      rownames   = FALSE,
      selection  = "single",
      class      = "compact stripe hover",
      extensions = "Buttons",
      options = list(
        pageLength = 17, scrollX = TRUE, dom = "Bfrtip",
        buttons = list(
          list(extend = "csv",   text = "Simpan CSV",
               filename = "Statistik_Zonal_Kecamatan_Sleman"),
          list(extend = "excel", text = "Simpan Excel",
               filename = "Statistik_Zonal_Kecamatan_Sleman")
        ),
        language = list(
          search = "Cari:", zeroRecords = "Data tidak ditemukan",
          info = "Menampilkan _START_ - _END_ dari _TOTAL_ kecamatan",
          infoEmpty = "Tidak ada data", infoFiltered = "(disaring dari _MAX_ kecamatan)",
          paginate = list(previous = "Sebelumnya", `next` = "Berikutnya")
        )
      )
    ) %>%
      formatRound(columns = idx_stat, digits = 3) %>%
      formatRound(columns = idx_lain, digits = 2)
  })
  
  output$unduh_csv <- downloadHandler(
    filename = function() "Statistik_Zonal_Kecamatan_Sleman.csv",
    content  = function(file) {
      write.csv(tabel_data(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)