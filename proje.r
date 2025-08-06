#######################################################################
# Proje: Kümeleme ve Tahmin Analizi
# Amaç: Veritabanından veya örnek CSV dosyalarından alınan verilerle
#       yayın, birim ve alan bazında analiz ve 2026 yılı için tahminler
# Yazar: [Adınız]
# Açıklama: Her önemli kod bloğu öncesinde açıklayıcı yorumlar bulunur.
#######################################################################

# --- GEREKLİ KÜTÜPHANELERİ YÜKLE ---
library(DBI)
library(odbc)
library(dplyr)
library(ggplot2)
library(dotenv) # .env dosyasındaki veritabanı bilgilerini almak için

if (file.exists(".env")) {
    # .env dosyası bulunduysa gerçek veritabanı bağlantısı kuruluyor
    message("✅ .env dosyası bulundu. Gerçek veritabanı bağlantısı kuruluyor...")
    library(dotenv)
    dotenv::load_dot_env(".env")

    # .env dosyasındaki zorunlu değişkenlerin dolu olup olmadığını kontrol et
    required_vars <- c("DB_DRIVER", "DB_SERVER", "DB_DATABASE", "DB_UID", "DB_PWD", "DB_PORT")
    if (any(sapply(required_vars, function(v) Sys.getenv(v) == ""))) {
        stop("❌ .env dosyasındaki veritabanı bilgileri eksik!")
    }
    # DB bağlantı kodların burada (aşağıda)
} else {
    # .env dosyası bulunamadıysa örnek CSV verileri yükleniyor veya oluşturuluyor
    message("⚠️ .env dosyası bulunamadı. Örnek CSV verileri yükleniyor veya oluşturuluyor...")

    # Örnek veri dosyalarının isimlerini tanımla
    sample_files <- list(
        cuauthor = "sample_cuauthor.csv",
        woshit = "sample_woshit.csv",
        woshitattributes = "sample_woshitattributes.csv"
    )

    # Fonksiyon: Örnek veri oluştur ve kaydet
    create_sample_data <- function() {
        # Örnek cuauthor verisi
        cuauthor <- data.frame(
            ID = 1:5,
            Name = c("Author A", "Author B", "Author C", "Author D", "Author E"),
            YoksisId = as.character(c(101, 102, 103, NA, 105)),
            stringsAsFactors = FALSE
        )
        write.csv(cuauthor, sample_files$cuauthor, row.names = FALSE)

        # Örnek woshit verisi
        woshit <- data.frame(
            HitId = 1:10,
            SourcePublishYear = sample(2015:2023, 10, replace = TRUE),
            AuthorCount = sample(1:5, 10, replace = TRUE),
            stringsAsFactors = FALSE
        )
        write.csv(woshit, sample_files$woshit, row.names = FALSE)

        # Örnek woshitattributes verisi
        woshitattributes <- data.frame(
            HitId = rep(1:10, each = 2),
            Value = sample(c("Field A", "Field B", "Field C", NA), 20, replace = TRUE),
            stringsAsFactors = FALSE
        )
        write.csv(woshitattributes, sample_files$woshitattributes, row.names = FALSE)
    }

    # Dosyalar varsa yükle, yoksa oluştur
    for (file_key in names(sample_files)) {
        if (!file.exists(sample_files[[file_key]])) {
            create_sample_data()
            break  # oluşturduktan sonra döngüyü kır, çünkü hepsi oluşturuldu
        }
    }

    # CSV dosyalarını yükle
    cuauthor <- read.csv(sample_files$cuauthor, stringsAsFactors = FALSE)
    woshit <- read.csv(sample_files$woshit, stringsAsFactors = FALSE)
    woshitattributes <- read.csv(sample_files$woshitattributes, stringsAsFactors = FALSE)
    # Diğer işlemler için benzer şekilde devam edilir
}

# .env dosyasından bilgileri alarak veritabanına bağlan
con <- dbConnect(odbc(),
                 Driver   = Sys.getenv("DB_DRIVER"),
                 Server   = Sys.getenv("DB_SERVER"),
                 Database = Sys.getenv("DB_DATABASE"),
                 UID      = Sys.getenv("DB_UID"),
                 PWD      = Sys.getenv("DB_PWD"),
                 Port     = as.integer(Sys.getenv("DB_PORT")),
                 TrustServerCertificate = "yes")

# --- VERİYİ SQL'DEN ÇEK ---
# Veritabanından gerekli tabloları oku
wos_hit     <- dbReadTable(con, "woshit") # Yayın bilgileri
wos_author     <- dbReadTable(con, "WosAuthor") # Yazar-yayın eşleşmeleri
cu_author_rid  <- dbReadTable(con, "CuAuthorRID") # Yazar kimlik eşleştirmeleri
cu_author      <- dbReadTable(con, "CuAuthor") # Yazarlar
yoksis_birim   <- dbReadTable(con, "YoksisBirim") # Birim bilgileri

# Birim tablosunda YoksisId eksik olanları çıkar, karaktere çevir
yoksis_birim <- yoksis_birim %>%
  filter(!is.na(YoksisId)) %>%
  mutate(YoksisId = as.character(YoksisId))

# cu_author tablosunda YoksisId'yi karaktere çevir
cu_author <- cu_author %>%
  mutate(YoksisId = as.character(YoksisId))

# Makale-birim eşleşmesi: birim_bag tablosunu oluştur
# Amaç: Her yayının bağlı olduğu birimi bulmak
birim_bag <- wos_author %>%
  left_join(cu_author_rid, by = c("researcherId" = "ResearcherID"), relationship = "many-to-many") %>%
  left_join(cu_author, by = c("CuAuthorID" = "ID"), relationship = "many-to-many") %>%
  left_join(yoksis_birim, by = c("YoksisId" = "YoksisId")) %>%
  select(HitId, YoksisId, BirimAdi = Ad.y) %>%
  filter(!is.na(YoksisId))

# --- JOIN ve TEMİZLEME ---
# Yayınlar ile birimleri birleştir, eksik birim bilgisi olanları çıkar
df <- left_join(wos_hit, birim_bag, by = "HitId") %>%
  mutate(YoksisId = as.character(YoksisId))
df_clean <- df %>% filter(!is.na(YoksisId))

# --- BİRİMLERE GÖRE YAYIN SAYISI ---
# Her bir birimin toplam yayın sayısını hesapla
birim_sayilari <- df_clean %>%
  count(BirimAdi, sort = TRUE)

# --- GÖRSELLEŞTİRME: EN ÇOK YAYIN YAPAN 10 BİRİM ---
# En çok yayın yapan 10 birimi çubuk grafikle göster
top10_plot <- birim_sayilari %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(BirimAdi, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "En Çok Yayın Yapan 10 Birim",
       x = "Birim", y = "Yayın Sayısı") +
  theme_minimal()

# --- GÖRSELİ GÖSTER ---
print(top10_plot)
cat("--------------------------------------------------\n")

# --- GÖRSELİ KAYDET ---
ggsave("en_cok_yayin_yapan_10_birim.png", plot = top10_plot, width = 10, height = 6)

# --- YAZAR BAZINDA 2026 TAHMİNİ ---
# Amaç: Tek yazarlı yayınların yıllara göre trendini analiz edip 2026 için tahmin yapmak

# Yayın yılını yeni bir sütun olarak ekle
df_clean$Yil <- as.integer(df_clean$SourcePublishYear)

# Yazar-Yıl bazında yayın sayılarını hesapla
yazar_yil_sayilari <- df_clean %>%
  filter(!is.na(Yil)) %>%
  count(AuthorCount, Yil)

# Yazar sayısı 1 olanlar için yıllık ortalama makale sayısını hesapla
tek_yazar_trendi <- yazar_yil_sayilari %>%
  filter(AuthorCount == 1) %>%
  group_by(Yil) %>%
  summarise(ortalama_makale = mean(n)) %>%
  filter(!is.na(ortalama_makale))

# Basit doğrusal regresyon ile trendi modelle ve 2026 için tahmin yap
model <- lm(ortalama_makale ~ Yil, data = tek_yazar_trendi)
tahmin_2026 <- predict(model, newdata = data.frame(Yil = 2026))

cat("2026 için tek yazarlı yayın sayısı ortalama tahmini:", round(tahmin_2026, 2), "\n")
cat("--------------------------------------------------\n")

# --- WOS HIT ATTRIBUTES TABLOSUNDAN VERİ ÇEKME ---
# Amaç: Araştırma alanı (Value) sütununu kullanarak analiz yapmak
wos_attr <- dbReadTable(con,"WosHitAttributes")

# Her yayına (HitId) karşılık gelen ilk alan bilgisini al, NA'ları çıkar
df_alan <- wos_attr %>%
    filter(!is.na(Value)) %>%
    group_by(HitId) %>%
    summarise(Value = first(Value), .groups = "drop")

# Yıl bilgisi ile birleştir, eksik yıl veya alan olanları çıkar
df_alan_full <- df_clean %>%
    select(HitId, Yil) %>%
    left_join(df_alan, by = "HitId") %>%
    filter(!is.na(Yil) & !is.na(Value))

# Value sütununa göre en popüler 10 araştırma alanını bul
en_populer_alanlar <- df_alan_full %>%
    count(Value, sort = TRUE) %>%
    slice_max(n, n = 10) %>%
    pull(Value)

# --- 2026 YILI İÇİN ALAN BAZINDA TAHMİN ---
# En popüler alanlar için 2026 yılı tahminlerini yazdır
for (alan in en_populer_alanlar) {
    alan_trend <- df_alan_full %>% 
        filter(Value == alan) %>% # Belirli alanı filtrele
        count(Yil) # Yıl bazında yayın sayısını hesapla
    if (nrow(alan_trend) >= 2) { # Yeterli veri varsa
        # Basit doğrusal regresyon modeli oluştur
        model <- lm(n ~ Yil, data = alan_trend) 
        tahmin <- predict(model, newdata = data.frame(Yil = 2026)) # Tahmin yap
        cat(alan,  "alanı için 2026 yılı yayın sayısı tahmini:", round(tahmin, 2), "\n") # Tahmini yazdır
        cat("--------------------------------------------------\n")
    }
}

# --- YIL TRENDİ (ALAN BAZINDA) ---
# Amaç: Her alanın yıllara göre büyüme eğilimini (egim) hesaplamak

# Yıl ve alan bazında yayın sayılarını hesapla, eksik verileri çıkar
alan_yil_trend <- df_alan_full %>%
    count(Value, Yil) %>%
    filter(!is.na(Yil), !is.na(Value))

# Her alan için yıllık büyüme eğimini (egim) bul
alan_egim<- alan_yil_trend %>% 
    group_by(Value) %>% # Alan bazında gruplama
    filter(n() >= 2) %>% # En az 2 yıl verisi olan alanları al
    summarise(
        egim = coef(lm(n ~ Yil))[2], # Yılın katsayısı (büyüme hızı)
        .groups = "drop"
    )

# En hızlı büyüyen 10 alanı seç
en_hizli_buyuyen_alanlar <- alan_egim %>%
    filter(egim > 0) %>% # Sadece pozitif eğimli (büyüyen) alanlar
    arrange(desc(egim)) %>%
    slice_head(n = 10)

cat("En hızlı büyüyen 10 alan (eğim değerine göre):\n")    
cat("--------------------------------------------------\n")
print(en_hizli_buyuyen_alanlar)
cat("--------------------------------------------------\n")

# --- EN HIZLI BÜYÜYEN 10 ALANIN GÖRSELLEŞTİRİLMESİ ---
# En hızlı büyüyen 10 alanı çubuk grafikle göster
alan_grafik <- en_hizli_buyuyen_alanlar %>%
  ggplot(aes(x = reorder(Value, egim), y = egim)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "En Hızlı Büyüyen 10 Araştırma Alanı",
       x = "Araştırma Alanı", y = "Büyüme Eğimi") +
  theme_minimal()

print(alan_grafik)
cat("--------------------------------------------------\n")

# --- GÖRSELİ KAYDET ---
ggsave("en_hizli_buyuyen_10_alan.png", plot = alan_grafik, width = 10, height = 6)

 
# --- BİRİM BAZLI YAYIN TRENDİ ve 2026 TAHMİNİ ---
# Amaç: Her birimin yıllara göre büyüme eğilimini ve 2026 tahminini hesaplamak

# (Yayın yılını tekrar eklemeye gerek yok, yukarıda eklenmişti)

# Yıl ve birim bazında yayın sayılarını hesapla
birim_yil_trend <- df_clean %>%
  filter(!is.na(Yil), !is.na(BirimAdi)) %>%
  count(BirimAdi, Yil)

# Her birim için büyüme eğimini hesapla
birim_egim <- birim_yil_trend %>%
  group_by(BirimAdi) %>%
  filter(n() >= 2) %>%
  summarise(
    egim = coef(lm(n ~ Yil))[2],
    .groups = "drop"
  )

# En hızlı büyüyen 10 birimi seç
en_hizli_buyuyen_birimler <- birim_egim %>%
  filter(egim > 0) %>%
  arrange(desc(egim)) %>%
  slice_head(n = 10)

cat("En hızlı büyüyen 10 birim (eğim değerine göre):\n")
cat("--------------------------------------------------\n")
print(en_hizli_buyuyen_birimler)
cat("--------------------------------------------------\n")

# --- GÖRSELLEŞTİRME: EN HIZLI BÜYÜYEN 10 BİRİM ---
# En hızlı büyüyen 10 birimi çubuk grafikle göster
birim_grafik <- en_hizli_buyuyen_birimler %>%
  ggplot(aes(x = reorder(BirimAdi, egim), y = egim)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(title = "En Hızlı Büyüyen 10 Birim",
       x = "Birim", y = "Büyüme Eğimi") +
  theme_minimal()

print(birim_grafik)
cat("--------------------------------------------------\n")

# --- GÖRSELİ KAYDET ---
ggsave("en_hizli_buyuyen_10_birim.png", plot = birim_grafik, width = 10, height = 6)


# --- BİRİMLERE GÖRE EN ÇOK YAYIN YAPILAN ARAŞTIRMA ALANLARI ---
# Amaç: Her birimin en çok yayın yaptığı araştırma alanlarını bulmak ve görselleştirmek

# woshitattributes tablosundaki ResearchField (Value) sütunu kullanılacak
woshitattributes <- dbReadTable(con, "WosHitAttributes")
# colnames(woshitattributes) # (İsteğe bağlı: Sütun isimlerini görmek için)

# Her yayını birimiyle ve alanıyla eşleştir
birim_alan <- df_clean %>%
  inner_join(woshitattributes %>% select(HitId, Value), by = "HitId", relationship = "many-to-many") %>%
  filter(!is.na(Value), !is.na(BirimAdi))

# Her bir birimin en çok yayın yaptığı ilk 3 alanı bul
en_populer_birim_alanlari <- birim_alan %>%
  group_by(BirimAdi, Value) %>%
  summarise(YayinSayisi = n(), .groups = "drop") %>%
  group_by(BirimAdi) %>%
  slice_max(order_by = YayinSayisi, n = 3, with_ties = FALSE) %>%
  ungroup()

# En çok yayına sahip ilk 5 birimi seç
en_populer_birimler <- birim_alan %>%
  count(BirimAdi, sort = TRUE) %>%
  slice_head(n = 5) %>%
  pull(BirimAdi)

# Bu birimlere ait en popüler 3 alanı filtrele
en_populer_birim_alanlari_filtered <- en_populer_birim_alanlari %>%
  filter(BirimAdi %in% en_populer_birimler)

# --- GÖRSELLEŞTİRME: BİRİMLERE GÖRE EN ÇOK YAYIN YAPILAN ALANLAR ---
grafik_birim_alan <- ggplot(en_populer_birim_alanlari_filtered, aes(x = reorder(Value, YayinSayisi), y = YayinSayisi, fill = BirimAdi)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ BirimAdi, scales = "free_y", ncol = 1) +
  coord_flip() +
  labs(title = "İlk 5 Birime Göre En Çok Yayın Yapılan Araştırma Alanları",
       x = "Araştırma Alanı", y = "Yayın Sayısı") +
  theme_minimal() +
  theme(strip.text = element_text(size = 8),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size = 6),
        plot.title = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 10))
# --- GÖRSELİ GÖSTER ---
print(grafik_birim_alan)
cat("--------------------------------------------------\n")
# --- GÖRSELİ KAYDET ---
ggsave("birimlere_gore_yayin_alanlari.png", plot = grafik_birim_alan, width = 12, height = 8)

# --- BİRİM ve ALAN BAZINDA 2026 TAHMİNİ ---
# Amaç: En popüler araştırma alanları ve en hızlı büyüyen birimler için 2026 yılı yayın tahminlerini karşılaştırmak

# Alan bazında tahminleri oluştur
alan_tahmin_df <- data.frame()
for (alan in en_populer_alanlar) {
    alan_trend <- df_alan_full %>%
        filter(Value == alan) %>%
        count(Yil)
    if (nrow(alan_trend) >= 2) {
        model <- lm(n ~ Yil, data = alan_trend) 
        tahmin <- predict(model, newdata = data.frame(Yil = 2026))
        alan_tahmin_df <- rbind(alan_tahmin_df, data.frame(Kategori = alan, Tahmin = round(tahmin, 2), Tip= "Araştırma Alanı"))
    }
}
# Birim bazında tahminleri oluştur
birim_tahmin_df <- data.frame()
for (birim in en_hizli_buyuyen_birimler$BirimAdi) {
    birim_trend <- birim_yil_trend %>%
        filter(BirimAdi == birim)
    if (nrow(birim_trend) >= 2) {
        model <- lm(n ~ Yil, data = birim_trend) 
        tahmin <- predict(model, newdata = data.frame(Yil = 2026))
        birim_tahmin_df <- rbind(birim_tahmin_df, data.frame(Kategori = birim, Tahmin = round(tahmin, 2), Tip= "Birim"))
    }
}

# Tahminleri birleştir
tahmin_karsilastirma <- rbind(alan_tahmin_df, birim_tahmin_df)

# --- GÖRSELLEŞTİRME: 2026 Tahmin Karşılaştırması ---
# Not: 'Tahmin2026' sütunu olmadığı için mevcut 'Tahmin' sütunu kullanılacak şekilde güncellendi
tahmin_grafik <- ggplot(tahmin_karsilastirma, aes(x = reorder(Kategori, Tahmin), y = Tahmin, fill = Tip)) +
  geom_col(position = "dodge") +
  coord_flip() +
  facet_wrap(~ Tip, scales = "free_y") +
  labs(title = "2026 Tahmin Karşılaştırması: Araştırma Alanları ve Birimler",
       x = "Kategori", y = "Tahmini Yayın Sayısı (2026)") +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    strip.text = element_text(size = 10, face = "bold")
  )

print(tahmin_grafik)
cat("--------------------------------------------------\n")

# --- GÖRSELİ KAYDET ---
ggsave("tahmin_karsilastirma_2026.png", plot = tahmin_grafik, width = 12, height = 8)


# --- ALAN BAZLI MODEL KARŞILAŞTIRMA ---
# Amaç: En popüler alanlarda iki farklı model (doğrusal regresyon ve ortalama) ile 2026 tahminlerini karşılaştırmak

model_karsilastirma_df <- data.frame(stringsAsFactors = FALSE)

for (alan in en_populer_alanlar) {
    alan_trend <- df_alan_full %>%
        filter(Value == alan) %>%
        count(Yil) %>%
        filter(!is.na(Yil))
    
    if (nrow(alan_trend) >= 2) {
        # Doğrusal Regresyon ile tahmin
        model_lr <- lm(n ~ Yil, data = alan_trend)
        tahmin_lr <- predict(model_lr, newdata = data.frame(Yil = 2026))
        
        # Basit Ortalama ile tahmin
        tahmin_ortalama <- mean(alan_trend$n, na.rm = TRUE)
        
        # Sonuçları tabloya ekle
        model_karsilastirma_df <- rbind(
            model_karsilastirma_df,
            data.frame(Kategori = alan, Model = "Doğrusal Regresyon", Tahmin2026 = round(tahmin_lr, 2), stringsAsFactors = FALSE),
            data.frame(Kategori = alan, Model = "Basit Ortalama", Tahmin2026 = round(tahmin_ortalama, 2), stringsAsFactors = FALSE)
        )
    }
}
# Sütun sırasını ayarla
model_karsilastirma_df <- model_karsilastirma_df[, c("Kategori", "Model", "Tahmin2026")]

# Sonuç tablosunu göster
cat("📊 Model Karşılaştırma Sonuçları:\n")
cat("Her satır belirli bir araştırma alanı (Kategori) için kullanılan model türünü ve bu modelin 2026 yılı için tahmini yayın sayısını göstermektedir.\n")
cat("Model türleri:\n")
cat(" - Doğrusal Regresyon: Yıllara göre doğrusal eğilim kullanılarak yapılan tahmin\n")
cat(" - Basit Ortalama: Önceki yılların ortalaması alınarak yapılan tahmin\n")
cat("Tahmin2026 sütunu: İlgili modelin, 2026 yılı için öngördüğü yayın sayısı\n")
cat("--------------------------------------------------\n")
options(max.print = 10000)
print(model_karsilastirma_df)
cat("--------------------------------------------------\n")

# --- GÖRSELLEŞTİRME: Model Karşılaştırma Grafiği ---
model_karsilastirma_grafik <- ggplot(model_karsilastirma_df, aes(x = Model, y = Tahmin2026, fill = Model)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ Kategori, scales = "free_y") +
    labs(
      title = "2026 Tahmin Karşılaştırması (Alan Bazında)",
      x = "Model",
      y = "Tahmini Yayın Sayısı"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(model_karsilastirma_grafik)
cat("--------------------------------------------------\n")

# --- GÖRSELİ KAYDET ---
ggsave("model_karsilastirma_2026.png", plot = model_karsilastirma_grafik, width = 14, height = 10)