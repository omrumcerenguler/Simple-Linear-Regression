# proje.R

# --- GEREKLİ KÜTÜPHANELER ---
library(DBI)
library(odbc)
library(dplyr)
library(ggplot2)
library(dotenv)  # .env dosyasındaki veritabanı bilgilerini almak için
load_dot_env(".env")

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

wos_hit     <- dbReadTable(con, "woshit")

# Gerekli diğer tabloları da oku
wos_author     <- dbReadTable(con, "WosAuthor")
cu_author_rid  <- dbReadTable(con, "CuAuthorRID")
cu_author      <- dbReadTable(con, "CuAuthor")
yoksis_birim   <- dbReadTable(con, "YoksisBirim")

# Makale-birim eşleşmesi: birim_bag tablosunu oluştur
birim_bag <- wos_author %>%
  left_join(cu_author_rid, by = c("researcherId" = "ResearcherID"), relationship = "many-to-many") %>%
  left_join(cu_author, by = c("CuAuthorID" = "ID"), relationship = "many-to-many") %>%
  left_join(yoksis_birim, by = c("YoksisId" = "YoksisId")) %>%
  select(HitId, YoksisId) %>%
  filter(!is.na(YoksisId))

# --- JOIN ve TEMİZLEME ---
df <- left_join(wos_hit, birim_bag, by = "HitId")
df_clean <- df %>% filter(!is.na(YoksisId))

# --- BİRİMLERE GÖRE YAYIN SAYISI ---
birim_sayilari <- df_clean %>%
  count(YoksisId, sort = TRUE)

# --- GÖRSELLEŞTİRME: EN ÇOK YAYIN YAPAN 10 BİRİM ---
top10_plot <- birim_sayilari %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(YoksisId, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "En Çok Yayın Yapan 10 Birim",
       x = "Birim", y = "Yayın Sayısı") +
  theme_minimal()

# --- GÖRSELİ GÖSTER ---
print(top10_plot)

ggsave("en_cok_yayin_yapan_10_birim.png", plot = top10_plot, width = 10, height = 6)

# --- YAZAR BAZINDA 2026 TAHMİNİ ---

# Yayın yılını al
df_clean$Yil <- as.integer(df_clean$SourcePublishYear)
# Yazar-Yıl bazında yayın sayılarını hesapla
yazar_yil_sayilari <- df_clean %>%
  filter(!is.na(Yil)) %>%
  count(AuthorCount, Yil)

# Yazar sayısı 1 olanlara göre yıllık ortalama makale sayısını hesapla
tek_yazar_trendi <- yazar_yil_sayilari %>%
  filter(AuthorCount == 1) %>%
  group_by(Yil) %>%
  summarise(ortalama_makale = mean(n)) %>%
  filter(!is.na(ortalama_makale))

# Basit doğrusal regresyon ile tahmin yap
model <- lm(ortalama_makale ~ Yil, data = tek_yazar_trendi)

# 2026 yılı için tahmin
tahmin_2026 <- predict(model, newdata = data.frame(Yil = 2026))

cat("2026 için tek yazarlı yayın sayısı ortalama tahmini:", round(tahmin_2026, 2), "\n")