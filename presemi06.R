pacman::p_load(tidyverse, readxl, ggthemes)

# NEEDS（FqReport）のxlsxは中身のファイル名が "sharedstrings.xml"（小文字）になっており、
# readxl では "Error: vector" で読めない。一時フォルダで "sharedStrings.xml" に直してから読む。
read_fq <- function(path, sheet = "Sheet1") {
  tmp_dir <- tempfile()
  unzip(path, exdir = tmp_dir)
  file.rename(file.path(tmp_dir, "xl/sharedstrings.xml"),
              file.path(tmp_dir, "xl/sharedStrings.xml"))
  fixed <- tempfile(fileext = ".xlsx")
  old_wd <- setwd(tmp_dir)
  on.exit(setwd(old_wd))
  zip(fixed, list.files(".", recursive = TRUE, all.files = TRUE), flags = "-q -X -nw")
  setwd(old_wd)

  # 1〜8行目は見出し（7行目が項目名、8行目がコード）。データは9行目から。
  read_excel(fixed, sheet = sheet, skip = 8, col_names = FALSE,
             na = c("", "-"),
             col_types = c(rep("text", 8), rep("numeric", 9), "text"),
             .name_repair = "unique_quiet") |>
    set_names(c("company", "ticker", "blank", "period",
                "nkcode", "industrycode", "tse_flag", "listed_flag", "months",
                "total_assets", "ni_parent", "working_capital", "ni",
                "effective_tax_rate", "tax_burden_rate", "sales", "ni_forecast",
                "fiscal_year")) |>
    select(-blank)
}

# Excelファイルを読み込む
df_excel_01 <- read_fq("data/FqReport_01.xlsx")
df_excel_02 <- read_fq("data/FqReport_02.xlsx")
df_excel_03 <- read_fq("data/FqReport_03.xlsx")
df_excel_04 <- read_fq("data/FqReport_04.xlsx")

# 4ファイルを縦に結合（P〜P-11の12期分）
# period が空の行は、その期のデータがない企業（新規上場など）の空行なので除く
df_fq <- bind_rows(df_excel_01, df_excel_02, df_excel_03, df_excel_04) |>
  filter(!is.na(period)) |>
  arrange(ticker, fiscal_year)

glimpse(df_fq)

# write_csv(df_fq, "data/BD1998.csv", na = "")

df <- read_csv("data/BD1998.csv", na = "")

# Burgstahler and Dichev (1997) の追試を行う
# 変数を作成する

df <- df |>
    filter(!is.na(ni) & !is.na(total_assets) & !is.na(ni_forecast)) |>
    filter(total_assets > 0) |>
    mutate(
        .by = ticker,
        ROA = ni / lag(total_assets, n = 1),
        Delta_ROA = ROA - lag(ROA, n = 1),
        Forecast_error = (ni - ni_forecast) / lag(total_assets, n = 1),
        wac = (working_capital - lag(working_capital, n = 1)) / lag(total_assets, n = 1)
    )


# ヒストグラム

df |>
    ggplot(aes(x = ROA)) +
    geom_histogram(
        binwidth = 0.005, boundary = 0,
        fill = "lightblue", color = "black"
    ) +
    # limits = seq(-0.08, 0.08) は -0.08 の1つだけになりエラー。範囲は c(下限, 上限) で指定する
    coord_cartesian(xlim = c(-0.08, 0.08)) +
    labs(
        title = "ROA Distribution",
        x = "ROA",
        y = "Frequency"
    ) +
    theme_economist()
