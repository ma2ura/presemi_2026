# install.packages("pacman") # 最初の1回だけ
pacman::p_load(tidyverse, ggthemes, skimr)  # とにかく最初にtidyverseを読み込む

# データの読み込み
df <- read_csv("data/forecast_combined2.csv")

df <- df |>
    select(-c(1,4)) |>
    rename(
        ticker = ...2,
        相対年度 = ...3,
        tickerN = "FINHISA'NKCODE",
        kessan = "FINHISA'ACC",
        months = "FINHISA'MACC",
        連単フラグ = "FINHISA'SCFLG",
        決算種別 = "FINHISA'HACC",    # 本決算10
        会計基準 = "FINHISA'SECFLG", # 単独0, 連結(日本) 1, SEC 2, IFRS 3
        実績予想 = "FINHISA'RPFLG", # 予想0、実績1
        発表日 = "FINHISA'PUBLISHDATE",
        予想売上高 = "FINHISA'CE003",
        予想営業利益 ="FINHISA'CE004", 
        予想経常利益 = "FINHISA'CE005",
        予想純利益 = "FINHISA'CE006",
        予想EPS = "FINHISA'CE007"
    )

df <- df |>
    distinct() |>
    arrange(ticker, kessan) |>
    filter(!is.na(相対年度))

# ------------------------------------------------------------
# 予想と実績を、同じ決算期で横に並べる
# ------------------------------------------------------------
# 検算：1社・1決算期・予想／実績の組み合わせが1行ずつか（0行なら重複なし）
df |> count(ticker, kessan, 実績予想) |> filter(n > 1)

# 予想の行だけ取り出す（列名は「予想〇〇」のまま）
yosou <- df |>
    filter(実績予想 == 0) |>
    select(ticker, tickerN, kessan,
           予想発表日 = 発表日, 予想連単 = 連単フラグ, 予想会計基準 = 会計基準,
           予想売上高, 予想営業利益, 予想経常利益, 予想純利益, 予想EPS)

# 実績の行だけ取り出す（列名から「予想」を外す）
jisseki <- df |>
    filter(実績予想 == 1) |>
    select(ticker, tickerN, kessan, months,
           実績発表日 = 発表日, 実績連単 = 連単フラグ, 実績会計基準 = 会計基準,
           売上高 = 予想売上高, 営業利益 = 予想営業利益, 経常利益 = 予想経常利益,
           純利益 = 予想純利益, EPS = 予想EPS)

# 企業と決算期をキーにして横につなぐ
df_wide <- jisseki |>
    full_join(yosou, by = c("ticker", "tickerN", "kessan"),
              relationship = "one-to-one") |>   # 1対1でなければエラーで止める
    mutate(
        決算年 = as.integer(str_sub(kessan, 1, 4)),  # "2001/03" → 2001
        決算月 = as.integer(str_sub(kessan, 6, 7)),  # "2001/03" → 3
        連単一致 = 予想連単 == 実績連単               # 予想と実績で連結・単独がそろっているか
    ) |>
    relocate(ticker, tickerN, kessan, 決算年, 決算月, months,
             予想売上高, 売上高, 予想営業利益, 営業利益,
             予想経常利益, 経常利益, 予想純利益, 純利益, 予想EPS, EPS) |>
    arrange(ticker, kessan)

# 検算：行数の確認
nrow(df)                                  # 元のデータ（予想＋実績）
nrow(yosou); nrow(jisseki)                # 予想の行数、実績の行数
nrow(df_wide)                             # 横に並べたあと
df_wide |> summarise(
    予想と実績の両方あり = sum(!is.na(実績発表日) & !is.na(予想発表日)),
    実績だけ = sum(!is.na(実績発表日) & is.na(予想発表日)),
    予想だけ = sum(is.na(実績発表日)),
    連単が不一致 = sum(!連単一致, na.rm = TRUE)
)

df_wide |> head(10)

# 分析には、予想と実績の両方があり、連結・単独がそろっている行を使う
df_both <- df_wide |>
    filter(!is.na(実績発表日), !is.na(予想発表日), 連単一致)
nrow(df_both)

write_csv(df_both, "data/forecast_both.csv")


# ------------------------------------------------------------
# 予想・実績データ(forecast_both)と財務データ(financial_combined)を結合する
# ------------------------------------------------------------
forecast <- read_csv("data/forecast_both.csv", col_types = cols(tickerN = col_character()))   # 先頭の0を残す
nrow(forecast)
fin <- read_csv("data/financial_combined.csv", col_types = cols(日経会社コード = col_character()))
nrow(fin)

# 財務データの前処理：ファイル間の重複と、決算期が空の行を除く
fin <- fin |>
    select(-source_file) |>
    distinct() |>
    filter(!is.na(決算期))

# 検算：1社・1決算期が1行ずつか（0行なら重複なし）
fin |> count(日経会社コード, 決算期) |> filter(n > 1)
forecast |> count(tickerN, kessan) |> filter(n > 1)

# 日経会社コードと決算期をキーにして、財務データに予想・実績データを付ける
df_merged <- fin |>
    rename(fin_ticker = ticker) |>   # 列名の衝突を避ける
    left_join(
        forecast,
        by = c("日経会社コード" = "tickerN", "決算期" = "kessan"),
        relationship = "one-to-one"
    )

# 検算：行数は fin と同じはず／予想・実績データがついた行数
nrow(fin); nrow(df_merged)
sum(!is.na(df_merged$予想発表日))

write_csv(df_merged, "data/merged_forecast_both_financial.csv")

# BD追試

df <- read_csv("data/merged_forecast_both_financial.csv", col_types = cols(日経会社コード = col_character()))   # 先頭の0を残す

df <- df |>
    mutate(

    ) |>
    relocate(決算年, 決算月, .after = 決算期)

    