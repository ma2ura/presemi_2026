# パッケージを導入
pacman::p_load(tidyverse) # pacmanを導入

# データを読み込む。
# ここでは、任意の作業フォルダのdataフォルダに以下のcsvファイルが入っていることを前提
df <- read_csv(
  "data/merged_forecast_both_financial.csv", # ファイルへのパス
  col_types = cols(日経会社コード = col_character()) # 先頭の0を残すために会社コードを文字列に
) |> # 変数名を変更
  rename(親会社利益 = "親会社株主に帰属する当期純利益（連結）／当期利益（単独）［累計］") |>
  mutate( # 決算年と決算月の変数を作成
    決算年 = as.integer(str_sub(決算期, 1, 4)), # "2025/03" → 2025
    決算月 = as.integer(str_sub(決算期, 6, 7)) # "2025/03" → 3
  )


df <- df |>
  arrange(日経会社コード, 決算期) |> # 会社ごとに、決算期の順に並べる
  mutate(
    .by = 日経会社コード,
    連続 = (決算年 - lag(決算年) == 1) & (決算月 == lag(決算月)), # 前の行が「ちょうど1年前」か
    期首資産 = if_else(連続, lag(資産合計), NA_real_), # 前期末の総資産
    前期利益 = if_else(連続, lag(親会社利益), NA_real_) # 前期の当期純利益
  )

# 分析に用いる変数を作成する。
# filter()関数で条件を満たすものを抽出
df <- df |>
  filter(
    決算月数 == 12, 期首資産 > 0, # 12か月決算で、期首総資産が正の行
    between(決算年, 2016, 2025) # 2016～2025年のデータ
  ) |>
  mutate( # 新変数を作成
    利益率 = 親会社利益 / 期首資産, # ① 赤字回避：利益 ÷ 期首総資産
    利益増減率 = (親会社利益 - 前期利益) / 期首資産, # ② 減益回避：利益の変化 ÷ 期首総資産
    予想誤差率 = (純利益 - 予想純利益) / 期首資産 # ③ 予想未達回避：（実績 − 予想）÷ 期首総資産
  )


# データの記述統計量をチェック
df |>
  summarise(
    行数 = n(),
    会社数 = n_distinct(日経会社コード),
    最初の年 = min(決算年),
    最後の年 = max(決算年),
    赤字の割合 = mean(利益率 < 0, na.rm = TRUE),
    減益の割合 = mean(利益増減率 < 0, na.rm = TRUE),
    予想未達の割合 = mean(予想誤差率 < 0, na.rm = TRUE)
  )


# Burgstahler and Dichev (1998)の追試のためのグラフ作成用関数hist_bd()を作成
# 階級幅 bw のヒストグラム。ゼロの直前（-bw 以上 0 未満）と直後（0 以上 bw 未満）に色をつける

# ----------- 関数の定義 ---------------------
hist_bd <- function(
  # 関数の引数を設定
  x, # ヒストグラムを作るデータ
  bw = 0.005, # ヒストグラムのビンの幅
  lim = 0.1, # グラフの表示範囲
  xlab = "" # グラフのタイトル
) {
  x <- x[!is.na(x) & abs(x) < lim] # 表示範囲（±lim）だけ使う
  tibble(bin = floor(x / bw)) |> # 何番目の階級に入るか
    count(bin) |> # ビンの数
    mutate( # 変数を作成
      mid = (bin + 0.5) * bw, # 階級の真ん中の値
      区分 = case_when( # 場合分け
        bin == 0 ~ "ゼロの直後",
        bin == -1 ~ "ゼロの直前",
        TRUE ~ "その他"
      )
    ) |>
    ggplot(aes(x = mid, y = n, fill = 区分)) +
    geom_col(width = bw * 0.95) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_fill_manual(
      values = c("ゼロの直後" = "#e3439e", "ゼロの直前" = "#879dde", "その他" = "grey75")
    ) +
    labs(x = xlab, y = "企業・年度数", fill = NULL)
}

#---- ここまで -----


## 利益率のヒストグラム
## binの幅はデフォルトの0.005
hist_bd(df$利益率, bw = 0.001, lim = 0.05, xlab = "当期純利益 ÷ 期首総資産")

## ② 減益回避：利益の増減率のヒストグラム
hist_bd(df$利益増減率, bw = 0.001, lim = 0.05, xlab = "当期純利益の増減 ÷ 期首総資産")

## ③ 予想未達回避：予想誤差率のヒストグラム {.smaller}
hist_bd(df$予想誤差率, bw = 0.001, lim = 0.05, xlab = "（実績純利益 − 予想純利益）÷ 期首総資産")
