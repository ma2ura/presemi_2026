# 想定しているファイル構造

# 任意のフォルダ/ <- Rファイル置き場
# ├── img/ <- 画像ファイル置き場
# └── data/ <- csvファイル置き場

# パッケージを導入
pacman::p_load(tidyverse, patchwork) # pacmanを導入
# データを読み込む。
df <- read_csv("data/merged_forecast_both_financial.csv")
# 変数名が長いものを変更
df <- df |>
  rename(
    親会社利益 = "親会社株主に帰属する当期純利益（連結）／当期利益（単独）［累計］"
  )

# ヒストグラムを作成する尺度を作る。ここでは、利益率、利益変化率、予測誤差率
df <- df |>
  arrange(日経会社コード) |>
  group_by(日経会社コード) |> # 企業ごとに
  mutate( # 新しい変数を作成
    利益率 = 親会社利益 / lag(資産合計),
    利益変化率 = (親会社利益 - lag(親会社利益)) / lag(資産合計),
    利益予測誤差率 = (純利益 - 予想純利益) / lag(資産合計),
    営業利益率 = 営業利益 / lag(資産合計),
    営業利益変化率 = (営業利益 - lag(営業利益)) / lag(資産合計),
    営業利益予測誤差率 = (営業利益 - 予想営業利益) / lag(資産合計),
    経常利益率 = 経常利益 / lag(資産合計),
    経常利益変化率 = (経常利益 - lag(経常利益)) / lag(資産合計),
    経常利益予測誤差率 = (経常利益 - 予想経常利益) / lag(資産合計),
    売上高変化率 = (売上高 - lag(売上高)) / lag(資産合計),
    売上高予測誤差率 = (売上高 - 予想売上高) / lag(資産合計)
  )


# 良い感じのヒストグラムを書くための独自関数hist_bd()を作成する。
hist_bd <- function(
  x,
  bw = 0.005,
  lim = 0.1,
  xlab = ""
) {
  x <- x[!is.na(x) & abs(x) < lim] # 欠損値を除き、表示範囲のデータだけを取り出す
  tibble(bin = floor(x / bw)) |> # 各データが何番目の階級（bin）に入るかを計算する
    count(bin) |> # 階級ごとの企業・年度数（n）を数える
    mutate(
      mid = (bin + 0.5) * bw, # グラフの横軸用に、各階級の真ん中の値を計算する
      区分 = case_when(
        bin == 0 ~ "ゼロの直後", # 0 以上 bw 未満
        bin == -1 ~ "ゼロの直前", # -bw 以上 0 未満
        TRUE ~ "その他" # それ以外の階級
      )
    ) |>
    ggplot(aes(x = mid, y = n, fill = 区分)) +
    geom_col(width = bw * 0.95) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_fill_manual(
      values = c("ゼロの直後" = "#e3439e", "ゼロの直前" = "#879dde", "その他" = "grey75")
    ) +
    labs(x = xlab, y = "企業・年度数", fill = NULL) +
    theme_minimal(base_family = "noto")
}

# ヒストグラムで問題としている目標利益の少し下の棒(青い棒)と少し上の棒(赤い棒)が、
# もし利益操作が行われていなければ、棒の高さがどこにあったのかを予測したものとの差
# を測るための関数 bd_test
# 中身を詳しく知りたい場合は、CopilotやChatGPTなどにこのコードを解説させてください。
bd_test <- function(x, bw = 0.005, lim = 0.1) {
  x <- x[!is.na(x) & abs(x) < lim] # すべての階級番号の度数を数える
  n <- table(
    factor(
      floor(x / bw),
      levels = floor(-lim / bw):(floor(lim / bw) - 1)
    )
  )
  N <- sum(n)
  p <- n / N

  # 指定した階級 b の標準化差分を計算する内部関数
  sdiff <- function(b) {
    i <- as.character(b)
    l <- as.character(b - 1) # 左隣の階級
    r <- as.character(b + 1) # 右隣の階級
    sd <- sqrt(N * p[i] * (1 - p[i]) + 0.25 * N * (p[l] + p[r]) * (1 - p[l] - p[r]))
    unname((n[i] - (n[l] + n[r]) / 2) / sd)
  }

  tibble(
    階級 = c("ゼロの直前", "ゼロの直後"),
    度数 = as.integer(n[c("-1", "0")]),
    期待度数 = c((n["-2"] + n["0"]) / 2, (n["-1"] + n["1"]) / 2),
    標準化差分 = c(sdiff(-1), sdiff(0))
  )
}

# ヒストグラムと標準化差分の計算

g1 <- hist_bd(df$利益率, xlab = "当期純利益 ÷ 期首総資産")
g2 <- hist_bd(df$利益変化率, xlab = "当期純利益 ÷ 期首総資産")
g3 <- hist_bd(df$利益予測誤差率, xlab = "当期純利益 ÷ 期首総資産")
g1 + g2 + g3

bd_test(df$利益率)

g1 <- hist_bd(df$売上高変化率)
g2 <- hist_bd(df$売上高予測誤差率)

# patchworkパッケージを使って、グラフを並べる
g1 + g2
