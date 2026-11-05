# install.packages("pacman") # 最初の1回だけ
pacman::p_load(tidyverse, ggthemes, skimr)  # とにかく最初にtidyverseを読み込む

# データの読み込み

fin <- read_csv("data/ch04_financial_data.csv")
stock <- read_csv("data/ch05_stock_data.csv")

head(fin)

fin$ROA <- fin$OX / fin$OA
fin$ROA

hist(fin$ROA)

# tidyverse風に書き直してみる。
# mutate() 関数を使うと新しい変数を作ってくれます。
fin <- fin |>
  mutate(
    ROA = OX / OA # ROAを計算
)

bad <- fin |>
  left_join(stock, by = c("firm_ID", "year"))   # 企業番号と年度でつなぐ

nrow(bad)   # 結合前

stock_m <- stock |>
  arrange(firm_ID, month_ID) |>          # 会社ごとに、月の順に並べる
  group_by(firm_ID) |>                   # 会社ごとに計算する
  mutate(
    ret = (stock_price * adjustment_coefficient + DPS) / lag(stock_price) - 1) |>
  ungroup()

stock_m |> head()

stock_y <- stock_m |>
  filter(!is.na(ret)) |>
  group_by(firm_ID, year) |>
  summarise(
    n_month = n(),                                              # 何か月分あるか
    R       = prod(1 + ret) - 1,                                # 年次リターン
    MV      = last(stock_price) * last(shares_outstanding) / 1e6, # 年末の時価総額（百万円）
    .groups = "drop"
  )
stock_y |> count(firm_ID, year) |> filter(n > 1)   # 検算：1社1年1行になったか

df <- fin |>
  left_join(stock_y, by = c("firm_ID", "year"),
            relationship = "one-to-one")          # 1対1でなければエラーにする

nrow(fin); 
nrow(df)                                # 検算：行数は変わっていないか

head(df)

cor.test(df$ROA, df$R)   # ROAと年次リターンの相関係数を計算
