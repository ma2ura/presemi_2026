# ==============================================================================
# プレゼミ 2026 第3回 実習コード（Positron用）
# テーマ: 「平均」の罠と記述統計／実証分析の武器としての単回帰分析
# 作成者: 松浦 総一（立命館大学 経営学部）
# 
# 【Positronでの実行方法】
# - 実行したい行にカーソルを置き、[Cmd + Enter]（Mac）または [Ctrl + Enter]（Windows）
# - 複数行を選択して [Cmd + Enter] / [Ctrl + Enter] でまとめて実行可能
# - 右上の「Data Explorer」や「Variables」ペイン、「Plots」ペインに注目！
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. 必要なパッケージの読み込み
# ------------------------------------------------------------------------------
# 初回のみインストールが必要な場合があります（未インストールの場合はコメントアウトを外して実行）
# install.packages(c("tidyverse", "readxl", "scales"))

library(tidyverse) # データ操作（dplyr）や可視化（ggplot2）を含む必須セット
library(scales)    # グラフの軸の数値をカンマ区切りにするパッケージ

# ------------------------------------------------------------------------------
# 1. データの読み込み
# ------------------------------------------------------------------------------
# 東証上場企業500社（2024年度決算）の財務データを読み込みます
# ファイルパス: data/03_sales_data.csv

data_path <- "data/03_sales_data.csv"

# ファイルが存在するか確認
if (!file.exists(data_path)) {
  # プロジェクトルートにない場合のカレントディレクトリ調整
  if (file.exists("03_sales_data.csv")) {
    data_path <- "03_sales_data.csv"
  } else if (file.exists("../data/03_sales_data.csv")) {
    data_path <- "../data/03_sales_data.csv"
  }
}

# CSVの読み込み（tidyverseの read_csv を使用）
df <- read_csv(data_path)

# 【Positronの神機能①：VariablesペインとData Explorer】
# 1. 画面右側（または左側）の「Variables」ペインを見てください。
#    `df` というオブジェクトが表示され、行数（500 obs.）と列数（7 variables）が確認できます。
# 2. `df` の横にある「表アイコン（Data Explorer）」をクリックしてみてください！
#    Excelのようにデータをスクロール・並び替え・フィルタリングして閲覧できます。
#    列ごとのヒストグラムや欠損値の有無もGUI上で一目瞭然です。

# データの基本構造を確認
glimpse(df)
head(df, 10)

# 【データの素性（変数定義）】
# - firm_id      : 企業識別コード（一意の番号）
# - firm_name    : 企業名
# - industry     : 東証業種分類（食料品、輸送用機器、卸売業など）
# - year         : 決算年度（2024）
# - sales        : 売上高（単位：百万円） ※10,000 = 100億円
# - total_assets : 総資産（単位：百万円）
# - net_income   : 当期純利益（単位：百万円）


# ------------------------------------------------------------------------------
# 2. 記述統計量の計算（平均値 vs 中央値）
# ------------------------------------------------------------------------------

# ① 売上高の平均値（Mean）
mean_sales <- mean(df$sales)
cat("売上高の平均値:", comma(round(mean_sales)), "百万円（約", round(mean_sales / 10000), "00億円）\n")

# ② 売上高の中央値（Median）
median_sales <- median(df$sales)
cat("売上高の中央値:", comma(round(median_sales)), "百万円（約", round(median_sales / 10000), "00億円）\n")

# ③ 倍率（平均値 ÷ 中央値）
ratio <- mean_sales / median_sales
cat("倍率（平均 ÷ 中央値）:", round(ratio, 2), "倍\n")

# ④ 五数要約（Min, Q1, Median, Mean, Q3, Max）
summary(df$sales)

# ⑤ 散らばりの指標（標準偏差と四分位範囲）
cat("標準偏差(SD)    :", comma(round(sd(df$sales))), "百万円\n")
cat("四分位範囲(IQR) :", comma(round(IQR(df$sales))), "百万円\n")

# ⑥ 平均値未満の企業の割合
under_mean_pct <- mean(df$sales < mean_sales) * 100
cat("平均値未満の企業割合:", round(under_mean_pct, 1), "%\n")

# ⑦ 売上高トップ5社を確認してみる
df %>%
  select(firm_name, industry, sales, net_income) %>%
  arrange(desc(sales)) %>%
  slice(1:5) %>%
  mutate(売上高_兆円 = round(sales / 1000000, 2),
         利益_億円   = round(net_income / 100, 1))


# ------------------------------------------------------------------------------
# 3. データの分布を可視化する（ヒストグラム & 箱ひげ図）
# ------------------------------------------------------------------------------
# 【Positronの神機能②：Plotsペイン】
# コードを実行すると、右下の「Plots」ペインに綺麗なグラフが表示されます。
# 左右の矢印ボタンで過去に描画したグラフの履歴を行き来できます。

# グラフ1: 生データのヒストグラム（右に歪んだロングテール分布）
p1 <- ggplot(df, aes(x = sales / 1000)) + # 単位：10億円
  geom_histogram(bins = 45, fill = "#4A90E2", color = "white", alpha = 0.8) +
  geom_vline(xintercept = median_sales / 1000, color = "#2ECC71", linewidth = 1.2, linetype = "dashed") +
  geom_vline(xintercept = mean_sales / 1000, color = "#E74C3C", linewidth = 1.2) +
  annotate("text", x = 1000, y = 250, 
           label = paste0("緑点線: 中央値 (", comma(round(median_sales/1000)), "0億円)\n赤実線: 平均値 (", comma(round(mean_sales/1000)), "0億円)"),
           hjust = 0, size = 4.5, fontface = "bold") +
  labs(title = "上場企業500社の売上高分布（生データ）",
       subtitle = "大部分の企業が左端に密集し、極少数の超巨大企業が右へ長く伸びる",
       x = "売上高（10億円単位）", y = "企業数（社）") +
  theme_minimal(base_size = 13)

print(p1)

# グラフ2: 箱ひげ図（Boxplot）による外れ値の確認
p2 <- ggplot(df, aes(y = sales / 1000)) +
  geom_boxplot(fill = "#E8F4F8", color = "#2C3E50", outlier.color = "#E74C3C", outlier.size = 2.5) +
  labs(title = "売上高の箱ひげ図",
       subtitle = "赤い点は外れ値（Outlier）として検出された巨大企業群",
       y = "売上高（10億円単位）") +
  theme_minimal(base_size = 13)

print(p2)

# グラフ3: 対数変換（Log Transformation）後のヒストグラム
# 歪んだデータを正規分布に近づける実証分析の標準テクニック！
p3 <- ggplot(df, aes(x = log(sales))) +
  geom_histogram(bins = 25, fill = "#9B59B6", color = "white", alpha = 0.8) +
  geom_vline(xintercept = mean(log(df$sales)), color = "#F39C12", linewidth = 1.2) +
  labs(title = "売上高の対数分布 log(sales)",
       subtitle = "対数をとることで、左右対称の美しい山型分布（対数正規分布）に変換される",
       x = "自然対数 log(売上高)", y = "企業数（社）") +
  theme_minimal(base_size = 13)

print(p3)


# ------------------------------------------------------------------------------
# 4. 単回帰分析（OLS: 最小二乗法）の実行
# ------------------------------------------------------------------------------
# リサーチ・クエスチョン:
# 「売上高（規模）が大きい企業ほど、当期純利益（稼ぐ力）も大きいのか？」
# モデル: net_income = β0 + β1 * sales + u

model1 <- lm(net_income ~ sales, data = df)

# 回帰分析のサマリーを表示
summary(model1)

# 【結果の読み解き】
# 1. Estimate（傾き β1）: 約 0.0451
#    → 売上高が100万円増えると、当期純利益は平均して約4.5万円増える（マージン約4.5%）
# 2. Pr(>|t|)（p値）: < 2e-16 (***)
#    → 0.1%水準で統計的に極めて有意（偶然関係があるように見えている確率はゼロに近い）
# 3. Multiple R-squared（決定係数）: 約 0.615
#    → 当期純利益のばらつきの約61.5%が、売上高（規模）だけで説明できている

# 散布図と回帰直線の描画
p4 <- ggplot(df, aes(x = sales / 100, y = net_income / 100)) + # 単位：億円
  geom_point(color = "#34495E", alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", color = "#E74C3C", se = TRUE, linewidth = 1.2) +
  labs(title = "単回帰直線: 当期純利益 ~ 売上高（全500社）",
       subtitle = "赤線: OLS回帰直線（傾き: 0.045, R^2: 0.615）",
       x = "売上高（億円）", y = "当期純利益（億円）") +
  theme_minimal(base_size = 13)

print(p4)


# ------------------------------------------------------------------------------
# 5. 外れ値（レバレッジ・ポイント）の罠と検証
# ------------------------------------------------------------------------------
# 散布図を見ると、右上の超巨大企業数社（外れ値）が直線を強く引っ張っているように見えます。
# 上位5社を除外したら、回帰直線はどう変化するでしょうか？

# 売上高トップ5社を除外したサブサンプル（495社）
df_no_top5 <- df %>%
  arrange(desc(sales)) %>%
  slice(6:n())

# 上位5社除外データでの回帰分析
model_no_top5 <- lm(net_income ~ sales, data = df_no_top5)
summary(model_no_top5)

# 2つのモデルの比較
cat("=========================================\n")
cat("【全500社】       傾き:", round(coef(model1)[2], 4), 
    "  決定係数 R2:", round(summary(model1)$r.squared, 4), "\n")
cat("【上位5社除外】   傾き:", round(coef(model_no_top5)[2], 4), 
    "  決定係数 R2:", round(summary(model_no_top5)$r.squared, 4), "\n")
cat("=========================================\n")
# → たった5社を除いただけで、傾きは 0.045 から 0.060（3割増）へ、
#    R2は 0.61 から 0.87 へ跳ね上がりました！
#    これが「外れ値が回帰直線を歪める（レバレッジ効果）」の実態です。


# ------------------------------------------------------------------------------
# 6. 実証分析の処方箋：対数回帰モデル（Log-Log Regression）
# ------------------------------------------------------------------------------
# 規模の歪みを解決するため、黒字企業（利益 > 0）について両対数回帰を実行します。
# モデル: log(net_income) = β0 + β1 * log(sales) + u

df_pos <- df %>%
  filter(net_income > 0 & sales > 0)

model_log <- lm(log(net_income) ~ log(sales), data = df_pos)
summary(model_log)

# 対数散布図と回帰直線
p5 <- ggplot(df_pos, aes(x = log(sales), y = log(net_income))) +
  geom_point(color = "#8E44AD", alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", color = "#27AE60", linewidth = 1.2) +
  labs(title = "両対数回帰モデル: log(当期純利益) ~ log(売上高)",
       subtitle = paste0("傾き β1 = ", round(coef(model_log)[2], 3), 
                         "（売上高が1%増えると、当期純利益は約", round(coef(model_log)[2], 2), "%増える）"),
       x = "log(売上高)", y = "log(当期純利益)") +
  theme_minimal(base_size = 13)

print(p5)


# ------------------------------------------------------------------------------
# 7. チャレンジ課題（自分でコードを書いてみよう！）
# ------------------------------------------------------------------------------
# 【課題1】
# 総資産（total_assets）の記述統計量を計算してみましょう。
# 売上高と同様に、平均値と中央値には大きな開きがありますか？
# ヒント: mean(df$total_assets), median(df$total_assets)

# ［ここにコードを書く］


# 【課題2】
# 「売上高（sales）」を「総資産（total_assets）」で回帰する単回帰分析を実行してください。
# モデル: sales = β0 + β1 * total_assets + u
# 傾き（β1）はいくらになりましたか？ その数値は何を意味しているでしょうか？
# （※第2回の「総資産回転率」を思い出してみましょう！）

# ［ここにコードを書く］


# ==============================================================================
# 実習終了！
# お疲れ様でした。作成したグラフや出力結果をじっくり観察してみてください。
# ==============================================================================
