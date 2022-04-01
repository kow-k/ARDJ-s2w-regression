# ARDJ survey2-web regression

NLP27/2021 での発表 P4-4 "Simulationg acceptability judgments using ARDJ data" (Kow Kuroda, Hikaru Yokono, Keiga Abe, Tomoyuki Tsuchiya, Yoshihkiko Asao, Yuichiro Kobayashi, Toshiyuki Kanamaru, and Takumi Tagawa) のデータと解析コード公開

# R scripts

[1] を実行し，その後に [2] を実行すると，論文中の Figure 5 が得られる (はず):

1. [run-logistic-regression.R](run-logistic-regression.R)
2. [plot-results-using-ggplot2.R](plot-results-using-ggplot2.R)

# data

上の R script [1] が読み込むデータ (= ARDJ Survey 2 web の応答データ):

1. [table-s2w-sd-filtered1.csv](table-s2w-sd-filtered1.csv)

以下は参考データ:

2. [resp-s2u-unfiltered.csv](resp-s2u-unfiltered.csv) [Survey 2 unified のハズレ値除去なし版]
3. [resp-s2u-sd-filtered1.csv](resp-s2u-sd-filtered1.csv) [Survey 2 unified のハズレ値除去版1]
4. [resp-s2u-sd-filtered1.csv](resp-s2u-sd-filtered2.csv) [Survey 2 unified のハズレ値除去版2]

# paper

[NLP27/2021 paper](https://www.dropbox.com/s/3k5kxfcrekbxgwv/ARDJ-s2w-regression-nlp27.pdf?dl=0)

## 論文の補助ファイル

[A/UNA と条件 An(r1) の組み合わせの成功表](glm-fittings-by-Ax.xlsx)

# poster

[NLP27/2021 poster](https://www.dropbox.com/s/yj7kzs4wn14cnzm/ARDJ-s2w-regression-nlp27-poster-r2.pdf?dl=0)
