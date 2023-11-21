# 固定變數
set.seed(2023)

# z-score for 95% confidence interval 置信區間
z_score = 1.96

# 模擬次數：一百萬
trial_time = 1000000

run_trial <- function(kh, lx_vs_kh, hk, lx_vs_hk, sample_size) {
    # 計算采樣誤差，並乘以100，方便以百分比計算
    sd = z_score * 0.5 * sqrt(1/sample_size) * 100

    kh_data <- rnorm(trial_time, mean=kh, sd=sd)
    lx_vs_kh_data <- rnorm(trial_time, mean=lx_vs_kh, sd=sd)
    hk_data <- rnorm(trial_time, mean=hk, sd=sd)
    lx_vs_hk_data <- rnorm(trial_time, mean=lx_vs_hk, sd=sd)

    kh_vs_lx_result = kh_data - lx_vs_kh_data
    kh_win_rate = sum(kh_vs_lx_result > 0)/trial_time * 100
    print(sprintf("柯侯 vs 賴蕭 勝率：%.2f%%", kh_win_rate))

    hk_vs_lx_result = hk_data - lx_vs_hk_data
    hk_win_rate = sum(hk_vs_lx_result > 0)/trial_time * 100
    print(sprintf("侯柯 vs 賴蕭 勝率：%.2f%%", hk_win_rate))

    kh_vs_hk_result = (kh_data - hk_data)
    win_rate = sum(kh_vs_hk_result > 0)/trial_time * 100
    print(sprintf("柯侯 强於 侯柯 的概率：%.2f%%", win_rate))

    verdit = (kh_data - lx_vs_kh_data) - (hk_data - lx_vs_hk_data)
    verdit_rate = sum(verdit > 0)/trial_time * 100
    print(sprintf("面對賴蕭，柯侯 强於 侯柯 的概率：%.2f%%", verdit_rate))

    verdit = (kh_data - lx_vs_kh_data) - (hk_data - lx_vs_hk_data) - sd
    verdit_rate = sum(verdit > 0)/trial_time * 100
    print(sprintf("面對賴蕭，柯侯 强於 侯柯 的概率：%.2f%% 修正", verdit_rate))
    print("")
}

# 匯流（趨勢）
kh = 48.30
lx_vs_kh = 39.20
hk = 46.10
lx_vs_hk = 41.60
size = 2046

print("匯流（趨勢）")
run_trial(kh, lx_vs_kh, hk, lx_vs_hk, size)

# 聯合報
kh = 41.00
lx_vs_kh = 35.00
hk = 42.00
lx_vs_hk = 36.00
size = 1149

print("聯合報")
run_trial(kh, lx_vs_kh, hk, lx_vs_hk, size)

# 鏡電視（大地）
kh = 46.60
lx_vs_kh = 33.10
hk = 46.50
lx_vs_hk = 34.90
size = 1112

print("鏡電視（大地）")
run_trial(kh, lx_vs_kh, hk, lx_vs_hk, size)

# 競爭力（世新）
kh = 46.01
lx_vs_kh = 32.00
hk = 40.82
lx_vs_hk = 35.86
size = 1112

print("競爭力（世新）")
run_trial(kh, lx_vs_kh, hk, lx_vs_hk, size)

# TPP內參
kh = 44.00
lx_vs_kh = 32.00
hk = 39.70
lx_vs_hk = 33.00
size = 1082

print("TPP內參")
run_trial(kh, lx_vs_kh, hk, lx_vs_hk, size)

# KMT內參
kh = 38.80
lx_vs_kh = 29.30
hk = 38.20
lx_vs_hk = 30.60
size = 1484

print("KMT內參")
run_trial(kh, lx_vs_kh, hk, lx_vs_hk, size)
