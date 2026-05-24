# ==============================================================
# 模块 09：六项敏感性分析（补充表 S1–S6）
# ==============================================================

# ---- S1：CHE 阈值 -------------------------------------------
s1_rows <- list()
for (thr in c(0.10, 0.20, 0.30, 0.40)) {
  che <- ifelse(!is.na(df_main$annual_income_est) & df_main$annual_income_est > 0,
                as.integer(df_main$cost_oop_clean / df_main$annual_income_est > thr),
                NA_integer_)
  s1_rows[[length(s1_rows) + 1]] <- data.frame(
    `CHE 阈值` = paste0(thr * 100, "%"),
    `CHE 发生率 n/N (%)` = fmt_n_pct(che, event = 1),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}
s1 <- do.call(rbind, s1_rows)
ft_s1 <- flextable::flextable(s1) |>
  apply_table_theme(footnotes = "主分析采用 20% 阈值（与正文 Table 3-2 一致）。")

# ---- S2：日薪敏感性 -----------------------------------------
s2_rows <- list()
for (w in c(200, 320, 1047)) {
  tmp <- derive_cost(df_main, daily_wage = w)
  s2_rows[[length(s2_rows) + 1]] <- data.frame(
    `日薪假设` = paste0(w, " 元/天"),
    `间接成本 中位数 (P25, P75)` = fmt_median_iqr(tmp$indirect_cost, 0),
    `总经济负担 中位数 (P25, P75)` = fmt_median_iqr(tmp$total_burden, 0),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}
s2 <- do.call(rbind, s2_rows)
ft_s2 <- flextable::flextable(s2) |>
  apply_table_theme(footnotes = c(
    "主分析采用 320 元/天（与正文一致）。",
    "患者本人误工损失日薪仍按月收入档位估算，不随本表假设变动。"
  ))

# ---- S3：EQ-5D 算法 -----------------------------------------
s3 <- data.frame(
  `效用值算法` = c("MULT8r（主分析）", "ADD20r（敏感性）"),
  `中位数 (P25, P75)` = c(
    fmt_median_iqr(df_main$eq5d_utility_mult8r, 3),
    fmt_median_iqr(df_main$eq5d_utility_add20r, 3)
  ),
  check.names = FALSE, stringsAsFactors = FALSE
)
ft_s3 <- flextable::flextable(s3) |>
  apply_table_theme(footnotes = "两种算法均来源于 Luo 等（2017）中国 EQ-5D-5L 价值集；主分析采用 MULT8r。")

# ---- S4：极值截尾 -------------------------------------------
p5  <- quantile(df_main$total_burden, 0.05, na.rm = TRUE)
p95 <- quantile(df_main$total_burden, 0.95, na.rm = TRUE)
df_trim <- df_main |>
  dplyr::filter(total_burden >= p5 & total_burden <= p95)

s4_items <- list(
  c("direct_med_cost",     "直接医疗成本"),
  c("direct_non_med_cost", "直接非医疗成本"),
  c("indirect_cost",       "间接成本"),
  c("total_burden",        "总经济负担"),
  c("cost_oop_clean",      "自付费用")
)
s4_rows <- list()
for (it in s4_items) {
  s4_rows[[length(s4_rows) + 1]] <- data.frame(
    `指标` = it[2],
    `原始样本 中位数 (P25, P75)` = fmt_median_iqr(df_main[[it[1]]], 0),
    `截尾后 中位数 (P25, P75)`   = fmt_median_iqr(df_trim[[it[1]]], 0),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}
s4 <- do.call(rbind, s4_rows)
ft_s4 <- flextable::flextable(s4) |>
  apply_table_theme(footnotes = sprintf(
    "总经济负担按 P5–P95 截尾：[%.0f, %.0f] 元。", p5, p95
  ))

# ---- S5：完整案例 -------------------------------------------
need_vars <- c("age", "income_num", "stage_diag_num", "total_burden",
               "cost_oop_clean", "eq5d_utility_mult8r")
need_vars <- intersect(need_vars, names(df_main))
df_complete <- df_main[stats::complete.cases(df_main[, need_vars, drop = FALSE]), ]

s5 <- data.frame(
  `分析集` = c(sprintf("主分析样本 (N=%d)", nrow(df_main)),
               sprintf("完整案例样本 (N=%d)", nrow(df_complete))),
  `总经济负担 中位数 (P25, P75)` = c(fmt_median_iqr(df_main$total_burden, 0),
                                     fmt_median_iqr(df_complete$total_burden, 0)),
  `自付费用 中位数 (P25, P75)` = c(fmt_median_iqr(df_main$cost_oop_clean, 0),
                                   fmt_median_iqr(df_complete$cost_oop_clean, 0)),
  `EQ-5D 效用值 中位数 (P25, P75)` = c(fmt_median_iqr(df_main$eq5d_utility_mult8r, 3),
                                       fmt_median_iqr(df_complete$eq5d_utility_mult8r, 3)),
  check.names = FALSE, stringsAsFactors = FALSE
)
ft_s5 <- flextable::flextable(s5) |>
  apply_table_theme(footnotes = "完整案例样本：在年龄、月收入、肿瘤分期、总费用、自付费用、EQ-5D 效用值上均无缺失者。")

# ---- S6：扩展样本 -------------------------------------------
s6 <- data.frame(
  `分析集` = c(sprintf("主分析样本 (N=%d)", nrow(df_main)),
               sprintf("扩展样本 (N=%d)",   nrow(df_extend))),
  `总经济负担 中位数 (P25, P75)` = c(fmt_median_iqr(df_main$total_burden, 0),
                                     fmt_median_iqr(df_extend$total_burden, 0)),
  `自付费用 中位数 (P25, P75)` = c(fmt_median_iqr(df_main$cost_oop_clean, 0),
                                   fmt_median_iqr(df_extend$cost_oop_clean, 0)),
  `EQ-5D 效用值 中位数 (P25, P75)` = c(fmt_median_iqr(df_main$eq5d_utility_mult8r, 3),
                                       fmt_median_iqr(df_extend$eq5d_utility_mult8r, 3)),
  `CHE n/N (%)` = c(fmt_n_pct(df_main$is_che, event = 1),
                    fmt_n_pct(df_extend$is_che, event = 1)),
  check.names = FALSE, stringsAsFactors = FALSE
)
ft_s6 <- flextable::flextable(s6) |>
  apply_table_theme(footnotes = c(
    "扩展样本 = 主分析样本 + 通过质控但术后超 3 个月且未复发的样本。",
    "若主结论保持稳健，提示术后时间窗放宽不会实质性改变结果方向。"
  ))

# ---- 合并导出 -----------------------------------------------
doc <- officer::read_docx() |>
  officer::body_add_par("补充表 S1  CHE 判定阈值敏感性分析", style = "Normal") |>
  flextable::body_add_flextable(ft_s1) |>
  officer::body_add_break() |>
  officer::body_add_par("补充表 S2  家属陪护机会成本日薪敏感性分析", style = "Normal") |>
  flextable::body_add_flextable(ft_s2) |>
  officer::body_add_break() |>
  officer::body_add_par("补充表 S3  EQ-5D-5L 效用值算法敏感性分析", style = "Normal") |>
  flextable::body_add_flextable(ft_s3) |>
  officer::body_add_break() |>
  officer::body_add_par("补充表 S4  极值截尾敏感性分析", style = "Normal") |>
  flextable::body_add_flextable(ft_s4) |>
  officer::body_add_break() |>
  officer::body_add_par("补充表 S5  完整案例分析", style = "Normal") |>
  flextable::body_add_flextable(ft_s5) |>
  officer::body_add_break() |>
  officer::body_add_par("补充表 S6  扩展样本敏感性分析", style = "Normal") |>
  flextable::body_add_flextable(ft_s6)

print(doc, target = file.path(output_path, "Supplementary_S1-S6_敏感性分析.docx"))

message("[模块09] 6 张补充表已输出。")
