# ==============================================================
# 模块 05：转移/复发亚组对比（Table 3-5）
# ==============================================================
# 自变量：recur_group（未转移/复发 vs 已转移/复发）
# 结局：5 类费用 + CHE + 4 个核心 HRQoL（与模块04一致）
# ==============================================================

outcomes <- list(
  list(var = "direct_med_cost",     label = "直接医疗成本（元）",     type = "continuous", digits = 0),
  list(var = "direct_non_med_cost", label = "直接非医疗成本（元）",   type = "continuous", digits = 0),
  list(var = "indirect_cost",       label = "间接成本（元）",         type = "continuous", digits = 0),
  list(var = "total_burden",        label = "总经济负担（元）",       type = "continuous", digits = 0),
  list(var = "cost_oop_clean",      label = "自付费用（元）",         type = "continuous", digits = 0),
  list(var = "is_che",              label = "CHE 发生率",             type = "binary"),
  list(var = "eq5d_utility_mult8r", label = "EQ-5D 效用值",           type = "continuous", digits = 3),
  list(var = "eq5d_vas",            label = "EQ-VAS",                 type = "continuous", digits = 0),
  list(var = "c30_QL",              label = "C30 整体健康/QoL",       type = "continuous", digits = 1),
  list(var = "c30_PF",              label = "C30 躯体功能",           type = "continuous", digits = 1),
  list(var = "c30_EF",              label = "C30 情绪功能",           type = "continuous", digits = 1),
  list(var = "c30_PA",              label = "C30 疼痛",               type = "continuous", digits = 1),
  list(var = "c30_FA",              label = "C30 疲乏",               type = "continuous", digits = 1)
)

g <- df_main$recur_group
n_a <- sum(g == "未转移/复发", na.rm = TRUE)
n_b <- sum(g == "已转移/复发", na.rm = TRUE)

rows <- list()
for (oc in outcomes) {
  v <- oc$var
  if (!v %in% names(df_main)) next
  x_a <- df_main[[v]][g == "未转移/复发"]
  x_b <- df_main[[v]][g == "已转移/复发"]
  if (oc$type == "continuous") {
    s_a <- fmt_median_iqr(x_a, oc$digits)
    s_b <- fmt_median_iqr(x_b, oc$digits)
    p   <- compare_continuous(df_main[[v]], g)
  } else {
    s_a <- fmt_n_pct(x_a, event = 1)
    s_b <- fmt_n_pct(x_b, event = 1)
    p   <- compare_categorical(df_main[[v]], g)
  }
  rows[[length(rows) + 1]] <- data.frame(
    指标 = oc$label, 未复发 = s_a, 已复发 = s_b, P值 = fmt_p(p),
    stringsAsFactors = FALSE
  )
}
tab35 <- do.call(rbind, rows)

ft_tab35 <- flextable::flextable(tab35) |>
  flextable::set_header_labels(
    指标 = "结局指标",
    未复发 = sprintf("未转移/复发 (N=%d)", n_a),
    已复发 = sprintf("已转移/复发 (N=%d)", n_b),
    P值 = "P 值"
  ) |>
  flextable::bold(j = 1, part = "body") |>
  flextable::width(j = 1, width = 2.4) |>
  flextable::width(j = c(2, 3), width = 1.7) |>
  flextable::width(j = 4, width = 0.8) |>
  apply_table_theme(
    footnotes = c(
      "连续变量以中位数（P25，P75）表示，比较使用 Mann-Whitney U 秩和检验。",
      "CHE 以 n/N（%）表示，比较使用卡方检验或 Fisher 精确检验。",
      "转移/复发亚组术后时间不受 3 个月限制。"
    )
  )

write_table_to_docx(
  ft = ft_tab35,
  title = sprintf("表 3-5  转移/复发亚组对比（N = %d）", nrow(df_main)),
  file_path = file.path(output_path, "Table3-5_转移复发亚组.docx")
)

message("[模块05] Table 3-5 已输出。")
