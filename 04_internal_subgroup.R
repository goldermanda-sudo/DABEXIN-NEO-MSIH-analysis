# ==============================================================
# 模块 04：内部分组相关因素分析（Table 3-4）
# ==============================================================
# 7 个分组变量 × 多个结局 → 一张大综合表
# 分组变量：stage_group / residence_group / comorbid_group / stoma_group /
#           income_group / age_group / insurance_group
# 结局：5 类费用 + CHE + 4 个核心 HRQoL（EQ-5D 效用值、EQ-VAS、C30-QL、C30-PA）
# ==============================================================

subgroup_vars <- list(
  list(var = "stage_group",     label = "肿瘤分期 (I-II vs III-IV)"),
  list(var = "residence_group", label = "家庭常住地 (城镇 vs 农村)"),
  list(var = "comorbid_group",  label = "合并慢性病 (无 vs 有)"),
  list(var = "stoma_group",     label = "术后造口 (无 vs 有)"),
  list(var = "income_group",    label = "月均收入 (≤5000 vs >5000)"),
  list(var = "age_group",       label = "年龄 (≤60 vs >60)"),
  list(var = "insurance_group", label = "医保 (城镇职工 vs 其他)")
)

outcome_vars <- list(
  list(var = "direct_med_cost",     label = "直接医疗成本",       type = "continuous", digits = 0),
  list(var = "direct_non_med_cost", label = "直接非医疗成本",     type = "continuous", digits = 0),
  list(var = "indirect_cost",       label = "间接成本",           type = "continuous", digits = 0),
  list(var = "total_burden",        label = "总经济负担",         type = "continuous", digits = 0),
  list(var = "cost_oop_clean",      label = "自付费用",           type = "continuous", digits = 0),
  list(var = "is_che",              label = "CHE 发生",           type = "binary"),
  list(var = "eq5d_utility_mult8r", label = "EQ-5D 效用值",       type = "continuous", digits = 3),
  list(var = "eq5d_vas",            label = "EQ-VAS",             type = "continuous", digits = 0),
  list(var = "c30_QL",              label = "C30 整体健康/QoL",   type = "continuous", digits = 1),
  list(var = "c30_PA",              label = "C30 疼痛",           type = "continuous", digits = 1)
)

# 构造一张长表：每行 = (结局, 分组变量)，列 = 两个亚组的统计量 + P 值
rows <- list()
for (sg in subgroup_vars) {
  g <- df_main[[sg$var]]
  if (is.null(g)) next
  levs <- if (is.factor(g)) levels(g) else sort(unique(as.character(g[!is.na(g)])))
  if (length(levs) != 2) next   # 仅处理二分组

  # 段标题行
  rows[[length(rows) + 1]] <- data.frame(
    section = sg$label,
    指标 = "",
    亚组1 = "",
    亚组2 = "",
    P值 = "",
    stringsAsFactors = FALSE
  )

  for (oc in outcome_vars) {
    v <- oc$var
    if (!v %in% names(df_main)) next
    x1 <- df_main[[v]][g == levs[1]]
    x2 <- df_main[[v]][g == levs[2]]
    if (oc$type == "continuous") {
      s1 <- fmt_median_iqr(x1, oc$digits)
      s2 <- fmt_median_iqr(x2, oc$digits)
      p  <- compare_continuous(df_main[[v]], g)
    } else {
      s1 <- fmt_n_pct(x1, event = 1)
      s2 <- fmt_n_pct(x2, event = 1)
      p  <- compare_categorical(df_main[[v]], g)
    }
    rows[[length(rows) + 1]] <- data.frame(
      section = "",
      指标 = oc$label,
      亚组1 = s1,
      亚组2 = s2,
      P值 = fmt_p(p),
      stringsAsFactors = FALSE
    )
  }
  # 在每个分组变量结束后，记录两个亚组的列名（用于该段的列标题脚注）
  rows[[length(rows) + 1]] <- data.frame(
    section = "",
    指标 = sprintf("[%s 两组：'%s'  vs  '%s']", sg$label, levs[1], levs[2]),
    亚组1 = "", 亚组2 = "", P值 = "",
    stringsAsFactors = FALSE
  )
}

tab34 <- do.call(rbind, rows)

ft_tab34 <- flextable::flextable(tab34) |>
  flextable::set_header_labels(
    section = "分组变量",
    指标 = "结局指标",
    亚组1 = "亚组 1",
    亚组2 = "亚组 2",
    P值 = "P 值"
  ) |>
  flextable::bold(i = ~ section != "", j = 1, part = "body") |>
  flextable::bg(i = ~ section != "", bg = "#F2F2F2", part = "body") |>
  flextable::width(j = 1, width = 2.0) |>
  flextable::width(j = 2, width = 2.0) |>
  flextable::width(j = c(3, 4), width = 1.5) |>
  flextable::width(j = 5, width = 0.8) |>
  apply_table_theme(
    footnotes = c(
      "连续型结局以中位数（P25，P75）描述，两组比较采用 Mann-Whitney U 秩和检验。",
      "二分类结局（CHE）以 n/N（%）描述，两组比较采用卡方检验（理论频数<5 时改 Fisher 精确检验）。",
      "每个分组变量段下方方括号注释指明两个亚组的取值。"
    )
  )

write_table_to_docx(
  ft = ft_tab34,
  title = sprintf("表 3-4  主分析样本内部分组相关因素分析（综合表，N = %d）", nrow(df_main)),
  file_path = file.path(output_path, "Table3-4_内部分组相关因素.docx")
)

message("[模块04] Table 3-4 已输出。")
