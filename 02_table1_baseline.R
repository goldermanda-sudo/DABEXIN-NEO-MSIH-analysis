# ==============================================================
# 模块 02：Table 1 —— 主分析样本基线特征
# ==============================================================
# 描述对象：df_main (n≈45)
# 内容：人口学 + 临床特征；不做组间比较（单臂研究无对照）
# ==============================================================

baseline_vars <- list(
  list(var = "age",             label = "年龄（岁）",         type = "continuous"),
  list(var = "gender",          label = "性别",               type = "categorical"),
  list(var = "residence_group", label = "家庭常住地",         type = "categorical"),
  list(var = "edu_level_ord",   label = "教育水平",           type = "categorical"),
  list(var = "marital_status",  label = "婚姻状况",           type = "categorical"),
  list(var = "occup_status",    label = "职业状态",           type = "categorical"),
  list(var = "insurance_group", label = "医疗保险类型",       type = "categorical"),
  list(var = "income_group",    label = "月均收入分组",       type = "categorical"),
  list(var = "stage_group",     label = "肿瘤分期",           type = "categorical"),
  list(var = "stoma_group",     label = "术后造口情况",       type = "categorical"),
  list(var = "comorbid_group",  label = "合并慢性病",         type = "categorical"),
  list(var = "recur_group",     label = "术后是否转移/复发",  type = "categorical"),
  list(var = "days_preop",      label = "术前住院天数（天）", type = "continuous"),
  list(var = "days_postop",     label = "术后住院天数（天）", type = "continuous"),
  list(var = "months_postop",   label = "术后调查时间间隔（月）", type = "continuous")
)

n_total <- nrow(df_main)

rows <- list()
for (item in baseline_vars) {
  v <- item$var
  if (!v %in% names(df_main)) next
  if (item$type == "continuous") {
    rows[[length(rows) + 1]] <- data.frame(
      变量 = item$label, 水平 = "",
      统计量 = fmt_median_iqr(df_main[[v]], 1),
      stringsAsFactors = FALSE
    )
  } else {
    x <- df_main[[v]]
    rows[[length(rows) + 1]] <- data.frame(
      变量 = item$label, 水平 = "", 统计量 = "",
      stringsAsFactors = FALSE
    )
    levs <- if (is.factor(x)) levels(x) else sort(unique(as.character(x[!is.na(x)])))
    for (lv in levs) {
      n_lv <- sum(as.character(x) == lv, na.rm = TRUE)
      pct  <- if (n_total > 0) 100 * n_lv / n_total else NA_real_
      rows[[length(rows) + 1]] <- data.frame(
        变量 = "", 水平 = paste0("  ", lv),
        统计量 = sprintf("%d (%.1f%%)", n_lv, pct),
        stringsAsFactors = FALSE
      )
    }
  }
}
tab1_data <- do.call(rbind, rows)

ft_table1 <- flextable::flextable(tab1_data) |>
  flextable::set_header_labels(
    变量 = "变量", 水平 = "水平",
    统计量 = sprintf("主分析样本 (N=%d)", n_total)
  ) |>
  flextable::bold(i = ~ 变量 != "", j = 1, part = "body") |>
  flextable::width(j = 1, width = 2.0) |>
  flextable::width(j = 2, width = 1.6) |>
  flextable::width(j = 3, width = 1.8) |>
  apply_table_theme(
    footnotes = c(
      "连续变量以中位数（P25，P75）描述；分类变量以频数 n（%）描述。",
      "本研究为单中心横断面单臂研究，本表不进行组间比较。"
    )
  )

write_table_to_docx(
  ft = ft_table1,
  title = sprintf("表 3-1  主分析样本基线特征（N = %d）", n_total),
  file_path = file.path(output_path, "Table3-1_基线特征.docx")
)

message("[模块02] Table 1 已输出。")
