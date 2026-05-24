# ==============================================================
# 模块 08：探索性达伯欣方案个案系列（Table 3-8）
# ==============================================================
# 描述对象：df_dabexin（n≈2）
# 内容：逐例呈现基本信息 + 费用 + CHE + HRQoL
# 不做任何统计推断
# ==============================================================

if (nrow(df_dabexin) == 0) {
  message("[模块08] 未识别到符合纳入标准的达伯欣方案个案，跳过 Table 3-8。")
} else {

# 每个个案一列
case_ids <- if ("sys_id" %in% names(df_dabexin)) df_dabexin$sys_id else seq_len(nrow(df_dabexin))
case_cols <- paste0("个案 ", seq_along(case_ids), " (ID=", case_ids, ")")

items <- list(
  list(section = "基本信息", label = "性别",
       get = function(d) as.character(d$gender)),
  list(section = "",        label = "年龄（岁）",
       get = function(d) as.character(d$age)),
  list(section = "",        label = "家庭常住地",
       get = function(d) as.character(d$residence_group)),
  list(section = "",        label = "月均收入档",
       get = function(d) as.character(d$income_group)),
  list(section = "",        label = "医疗保险类型",
       get = function(d) as.character(d$insurance_group)),
  list(section = "肿瘤与治疗信息", label = "肿瘤分期",
       get = function(d) as.character(d$stage_group)),
  list(section = "",        label = "术后造口",
       get = function(d) as.character(d$stoma_group)),
  list(section = "",        label = "手术日期",
       get = function(d) as.character(d$surgery_date)),
  list(section = "",        label = "术后至调查（月）",
       get = function(d) sprintf("%.1f", d$months_postop)),
  list(section = "",        label = "术后是否转移/复发",
       get = function(d) as.character(d$recur_group)),
  list(section = "经济负担", label = "直接医疗成本（元）",
       get = function(d) formatC(d$direct_med_cost, big.mark = ",", format = "f", digits = 0)),
  list(section = "",        label = "直接非医疗成本（元）",
       get = function(d) formatC(d$direct_non_med_cost, big.mark = ",", format = "f", digits = 0)),
  list(section = "",        label = "间接成本（元）",
       get = function(d) formatC(d$indirect_cost, big.mark = ",", format = "f", digits = 0)),
  list(section = "",        label = "总经济负担（元）",
       get = function(d) formatC(d$total_burden, big.mark = ",", format = "f", digits = 0)),
  list(section = "",        label = "自付费用（元）",
       get = function(d) formatC(d$cost_oop_clean, big.mark = ",", format = "f", digits = 0)),
  list(section = "",        label = "CHE 状态（20%阈值）",
       get = function(d) ifelse(is.na(d$is_che), "—",
                                ifelse(d$is_che == 1, "发生", "未发生"))),
  list(section = "HRQoL", label = "EQ-5D 效用值（MULT8r）",
       get = function(d) sprintf("%.3f", d$eq5d_utility_mult8r)),
  list(section = "",        label = "EQ-VAS",
       get = function(d) sprintf("%.0f", safe_num(d$eq5d_vas))),
  list(section = "",        label = "C30 整体健康/QoL",
       get = function(d) sprintf("%.1f", d$c30_QL)),
  list(section = "",        label = "C30 躯体功能",
       get = function(d) sprintf("%.1f", d$c30_PF)),
  list(section = "",        label = "C30 情绪功能",
       get = function(d) sprintf("%.1f", d$c30_EF)),
  list(section = "",        label = "C30 疼痛",
       get = function(d) sprintf("%.1f", d$c30_PA)),
  list(section = "",        label = "C30 疲乏",
       get = function(d) sprintf("%.1f", d$c30_FA)),
  list(section = "",        label = "CR29 胃肠道症状",
       get = function(d) sprintf("%.1f", d$cr29_GI)),
  list(section = "",        label = "CR29 体重/健康担忧",
       get = function(d) sprintf("%.1f", d$cr29_ANX))
)

rows <- list()
for (it in items) {
  row <- data.frame(分组 = it$section, 指标 = it$label, stringsAsFactors = FALSE)
  for (i in seq_along(case_ids)) {
    val <- tryCatch(it$get(df_dabexin[i, ]), error = function(e) "—")
    row[[case_cols[i]]] <- if (length(val) == 0 || is.null(val) || is.na(val)) "—" else val
  }
  rows[[length(rows) + 1]] <- row
}
tab38 <- do.call(rbind, rows)

ft_tab38 <- flextable::flextable(tab38) |>
  flextable::bold(i = ~ 分组 != "", j = 1, part = "body") |>
  flextable::bg(i = ~ 分组 != "", bg = "#F2F2F2", part = "body") |>
  flextable::width(j = 1, width = 1.7) |>
  flextable::width(j = 2, width = 2.3) |>
  apply_table_theme(
    footnotes = c(
      "本表为探索性描述性报告，不进行统计学检验。",
      "费用单位为人民币元；C30 / CR29 各维度按官方手册线性转换为 0–100 分。",
      "本表数据为附录 A.6 节样本量估算的初步参考。"
    )
  )

write_table_to_docx(
  ft = ft_tab38,
  title = sprintf("表 3-8  探索性达伯欣方案个案系列（n = %d）", nrow(df_dabexin)),
  file_path = file.path(output_path, "Table3-8_达伯欣个案系列.docx")
)

message(sprintf("[模块08] Table 3-8 已输出（n=%d）。", nrow(df_dabexin)))
}
