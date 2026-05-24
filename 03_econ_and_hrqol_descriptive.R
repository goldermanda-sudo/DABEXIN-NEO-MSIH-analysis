# ==============================================================
# 模块 03：经济负担现状（Table 3-2）+ HRQoL 现状（Table 3-3）
# ==============================================================
# 描述对象：df_main (n≈45)
# 内容：单组描述，给出中位数（P25，P75）或 n/N (%)
# ==============================================================

# ----------------- Table 3-2 经济负担 -------------------------
cost_items <- list(
  list(var = "direct_med_cost",     label = "直接医疗成本（元）",       type = "continuous", digits = 0),
  list(var = "cost_oop_clean",      label = "其中：患者自付费用（元）", type = "continuous", digits = 0),
  list(var = "direct_non_med_cost", label = "直接非医疗成本（元）",     type = "continuous", digits = 0),
  list(var = "indirect_cost",       label = "间接成本（元）",           type = "continuous", digits = 0),
  list(var = "total_burden",        label = "总经济负担（元）",         type = "continuous", digits = 0),
  list(var = "is_che",              label = "灾难性卫生支出（CHE，20%阈值）", type = "binary",   digits = 1)
)

rows <- list()
for (it in cost_items) {
  v <- it$var
  if (!v %in% names(df_main)) next
  if (it$type == "continuous") {
    val <- fmt_median_iqr(df_main[[v]], it$digits)
  } else {
    val <- fmt_n_pct(df_main[[v]], event = 1)
  }
  rows[[length(rows) + 1]] <- data.frame(
    指标 = it$label, 主分析样本 = val, stringsAsFactors = FALSE
  )
}
tab32 <- do.call(rbind, rows)

ft_tab32 <- flextable::flextable(tab32) |>
  flextable::set_header_labels(
    指标 = "经济负担指标",
    主分析样本 = sprintf("主分析样本 (N=%d)", nrow(df_main))
  ) |>
  flextable::bold(j = 1, part = "body") |>
  flextable::width(j = 1, width = 3.0) |>
  flextable::width(j = 2, width = 2.5) |>
  apply_table_theme(
    footnotes = c(
      "连续型指标以中位数（P25，P75）描述；CHE 以 n/N（%）描述。",
      "费用按社会视角测算；间接成本采用人力资本法。",
      "CHE 定义：患者自付费用 / 估算家庭年收入 > 20%。"
    )
  )
write_table_to_docx(
  ft = ft_tab32,
  title = sprintf("表 3-2  主分析样本疾病经济负担现状（N = %d）", nrow(df_main)),
  file_path = file.path(output_path, "Table3-2_经济负担现状.docx")
)

# ----------------- Table 3-3 HRQoL ---------------------------
hrqol_items <- list(
  list(section = "EQ-5D-5L",                          var = "eq5d_utility_mult8r", label = "健康效用值（MULT8r）"),
  list(section = "",                                  var = "eq5d_vas",            label = "EQ-VAS（0–100）"),
  list(section = "QLQ-C30 功能域（高分=好）",        var = "c30_PF",              label = "躯体功能"),
  list(section = "",                                  var = "c30_RF",              label = "角色功能"),
  list(section = "",                                  var = "c30_EF",              label = "情绪功能"),
  list(section = "",                                  var = "c30_CF",              label = "认知功能"),
  list(section = "",                                  var = "c30_SF",              label = "社会功能"),
  list(section = "",                                  var = "c30_QL",              label = "整体健康/QoL"),
  list(section = "QLQ-C30 症状域（高分=症状重）",    var = "c30_FA",              label = "疲乏"),
  list(section = "",                                  var = "c30_NV",              label = "恶心呕吐"),
  list(section = "",                                  var = "c30_PA",              label = "疼痛"),
  list(section = "",                                  var = "c30_DY",              label = "气促"),
  list(section = "",                                  var = "c30_SL",              label = "失眠"),
  list(section = "",                                  var = "c30_AP",              label = "食欲丧失"),
  list(section = "",                                  var = "c30_CO",              label = "便秘"),
  list(section = "",                                  var = "c30_DI",              label = "腹泻"),
  list(section = "",                                  var = "c30_FI",              label = "经济困难"),
  list(section = "QLQ-CR29 共有维度",                 var = "cr29_UI",             label = "排尿问题"),
  list(section = "",                                  var = "cr29_GI",             label = "胃肠道症状"),
  list(section = "",                                  var = "cr29_ChSE",           label = "化疗副作用"),
  list(section = "",                                  var = "cr29_ANX",            label = "体重/健康担忧"),
  list(section = "",                                  var = "cr29_BLD",            label = "便血"),
  list(section = "",                                  var = "cr29_MUC",            label = "粘液便"),
  list(section = "",                                  var = "cr29_AP_pain",        label = "肛周疼痛"),
  list(section = "",                                  var = "cr29_BI",             label = "体像"),
  list(section = "QLQ-CR29 分支维度",                 var = "cr29_STO",            label = "造口相关问题（仅造口者）"),
  list(section = "",                                  var = "cr29_DEF",            label = "排便功能问题（仅无造口者）"),
  list(section = "",                                  var = "cr29_SxI_M",          label = "性功能（男性）"),
  list(section = "",                                  var = "cr29_SxI_F",          label = "性功能（女性）")
)

rows <- list()
for (it in hrqol_items) {
  v <- it$var
  if (!v %in% names(df_main)) next
  rows[[length(rows) + 1]] <- data.frame(
    分组 = it$section, 维度 = it$label,
    统计量 = fmt_median_iqr(df_main[[v]], 1),
    stringsAsFactors = FALSE
  )
}
tab33 <- do.call(rbind, rows)

ft_tab33 <- flextable::flextable(tab33) |>
  flextable::set_header_labels(
    分组 = "量表分组",
    维度 = "维度",
    统计量 = sprintf("主分析样本 (N=%d) 中位数 (P25,P75)", nrow(df_main))
  ) |>
  flextable::bold(i = ~ 分组 != "", j = 1, part = "body") |>
  flextable::bg(i = ~ 分组 != "", bg = "#F2F2F2", part = "body") |>
  flextable::width(j = 1, width = 2.1) |>
  flextable::width(j = 2, width = 2.3) |>
  flextable::width(j = 3, width = 2.0) |>
  apply_table_theme(
    footnotes = c(
      "EQ-5D-5L 效用值采用中国人群价值集 MULT8r 模型计算。",
      "QLQ-C30、QLQ-CR29 各维度按官方手册线性转换为 0–100 分。",
      "功能维度与整体健康域：高分=生命质量好；症状维度：高分=症状负担重。",
      "CR29 分支维度按造口状态分别在相应亚组内计算。"
    )
  )
write_table_to_docx(
  ft = ft_tab33,
  title = sprintf("表 3-3  主分析样本健康相关生命质量各维度得分现状（N = %d）", nrow(df_main)),
  file_path = file.path(output_path, "Table3-3_HRQoL现状.docx")
)

message("[模块03] Table 3-2 / 3-3 已输出。")
