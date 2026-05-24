# ==============================================================
# 模块 06：三量表聚合效度（Table 3-6 + Figure 3-2 热力图）
# ==============================================================
# 描述对象：df_main
# 内容：10 对预设维度的 Spearman ρ + 显著性；13 维度热力图
# ==============================================================

calc_spearman <- function(x, y) {
  keep <- !is.na(x) & !is.na(y) & is.finite(x) & is.finite(y)
  if (sum(keep) < 5) return(list(r = NA_real_, p = NA_real_, n = sum(keep)))
  res <- tryCatch(
    cor.test(x[keep], y[keep], method = "spearman", exact = FALSE),
    error = function(e) NULL
  )
  if (is.null(res)) return(list(r = NA_real_, p = NA_real_, n = sum(keep)))
  list(r = unname(res$estimate), p = res$p.value, n = sum(keep))
}

conv_pairs <- list(
  list(v1 = "eq5d_pain",            l1 = "EQ-5D 疼痛/不适",
       v2 = "c30_PA",               l2 = "C30 疼痛"),
  list(v1 = "eq5d_anxiety",         l1 = "EQ-5D 焦虑/抑郁",
       v2 = "c30_EF",               l2 = "C30 情绪功能"),
  list(v1 = "eq5d_mobility",        l1 = "EQ-5D 行动能力",
       v2 = "c30_PF",               l2 = "C30 躯体功能"),
  list(v1 = "eq5d_usual_act",       l1 = "EQ-5D 日常活动",
       v2 = "c30_RF",               l2 = "C30 角色功能"),
  list(v1 = "eq5d_anxiety",         l1 = "EQ-5D 焦虑/抑郁",
       v2 = "cr29_ANX",             l2 = "CR29 体重/健康担忧"),
  list(v1 = "c30_QL",               l1 = "C30 整体健康/QoL",
       v2 = "eq5d_vas",             l2 = "EQ-VAS"),
  list(v1 = "eq5d_utility_mult8r",  l1 = "EQ-5D 效用值",
       v2 = "c30_QL",               l2 = "C30 整体健康/QoL"),
  list(v1 = "cr29_GI",              l1 = "CR29 胃肠道症状",
       v2 = "c30_DI",               l2 = "C30 腹泻"),
  list(v1 = "cr29_AP_pain",         l1 = "CR29 肛周疼痛",
       v2 = "c30_PA",               l2 = "C30 疼痛"),
  list(v1 = "cr29_UI",              l1 = "CR29 排尿问题",
       v2 = "c30_FA",               l2 = "C30 疲乏")
)

rows <- list()
for (p in conv_pairs) {
  if (!all(c(p$v1, p$v2) %in% names(df_main))) next
  rs <- calc_spearman(safe_num(df_main[[p$v1]]),
                      safe_num(df_main[[p$v2]]))
  rows[[length(rows) + 1]] <- data.frame(
    维度A = p$l1, 维度B = p$l2,
    N = rs$n,
    Spearmanρ = if (is.na(rs$r)) "—" else sprintf("%.3f", rs$r),
    P值 = fmt_p(rs$p),
    stringsAsFactors = FALSE
  )
}
tab36 <- do.call(rbind, rows)

ft_tab36 <- flextable::flextable(tab36) |>
  flextable::width(j = c(1, 2), width = 2.0) |>
  flextable::width(j = 3, width = 0.6) |>
  flextable::width(j = 4, width = 1.1) |>
  flextable::width(j = 5, width = 0.9) |>
  apply_table_theme(
    footnotes = c(
      "Spearman 秩相关；|ρ|≥0.3 视为中等相关，|ρ|≥0.5 视为强相关。",
      "EQ-5D 各维度按原始编码（高分=问题重）解读；C30 功能域高分=功能好；C30 / CR29 症状域高分=症状重。"
    )
  )

write_table_to_docx(
  ft = ft_tab36,
  title = sprintf("表 3-6  三种量表预设维度的 Spearman 秩相关分析（N = %d）", nrow(df_main)),
  file_path = file.path(output_path, "Table3-6_聚合效度.docx")
)

# ----------------- Figure 3-2 热力图 --------------------------
hm_vars <- intersect(
  c("eq5d_utility_mult8r","eq5d_vas","eq5d_mobility","eq5d_pain","eq5d_anxiety",
    "c30_QL","c30_PF","c30_EF","c30_PA","c30_FA","cr29_GI","cr29_ANX","cr29_BI"),
  names(df_main)
)
hm_labels <- c(
  "EQ-5D效用","EQ-VAS","EQ-5D行动","EQ-5D疼痛","EQ-5D焦虑",
  "C30整体QoL","C30躯体","C30情绪","C30疼痛","C30疲乏",
  "CR29胃肠道","CR29担忧","CR29体像"
)[seq_along(hm_vars)]

mat <- matrix(NA_real_, length(hm_vars), length(hm_vars),
              dimnames = list(hm_labels, hm_labels))
for (i in seq_along(hm_vars)) {
  for (j in seq_along(hm_vars)) {
    if (i == j) { mat[i, j] <- 1; next }
    mat[i, j] <- calc_spearman(
      safe_num(df_main[[hm_vars[i]]]),
      safe_num(df_main[[hm_vars[j]]])
    )$r
  }
}
long <- reshape2::melt(mat, varnames = c("Var1","Var2"), value.name = "rho")
long$Var1 <- factor(long$Var1, levels = hm_labels)
long$Var2 <- factor(long$Var2, levels = rev(hm_labels))
long$lab  <- ifelse(is.na(long$rho), "", sprintf("%.2f", long$rho))

fig <- ggplot2::ggplot(long, ggplot2::aes(x = Var1, y = Var2, fill = rho)) +
  ggplot2::geom_tile(color = "white") +
  ggplot2::geom_text(ggplot2::aes(label = lab), size = 2.8) +
  ggplot2::scale_fill_gradient2(
    low = "#2166AC", mid = "white", high = "#D6604D",
    midpoint = 0, limits = c(-1, 1), name = "Spearman ρ",
    na.value = "grey90"
  ) +
  ggplot2::labs(
    title = sprintf("图 3-2  三种量表聚合效度热力图（N=%d）", nrow(df_main)),
    x = NULL, y = NULL
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = ggplot2::element_text(size = 8),
    panel.grid = ggplot2::element_blank()
  )

ggplot2::ggsave(
  filename = file.path(output_path, "Figure3-2_聚合效度热力图.png"),
  plot = fig, width = 10, height = 9, dpi = 300, bg = "white"
)

message("[模块06] Table 3-6 与 Figure 3-2 已输出。")
