# ==============================================================
# 模块 07：CHE 风险因素分析（Table 3-7）
# ==============================================================
# 修复（fix2）：用 Firth 惩罚 Logistic（logistf 包）替代普通 glm，
#   解决"完全分离"（complete separation）导致的 OR=∞ 问题。
#   若用户未安装 logistf，自动降级为：仅展示单因素 OR + 标注降级原因。
# ==============================================================

candidate_vars <- list(
  list(var = "stage_group",     label = "肿瘤分期"),
  list(var = "residence_group", label = "家庭常住地"),
  list(var = "comorbid_group",  label = "合并慢性病"),
  list(var = "stoma_group",     label = "术后造口"),
  list(var = "income_group",    label = "月均收入"),
  list(var = "age_group",       label = "年龄分组"),
  list(var = "insurance_group", label = "医疗保险类型")
)

# 0. 准备数据 ---------------------------------------------------
df_che <- df_main |> dplyr::filter(!is.na(is_che))

# 1. 单因素分析 + 单因素 OR -------------------------------------
uni_rows <- list()
for (cv in candidate_vars) {
  v <- cv$var
  if (!v %in% names(df_che)) next
  g <- df_che[[v]]
  p <- compare_categorical(df_che$is_che, g)
  levs <- if (is.factor(g)) levels(g) else sort(unique(as.character(g[!is.na(g)])))
  ref <- levs[1]; exp_lv <- levs[2]
  a  <- sum(df_che$is_che == 1 & g == exp_lv, na.rm = TRUE)
  b  <- sum(df_che$is_che == 0 & g == exp_lv, na.rm = TRUE)
  c_ <- sum(df_che$is_che == 1 & g == ref,    na.rm = TRUE)
  d  <- sum(df_che$is_che == 0 & g == ref,    na.rm = TRUE)
  or_uni <- tryCatch({
    aa <- a + 0.5; bb <- b + 0.5; cc <- c_ + 0.5; dd <- d + 0.5
    or <- (aa * dd) / (bb * cc)
    se <- sqrt(1/aa + 1/bb + 1/cc + 1/dd)
    list(or = or, lo = exp(log(or) - 1.96 * se),
         hi = exp(log(or) + 1.96 * se))
  }, error = function(e) list(or = NA, lo = NA, hi = NA))

  uni_rows[[length(uni_rows) + 1]] <- data.frame(
    var_label = cv$label,
    var_name  = v,
    ref       = ref,
    exp_lv    = exp_lv,
    `单因素OR` = if (is.na(or_uni$or)) "—" else
      sprintf("%.2f (%.2f, %.2f)", or_uni$or, or_uni$lo, or_uni$hi),
    `单因素P`  = fmt_p(p),
    p_num     = p,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}
uni_tab <- do.call(rbind, uni_rows)

# 2. 多变量 Firth Logistic --------------------------------------
selected_vars <- uni_tab$var_name[!is.na(uni_tab$p_num) & uni_tab$p_num < 0.20]
multi_or <- rep("—", nrow(uni_tab))
multi_p  <- rep("—", nrow(uni_tab))
hl_p_str <- "—"
firth_note <- ""

has_logistf <- requireNamespace("logistf", quietly = TRUE)

if (!has_logistf) {
  message("[模块07] 未安装 logistf 包，正在自动安装……")
  install_ok <- tryCatch({
    install.packages("logistf", dependencies = TRUE,
                     repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
    requireNamespace("logistf", quietly = TRUE)
  }, error = function(e) FALSE)
  has_logistf <- isTRUE(install_ok)
}

if (length(selected_vars) >= 1 &&
    sum(!is.na(df_che$is_che)) >= 10 &&
    has_logistf) {

  fml <- as.formula(paste("is_che ~", paste(selected_vars, collapse = " + ")))

  mod_firth <- tryCatch(
    logistf::logistf(fml, data = df_che),
    error = function(e) NULL
  )

  if (!is.null(mod_firth)) {
    coefs <- mod_firth$coefficients
    ci_lo <- mod_firth$ci.lower
    ci_hi <- mod_firth$ci.upper
    pvs   <- mod_firth$prob
    terms <- names(coefs)

    for (i in seq_len(nrow(uni_tab))) {
      v <- uni_tab$var_name[i]
      idx <- grep(paste0("^", v), terms)
      if (length(idx) == 0) next
      tm <- terms[idx[1]]
      est <- coefs[tm]
      lo  <- ci_lo[tm]
      hi  <- ci_hi[tm]
      pv  <- pvs[tm]
      multi_or[i] <- sprintf("%.2f (%.2f, %.2f)", exp(est), exp(lo), exp(hi))
      multi_p[i]  <- fmt_p(pv)
    }

    firth_note <- "多变量模型采用 Firth 惩罚 Logistic 回归（logistf 包），以解决小样本完全分离问题。"

    # 拟合优度（Firth 模型用 AUC 简单评估，HL 不适用 Firth 框架）
    if (requireNamespace("pROC", quietly = TRUE)) {
      pred <- tryCatch(predict(mod_firth, type = "response"),
                       error = function(e) NULL)
      if (!is.null(pred)) {
        auc <- tryCatch(
          as.numeric(pROC::auc(df_che$is_che, pred, quiet = TRUE)),
          error = function(e) NA_real_
        )
        if (!is.na(auc)) hl_p_str <- sprintf("AUC = %.3f", auc)
      }
    }
  }
} else if (!has_logistf) {
  firth_note <- "logistf 包未安装且自动安装失败，本表仅展示单因素结果；如需多变量调整请先 install.packages('logistf')。"
}

uni_tab$`多变量OR` <- multi_or
uni_tab$`多变量P`  <- multi_p

# 3. 构造最终表 -------------------------------------------------
tab37 <- uni_tab[, c("var_label", "ref", "exp_lv",
                     "单因素OR", "单因素P",
                     "多变量OR", "多变量P")]
names(tab37) <- c("变量", "参照", "对照", "单因素OR", "单因素P",
                  "多变量OR", "多变量P")

# 4. 输出 -------------------------------------------------------
ft_tab37 <- flextable::flextable(tab37) |>
  flextable::bold(j = 1, part = "body") |>
  flextable::width(j = 1, width = 1.7) |>
  flextable::width(j = c(2, 3), width = 1.1) |>
  flextable::width(j = c(4, 6), width = 1.7) |>
  flextable::width(j = c(5, 7), width = 0.8) |>
  apply_table_theme(
    footnotes = c(
      "单因素 OR 由 2×2 表（含连续性校正 0.5）估算；P 值采用卡方/Fisher 检验。",
      "多变量 Logistic 模型纳入单因素分析中 P<0.20 的自变量。",
      firth_note,
      sprintf("模型评价：%s", hl_p_str),
      "OR > 1 表示对照水平较参照水平 CHE 风险升高。"
    )
  )

write_table_to_docx(
  ft = ft_tab37,
  title = "表 3-7  CHE 风险因素分析（单因素 + 多变量 Firth 惩罚 Logistic）",
  file_path = file.path(output_path, "Table3-7_CHE风险因素.docx")
)

message("[模块07] Table 3-7 已输出。")
