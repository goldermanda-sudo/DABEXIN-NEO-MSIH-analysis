# ==============================================================
# 模块 00：环境准备、数据读入、字典映射、纳排筛选
# ==============================================================
# 输出：
#   df_main          —— 主分析样本
#   df_recur         —— 主分析样本中"已转移/复发"亚组
#   df_nonrecur      —— 主分析样本中"未转移/复发"亚组
#   df_dabexin       —— 探索性达伯欣方案个案样本
#   df_extend        —— 主分析 + 通过质控但术后>3月扩展样本
#   df_dict          —— 数据字典
#   各工具函数（fmt_*, compare_*, apply_theme 等）
# ==============================================================
# 关键修复（fix2）：
# 1) 新数据"按文本"模式导出，所有量表题目都是中文字符串，
#    本模块新增 decode_eq5d / decode_c30_cr29 函数，把所有 EQ-5D、
#    C30、CR29 题目从中文文本统一转换为数字编码（1-5 或 1-4）。
#    转换后的列覆盖原列，下游模块无需修改。
# 2) caregiver_type、cr29_d18_stoma 改为中文文本匹配。
# 3) C30 E30/E31 数据中是 "1（非常差）" / "7（非常好）" 形式，
#    需要把括号备注剔除再转数字。
# ==============================================================


# 1. 路径与全局参数 ----------------------------------------------

input_path  <- "C:/Users/zlydg/Desktop/毕业设计新/R代码数据处理/simplified_R_v2_fix2"
output_path <- "C:/Users/zlydg/Desktop/毕业设计新/R代码数据处理/simplified_R_v2_fix2/结果输出"

if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

DAILY_WAGE_SOCIAL <- 320
CHE_THRESHOLD     <- 0.20
INCOME_CUT        <- 5000
AGE_CUT           <- 60
POSTOP_WINDOW_MO  <- 3
QC_ANSWER         <- "非常"
SURVEY_YEAR       <- 2026


# 2. 加载扩展包 --------------------------------------------------

required_pkgs <- c(
  "readxl", "dplyr", "tidyr", "stringr", "lubridate",
  "flextable", "officer", "ggplot2", "reshape2"
)
for (pkg in required_pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE,
                     repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
    library(pkg, character.only = TRUE)
  }
}


# 3. 指定数据文件与数据字典 ---------------------------------------

data_file_name <- "副本20260522-353820545_按文本_结肠癌患者疾病经济负担和健康相关生命质量调查_82_78"
dict_file_name <- "结肠癌QoL问卷_数据字典总表(146列)"

find_excel_file <- function(folder, base_name) {
  candidates <- file.path(folder, c(base_name,
                                    paste0(base_name, ".xlsx"),
                                    paste0(base_name, ".xls")))
  candidates <- unique(candidates)
  hit <- candidates[file.exists(candidates)]
  if (length(hit) > 0) return(hit[1])

  all_excel <- list.files(folder, pattern = "\\.xls[x]?$", full.names = TRUE)
  hit2 <- all_excel[tools::file_path_sans_ext(basename(all_excel)) == base_name |
                      basename(all_excel) == base_name]
  if (length(hit2) > 0) return(hit2[1])

  return(NA_character_)
}

data_file <- find_excel_file(input_path, data_file_name)
dict_file <- find_excel_file(input_path, dict_file_name)

if (is.na(data_file) || !file.exists(data_file)) {
  stop("未找到原始数据文件：", data_file_name)
}
if (is.na(dict_file) || !file.exists(dict_file)) {
  stop("未找到数据字典文件：", dict_file_name)
}

message("[模块00] 数据文件：", basename(data_file))
message("[模块00] 字典文件：", basename(dict_file))


# 4. 读取数据 ----------------------------------------------------

df_dict <- readxl::read_excel(dict_file)
df_raw  <- readxl::read_excel(data_file)

message(sprintf("[模块00] 原始数据 %d 行 × %d 列；字典 %d 行。",
                nrow(df_raw), ncol(df_raw), nrow(df_dict)))


# 5. 122 列 → 英文变量名 的位置映射 -----------------------------

rename_map <- c(
  "sys_id", "sys_submit_time", "sys_duration",
  "sys_source", "sys_source_detail", "sys_ip",
  "sys_total_score", "is_qualified", "sys_consent",
  "eq5d_mobility", "eq5d_self_care", "eq5d_usual_act",
  "eq5d_pain", "eq5d_anxiety", "eq5d_vas",
  paste0("cr29_d", sprintf("%02d", 1:17)),
  "cr29_d18_stoma",
  paste0("cr29_d", sprintf("%02d", 19:25)),
  "gender_filter",
  "cr29_d26_male", "cr29_d27_male",
  "cr29_d28_female", "cr29_d29_female",
  paste0("c30_e", sprintf("%02d", 1:14)),
  "c30_e15_qc",
  paste0("c30_e", sprintf("%02d", 16:31)),
  "cost_total", "cost_exam", "cost_surgery",
  "cost_drug_neoadj", "cost_drug_other",
  "cost_treatment", "cost_other",
  "cost_oop",
  "cost_travel", "cost_accom", "cost_nutri", "cost_equip",
  "days_preop", "days_postop",
  "caregiver_type",
  "care_prof_days", "care_prof_daily_cost",
  "care_fam_days",
  "care_both_prof_days", "care_both_prof_cost", "care_both_fam_days",
  "fund_source_text", "finan_burden",
  "diag_date", "stage_diag", "site_text",
  "has_neoadj", "neoadj_dabexin",
  "date_neoadj_start", "date_neoadj_end",
  "surgery_date", "stoma_status",
  "recur_status", "date_recur",
  "gender", "residence", "birth_year",
  "edu_level", "marital_status",
  "occup_status", "occup_type",
  "insurance_type", "monthly_income",
  "has_comorbid_raw", "comorb_text",
  "payment_method"
)

if (length(rename_map) != ncol(df_raw)) {
  stop(sprintf("[模块00] 映射表 %d 项 vs 数据 %d 列，请确认数据文件结构。",
               length(rename_map), ncol(df_raw)))
}
colnames(df_raw) <- rename_map


# 6. 通用工具函数 -----------------------------------------------

safe_num <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

# 中文宽容比较：trim、忽略括号备注、统一罗马数字与全角符号
normalize_chi <- function(x) {
  s <- as.character(x)
  s <- trimws(s)
  s <- gsub("\\s*[（(].*?[）)]", "", s)
  s <- gsub("\\s*\\[.*?\\]", "", s)
  s <- gsub("＞", ">", s, fixed = TRUE)
  s <- gsub("＜", "<", s, fixed = TRUE)
  s <- gsub("：", ":", s, fixed = TRUE)
  s <- gsub("，", ",", s, fixed = TRUE)
  s <- gsub("\u2160", "I",   s, fixed = TRUE)
  s <- gsub("\u2161", "II",  s, fixed = TRUE)
  s <- gsub("\u2162", "III", s, fixed = TRUE)
  s <- gsub("\u2163", "IV",  s, fixed = TRUE)
  s <- gsub("\u2164", "V",   s, fixed = TRUE)
  s
}

chi_in <- function(x, set) {
  normalize_chi(x) %in% normalize_chi(set)
}

# === 量表分级中文 → 数字 解码器（核心新增）====================
# (a) EQ-5D 5 维（5 级）
decode_eq5d <- function(x) {
  s <- as.character(x)
  s <- gsub("\\s+", "", s)
  dplyr::case_when(
    grepl("没有困难|没有疼痛|没有不舒服|没有焦虑|没有沮丧", s) ~ 1L,
    grepl("有一点", s) ~ 2L,
    grepl("中度", s)   ~ 3L,
    grepl("严重", s)   ~ 4L,
    grepl("无法|极度", s) ~ 5L,
    TRUE ~ NA_integer_
  )
}

# (b) C30 4 级题（E01–E29，除 E15 质控、E30/E31）
decode_c30_4 <- function(x) {
  s <- normalize_chi(x)
  dplyr::case_when(
    s == "没有" ~ 1L,
    s == "有点" ~ 2L,
    s == "相当" ~ 3L,
    s == "非常" ~ 4L,
    TRUE ~ NA_integer_
  )
}

# (c) C30 E30 / E31 7 级题（数据中是 "1（非常差）" ~ "7（非常好）"，
#     normalize_chi 会去掉括号备注，剩 "1"~"7"）
decode_c30_7 <- function(x) {
  suppressWarnings(as.integer(normalize_chi(x)))
}

# (d) CR29 4 级题
decode_cr29_4 <- function(x) {
  s <- normalize_chi(x)
  dplyr::case_when(
    s == "一点也不" ~ 1L,
    s == "有一点"   ~ 2L,
    s == "有些"     ~ 3L,
    s == "经常"     ~ 4L,
    # 兼容个别题目可能用 C30 风格
    s == "没有" ~ 1L,
    s == "有点" ~ 2L,
    s == "相当" ~ 3L,
    s == "非常" ~ 4L,
    TRUE ~ NA_integer_
  )
}

# (e) CR29 D18 造口（是/否 二分类）→ 编码 1=有造口袋, 2=无造口袋
decode_d18_stoma <- function(x) {
  dplyr::case_when(
    chi_in(x, "是") ~ 1L,
    chi_in(x, "否") ~ 2L,
    TRUE ~ NA_integer_
  )
}

# 日期解析
safe_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(suppressWarnings(as.Date(x, origin = "1899-12-30")))
  s <- trimws(as.character(x))
  s[s == "" | s == "(跳过)"] <- NA_character_
  out <- suppressWarnings(as.Date(s))
  bad <- is.na(out) & !is.na(s)
  if (any(bad)) {
    out[bad] <- suppressWarnings(as.Date(
      lubridate::parse_date_time(
        s[bad],
        orders = c("ymd HMS","ymd HM","ymd","Ymd HMS","Ymd HM","Ymd",
                   "y/m/d HMS","y/m/d HM","y/m/d",
                   "y.m.d HMS","y.m.d HM","y.m.d",
                   "y-m-d HMS","y-m-d HM","y-m-d")
      )
    ))
  }
  out
}

extract_year <- function(x) {
  s <- as.character(x)
  suppressWarnings(as.integer(substr(s, 1, 4)))
}

parse_duration_sec <- function(x) {
  s <- as.character(x)
  suppressWarnings(as.numeric(gsub("[^0-9.]", "", s)))
}


# 7. 量表题目"中文 → 数字"批量解码（核心修复）------------------
# 这一步把所有 EQ-5D、C30、CR29 题目从中文转为数字，覆盖原列。
# 这样后续 01 模块的所有 safe_num(...) 调用就能直接拿到 1-5 / 1-4 / 1-7。

# EQ-5D 5 个维度（5 级）
df_raw <- df_raw |>
  dplyr::mutate(dplyr::across(
    dplyr::all_of(c("eq5d_mobility", "eq5d_self_care", "eq5d_usual_act",
                    "eq5d_pain", "eq5d_anxiety")),
    decode_eq5d
  ))

# C30 E01–E14、E16–E29（4 级题）
c30_4_cols <- c(paste0("c30_e", sprintf("%02d", 1:14)),
                paste0("c30_e", sprintf("%02d", 16:29)))
c30_4_cols <- intersect(c30_4_cols, names(df_raw))
df_raw <- df_raw |>
  dplyr::mutate(dplyr::across(dplyr::all_of(c30_4_cols), decode_c30_4))

# C30 E30 / E31（7 级题）
c30_7_cols <- intersect(c("c30_e30", "c30_e31"), names(df_raw))
df_raw <- df_raw |>
  dplyr::mutate(dplyr::across(dplyr::all_of(c30_7_cols), decode_c30_7))

# CR29 D01–D17、D19–D29（4 级题，不含 D18 造口）
cr29_4_cols <- c(paste0("cr29_d", sprintf("%02d", c(1:17, 19:25))),
                 "cr29_d26_male", "cr29_d27_male",
                 "cr29_d28_female", "cr29_d29_female")
cr29_4_cols <- intersect(cr29_4_cols, names(df_raw))
df_raw <- df_raw |>
  dplyr::mutate(dplyr::across(dplyr::all_of(cr29_4_cols), decode_cr29_4))

# CR29 D18 造口（是/否 → 1/2）
df_raw <- df_raw |>
  dplyr::mutate(cr29_d18_stoma = decode_d18_stoma(cr29_d18_stoma))

# EQ-VAS 保持数值（数据本身就是 0–100）
df_raw <- df_raw |>
  dplyr::mutate(eq5d_vas = safe_num(eq5d_vas))


# 8. 关键派生变量 ------------------------------------------------

df0 <- df_raw |>
  dplyr::mutate(
    age            = SURVEY_YEAR - extract_year(birth_year),
    duration_sec   = parse_duration_sec(sys_duration),
    submit_date    = safe_date(sys_submit_time),
    diag_date      = safe_date(diag_date),
    surgery_date   = safe_date(surgery_date),
    months_postop  = as.numeric(submit_date - surgery_date) / 30,

    # 肿瘤分期
    stage_group = factor(
      dplyr::case_when(
        chi_in(stage_diag, c("I期", "II期"))   ~ "I-II期",
        chi_in(stage_diag, c("III期", "IV期")) ~ "III-IV期",
        TRUE ~ NA_character_
      ),
      levels = c("I-II期", "III-IV期")
    ),
    stage_diag_num = dplyr::case_when(
      chi_in(stage_diag, "I期")   ~ 1,
      chi_in(stage_diag, "II期")  ~ 2,
      chi_in(stage_diag, "III期") ~ 3,
      chi_in(stage_diag, "IV期")  ~ 4,
      TRUE ~ NA_real_
    ),

    residence_group = factor(
      dplyr::case_when(
        chi_in(residence, "城镇") ~ "城镇",
        chi_in(residence, "农村") ~ "农村",
        TRUE ~ NA_character_
      ),
      levels = c("城镇", "农村")
    ),

    stoma_group = factor(
      dplyr::case_when(
        chi_in(stoma_status, "无造口") ~ "无造口",
        chi_in(stoma_status, c("临时造口", "永久造口")) ~ "有造口",
        TRUE ~ NA_character_
      ),
      levels = c("无造口", "有造口")
    ),

    income_num = dplyr::case_when(
      chi_in(monthly_income, "<2000元")      ~ 1000,
      chi_in(monthly_income, "2000-5000元")  ~ 3500,
      chi_in(monthly_income, "5001-10000元") ~ 7500,
      chi_in(monthly_income, ">10000元")     ~ 15000,
      TRUE ~ NA_real_
    ),
    income_group = factor(
      dplyr::case_when(
        is.na(income_num)         ~ NA_character_,
        income_num <= INCOME_CUT  ~ "≤5000元",
        income_num >  INCOME_CUT  ~ ">5000元"
      ),
      levels = c("≤5000元", ">5000元")
    ),

    age_group = factor(
      dplyr::case_when(
        is.na(age)     ~ NA_character_,
        age <= AGE_CUT ~ "≤60岁",
        age >  AGE_CUT ~ ">60岁"
      ),
      levels = c("≤60岁", ">60岁")
    ),

    insurance_group = factor(
      dplyr::case_when(
        grepl("城镇职工", as.character(insurance_type)) ~ "城镇职工医保",
        is.na(insurance_type) | trimws(insurance_type) == "" ~ NA_character_,
        TRUE ~ "其他医保"
      ),
      levels = c("城镇职工医保", "其他医保")
    ),

    has_comorbid = dplyr::case_when(
      chi_in(has_comorbid_raw, "是") ~ 1L,
      chi_in(has_comorbid_raw, "否") ~ 0L,
      TRUE ~ NA_integer_
    ),
    comorbid_group = factor(
      dplyr::case_when(
        has_comorbid == 1 ~ "有合并症",
        has_comorbid == 0 ~ "无合并症",
        TRUE ~ NA_character_
      ),
      levels = c("无合并症", "有合并症")
    ),

    recur_flag = dplyr::case_when(
      chi_in(recur_status, "是") ~ 1L,
      chi_in(recur_status, "否") ~ 0L,
      TRUE ~ NA_integer_
    ),
    recur_group = factor(
      dplyr::case_when(
        recur_flag == 1 ~ "已转移/复发",
        recur_flag == 0 ~ "未转移/复发",
        TRUE ~ NA_character_
      ),
      levels = c("未转移/复发", "已转移/复发")
    ),

    qualified_flag = dplyr::case_when(
      chi_in(is_qualified, "是") ~ 1L,
      chi_in(is_qualified, "否") ~ 0L,
      TRUE ~ NA_integer_
    ),

    neoadj_flag = dplyr::case_when(
      chi_in(has_neoadj, "是") ~ 1L,
      chi_in(has_neoadj, "否") ~ 0L,
      TRUE ~ NA_integer_
    ),
    dabexin_flag = dplyr::case_when(
      chi_in(neoadj_dabexin, "是") ~ 1L,
      chi_in(neoadj_dabexin, "否") ~ 0L,
      TRUE ~ NA_integer_
    ),

    # 教育水平按受教育程度排序（用于 Table 3-1）
    edu_level_ord = factor(
      dplyr::case_when(
        chi_in(edu_level, "没上过学") ~ "没上过学",
        chi_in(edu_level, "小学水平") ~ "小学水平",
        chi_in(edu_level, "初中水平") ~ "初中水平",
        chi_in(edu_level, "高中水平") ~ "高中水平",
        chi_in(edu_level, "大学水平") ~ "大学水平",
        chi_in(edu_level, "更高")     ~ "更高",
        TRUE ~ NA_character_
      ),
      levels = c("没上过学", "小学水平", "初中水平",
                 "高中水平", "大学水平", "更高")
    ),

    # 陪护方式（中文匹配，供 derive_cost 使用）
    caregiver_type_num = dplyr::case_when(
      chi_in(caregiver_type, "无")               ~ 1L,
      chi_in(caregiver_type, "请专门的护工")     ~ 2L,
      chi_in(caregiver_type, "家人护理")         ~ 3L,
      chi_in(caregiver_type, "护工和家人同时护理") ~ 4L,
      TRUE ~ NA_integer_
    ),
    # 把陪护字段中可能存在的非数字（如"(跳过)"）转为 NA 数值
    care_prof_days        = safe_num(care_prof_days),
    care_prof_daily_cost  = safe_num(care_prof_daily_cost),
    care_fam_days         = safe_num(care_fam_days),
    care_both_prof_days   = safe_num(care_both_prof_days),
    care_both_prof_cost   = safe_num(care_both_prof_cost),
    care_both_fam_days    = safe_num(care_both_fam_days),
    days_preop            = safe_num(days_preop),
    days_postop           = safe_num(days_postop)
  )


# 9. 数据质量四道关卡 -------------------------------------------

# 9.1 质控题（c30_e15_qc 现在仍是中文）
df0$pass_qc <- as.integer(chi_in(df0$c30_e15_qc, QC_ANSWER))

# 9.2 费用录入规范
fee_cols <- c("cost_total", "cost_oop", "cost_exam", "cost_surgery",
              "cost_drug_neoadj", "cost_drug_other", "cost_treatment", "cost_other")
fee_cols <- intersect(fee_cols, names(df0))

is_pure_number <- function(x) {
  if (is.na(x)) return(TRUE)
  s <- trimws(as.character(x))
  if (s == "") return(TRUE)
  suppressWarnings(!is.na(as.numeric(s)))
}

if (length(fee_cols) == 0) {
  df0$pass_fee_format <- TRUE
} else {
  df0$pass_fee_format <- apply(
    df0[, fee_cols, drop = FALSE], 1,
    function(row) all(sapply(row, is_pure_number))
  )
}

# 9.3 日期逻辑
df0$pass_date_logic <- with(df0, {
  cond1 <- is.na(diag_date)    | is.na(surgery_date) | (diag_date    <= surgery_date)
  cond2 <- is.na(surgery_date) | is.na(submit_date)  | (surgery_date <= submit_date)
  cond1 & cond2
})

# 9.4 答题用时
df0$pass_duration <- df0$duration_sec >= 120 | is.na(df0$duration_sec)

# 9.5 综合
df0$pass_hard_qc <- with(df0,
  pass_qc == 1 & pass_fee_format & pass_date_logic & pass_duration
)


# 10. 样本切分 ---------------------------------------------------

df0$in_main <- with(df0,
  pass_hard_qc &
    (is.na(dabexin_flag) | dabexin_flag != 1) &
    ((!is.na(months_postop) & months_postop <= POSTOP_WINDOW_MO) |
       (!is.na(recur_flag) & recur_flag == 1))
)

df0$in_extend <- with(df0,
  pass_hard_qc & (is.na(dabexin_flag) | dabexin_flag != 1)
)

df0$in_dabexin <- with(df0,
  pass_hard_qc &
    !is.na(dabexin_flag) & dabexin_flag == 1 &
    !is.na(months_postop) & months_postop <= POSTOP_WINDOW_MO
)

df_main     <- dplyr::filter(df0, in_main)
df_recur    <- dplyr::filter(df_main, recur_flag == 1)
df_nonrecur <- dplyr::filter(df_main, recur_flag == 0)
df_dabexin  <- dplyr::filter(df0, in_dabexin)
df_extend   <- dplyr::filter(df0, in_extend)


# 11. 通用展示工具函数 ------------------------------------------

fmt_p <- function(p) {
  p <- suppressWarnings(as.numeric(p))[1]
  if (is.na(p)) return("—")
  if (p < 0.001) return("<0.001")
  sprintf("%.3f", p)
}

fmt_median_iqr <- function(x, digits = 1) {
  x <- safe_num(x)
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0) return("—")
  sprintf(paste0("%.", digits, "f (%.", digits, "f, %.", digits, "f)"),
          median(x), quantile(x, 0.25), quantile(x, 0.75))
}

fmt_n_pct <- function(x, event = 1) {
  x <- safe_num(x)
  x <- x[!is.na(x)]
  if (length(x) == 0) return("—")
  sprintf("%d/%d (%.1f%%)", sum(x == event), length(x), 100 * mean(x == event))
}

compare_continuous <- function(x, g) {
  d <- data.frame(x = safe_num(x), g = g)
  d <- d[!is.na(d$x) & !is.na(d$g), ]
  if (nrow(d) == 0 || length(unique(d$g)) != 2) return(NA_real_)
  g_factor <- factor(d$g)
  g1 <- d$x[g_factor == levels(g_factor)[1]]
  g2 <- d$x[g_factor == levels(g_factor)[2]]
  if (length(g1) < 2 || length(g2) < 2) return(NA_real_)
  tryCatch(wilcox.test(g1, g2, exact = FALSE)$p.value,
           error = function(e) NA_real_)
}

compare_categorical <- function(x, g) {
  keep <- !is.na(x) & !is.na(g)
  x <- x[keep]; g <- g[keep]
  if (length(x) == 0 || length(unique(g)) != 2) return(NA_real_)
  tab <- table(g, x)
  if (any(dim(tab) < 2)) return(NA_real_)
  chi <- tryCatch(chisq.test(tab, correct = FALSE), error = function(e) NULL)
  if (!is.null(chi) && all(chi$expected >= 5)) return(chi$p.value)
  tryCatch(fisher.test(tab, simulate.p.value = TRUE, B = 1e4)$p.value,
           error = function(e) NA_real_)
}

# 表标题：放在表上方（药学院 2026 格式要求）
write_table_to_docx <- function(ft, title, file_path, footnotes = NULL) {
  doc <- officer::read_docx() |>
    officer::body_add_par(title, style = "Normal") |>
    flextable::body_add_flextable(ft)
  if (!is.null(footnotes)) {
    for (fn in footnotes) {
      doc <- officer::body_add_par(doc, fn, style = "Normal")
    }
  }
  print(doc, target = file_path)
}

# flextable 三线表主题（脚注仍在表底，标题改为外部 paragraph）
apply_table_theme <- function(ft, footnotes = NULL) {
  ft <- ft |>
    flextable::font(fontname = "Times New Roman", part = "all") |>
    flextable::fontsize(size = 9, part = "all") |>
    flextable::fontsize(size = 8, part = "footer") |>
    flextable::align(align = "center", part = "all") |>
    flextable::align(j = 1, align = "left", part = "all") |>
    flextable::bold(part = "header") |>
    flextable::set_table_properties(layout = "autofit") |>
    flextable::border_remove() |>
    flextable::hline_top(border = officer::fp_border(color = "black", width = 1.5),
                         part = "header") |>
    flextable::hline(i = 1, border = officer::fp_border(color = "black", width = 0.75),
                     part = "header") |>
    flextable::hline_bottom(border = officer::fp_border(color = "black", width = 1.5),
                            part = "body")
  if (!is.null(footnotes)) {
    for (fn in footnotes) ft <- flextable::add_footer_lines(ft, values = fn)
    ft <- ft |>
      flextable::border(part = "footer",
                        border.top = officer::fp_border(width = 0),
                        border.bottom = officer::fp_border(width = 0)) |>
      flextable::border_inner_h(part = "footer",
                                border = officer::fp_border(width = 0))
  }
  ft
}


# 12. 模块完成提示 ----------------------------------------------

message("[模块00] 数据准备完成。")
message(sprintf("  · 主分析样本 n=%d；其中未复发 n=%d，已复发 n=%d",
                nrow(df_main), nrow(df_nonrecur), nrow(df_recur)))
message(sprintf("  · 探索性达伯欣样本 n=%d", nrow(df_dabexin)))
message(sprintf("  · 扩展样本 n=%d", nrow(df_extend)))

# 量表解码状况自检
check_cols <- c("eq5d_mobility", "c30_e01", "cr29_d01", "cr29_d18_stoma")
check_cols <- intersect(check_cols, names(df_main))
if (length(check_cols) > 0) {
  message("  · 量表解码自检（应全为整数 1-5 / 1-4）:")
  for (cn in check_cols) {
    v <- df_main[[cn]]
    message(sprintf("      %-20s 取值范围: [%s, %s]  非NA数: %d/%d",
                    cn,
                    if (all(is.na(v))) "NA" else as.character(min(v, na.rm = TRUE)),
                    if (all(is.na(v))) "NA" else as.character(max(v, na.rm = TRUE)),
                    sum(!is.na(v)), length(v)))
  }
}
