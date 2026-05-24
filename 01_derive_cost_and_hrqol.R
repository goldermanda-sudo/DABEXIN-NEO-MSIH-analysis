# ==============================================================
# 模块 01：费用衍生 + EQ-5D 效用值 + C30/CR29 维度得分
# ==============================================================
# 修复说明（fix2）：
# - derive_cost：用 caregiver_type_num（00 模块新派生）代替原 caregiver_type
# - derive_hrqol：cr29_d18_stoma 现在已经是 1/2 数字，has_stoma 用 == 1 判断
# ==============================================================

# 1. 费用与 CHE -------------------------------------------------
derive_cost <- function(df, daily_wage = DAILY_WAGE_SOCIAL,
                        che_thr = CHE_THRESHOLD) {
  df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c(
          "cost_total","cost_oop","cost_travel","cost_accom","cost_nutri","cost_equip",
          "days_preop","days_postop",
          "care_prof_days","care_prof_daily_cost",
          "care_fam_days",
          "care_both_prof_days","care_both_prof_cost","care_both_fam_days"
        )),
        safe_num
      ),
      prof_care_cost = dplyr::case_when(
        caregiver_type_num == 2 ~ tidyr::replace_na(care_prof_days, 0) *
                                  tidyr::replace_na(care_prof_daily_cost, 0),
        caregiver_type_num == 4 ~ tidyr::replace_na(care_both_prof_days, 0) *
                                  tidyr::replace_na(care_both_prof_cost, 0),
        TRUE                    ~ 0
      ),
      family_care_days = dplyr::case_when(
        caregiver_type_num == 3 ~ tidyr::replace_na(care_fam_days, 0),
        caregiver_type_num == 4 ~ tidyr::replace_na(care_both_fam_days, 0),
        TRUE                    ~ 0
      ),
      patient_daily_wage = dplyr::case_when(
        income_num == 1000  ~ 1000  / 22,
        income_num == 3500  ~ 3500  / 22,
        income_num == 7500  ~ 7500  / 22,
        income_num == 15000 ~ 15000 / 22,
        TRUE                ~ daily_wage
      ),
      direct_med_cost = tidyr::replace_na(cost_total, 0),
      direct_non_med_cost = tidyr::replace_na(cost_travel, 0) +
                            tidyr::replace_na(cost_accom,  0) +
                            tidyr::replace_na(cost_nutri,  0) +
                            tidyr::replace_na(cost_equip,  0) +
                            prof_care_cost,
      indirect_cost = (tidyr::replace_na(days_preop, 0) +
                       tidyr::replace_na(days_postop, 0)) * patient_daily_wage +
                       family_care_days * daily_wage,
      total_burden = direct_med_cost + direct_non_med_cost + indirect_cost,
      cost_oop_clean = tidyr::replace_na(cost_oop, 0),
      annual_income_est = dplyr::case_when(
        income_num == 1000  ~ 1000  * 12 * 1.5,
        income_num == 3500  ~ 3500  * 12 * 1.5,
        income_num == 7500  ~ 7500  * 12 * 1.5,
        income_num == 15000 ~ 15000 * 12 * 1.5,
        TRUE                ~ NA_real_
      ),
      is_che = dplyr::if_else(
        !is.na(annual_income_est) & annual_income_est > 0,
        as.integer(cost_oop_clean / annual_income_est > che_thr),
        NA_integer_
      ),
      is_che_group = factor(
        dplyr::if_else(is_che == 1, "发生CHE", "未发生CHE"),
        levels = c("未发生CHE", "发生CHE")
      )
    )
}

# 2. EQ-5D 效用值（MULT8r 主分析 + ADD20r 敏感性）---------------
# 注意：此时 eq5d_mobility 等已经是数字 1-5，可以直接做索引
derive_eq5d <- function(df) {
  coef_mult <- list(
    MO = c(0, 0.066, 0.158, 0.287, 0.345),
    SC = c(0, 0.048, 0.116, 0.210, 0.253),
    UA = c(0, 0.045, 0.107, 0.194, 0.233),
    PD = c(0, 0.058, 0.138, 0.252, 0.302),
    AD = c(0, 0.049, 0.118, 0.215, 0.258)
  )
  safe_idx <- function(v, l) {
    l <- suppressWarnings(as.integer(l))
    ifelse(is.na(l) | l < 1 | l > 5, NA_real_, v[l])
  }
  df |>
    dplyr::mutate(
      eq5d_utility_mult8r = 1 - (
        safe_idx(coef_mult$MO, eq5d_mobility) +
          safe_idx(coef_mult$SC, eq5d_self_care) +
          safe_idx(coef_mult$UA, eq5d_usual_act) +
          safe_idx(coef_mult$PD, eq5d_pain) +
          safe_idx(coef_mult$AD, eq5d_anxiety)
      ),
      eq5d_utility_add20r = 1 - (
        dplyr::case_when(eq5d_mobility==1~0, eq5d_mobility==2~0.057,
                         eq5d_mobility==3~0.136, eq5d_mobility==4~0.260,
                         eq5d_mobility==5~0.351, TRUE~NA_real_) +
        dplyr::case_when(eq5d_self_care==1~0, eq5d_self_care==2~0.055,
                         eq5d_self_care==3~0.142, eq5d_self_care==4~0.213,
                         eq5d_self_care==5~0.269, TRUE~NA_real_) +
        dplyr::case_when(eq5d_usual_act==1~0, eq5d_usual_act==2~0.056,
                         eq5d_usual_act==3~0.109, eq5d_usual_act==4~0.196,
                         eq5d_usual_act==5~0.236, TRUE~NA_real_) +
        dplyr::case_when(eq5d_pain==1~0, eq5d_pain==2~0.047,
                         eq5d_pain==3~0.131, eq5d_pain==4~0.264,
                         eq5d_pain==5~0.287, TRUE~NA_real_) +
        dplyr::case_when(eq5d_anxiety==1~0, eq5d_anxiety==2~0.027,
                         eq5d_anxiety==3~0.119, eq5d_anxiety==4~0.215,
                         eq5d_anxiety==5~0.253, TRUE~NA_real_)
      )
    )
}

# 3. C30 / CR29 维度得分 ---------------------------------------
calc_scale_score <- function(df, items,
                             type = c("functional", "symptom", "global"),
                             max_raw = 4) {
  type <- match.arg(type)
  items <- intersect(items, names(df))
  if (length(items) == 0) return(rep(NA_real_, nrow(df)))
  m <- as.data.frame(lapply(df[, items, drop = FALSE], safe_num))
  raw_mean <- rowMeans(m, na.rm = TRUE)
  raw_mean[is.nan(raw_mean)] <- NA_real_
  rs <- (raw_mean - 1) / (max_raw - 1)
  if (type == "functional") return((1 - rs) * 100)
  if (type == "global")     return(rs * 100)
  rs * 100
}

c30_domains <- list(
  c30_PF = list(items = c("c30_e01","c30_e02","c30_e03","c30_e04","c30_e05"), type = "functional", max_raw = 4),
  c30_RF = list(items = c("c30_e06","c30_e07"),                              type = "functional", max_raw = 4),
  c30_EF = list(items = c("c30_e22","c30_e23","c30_e24","c30_e25","c30_e27"),type = "functional", max_raw = 4),
  c30_CF = list(items = c("c30_e21","c30_e26"),                              type = "functional", max_raw = 4),
  c30_SF = list(items = c("c30_e27","c30_e28"),                              type = "functional", max_raw = 4),
  c30_QL = list(items = c("c30_e30","c30_e31"),                              type = "global",     max_raw = 6),
  c30_FA = list(items = c("c30_e10","c30_e12","c30_e19"),                    type = "symptom",    max_raw = 4),
  c30_NV = list(items = c("c30_e13","c30_e14"),                              type = "symptom",    max_raw = 4),
  c30_PA = list(items = c("c30_e09","c30_e20"),                              type = "symptom",    max_raw = 4),
  c30_DY = list(items = c("c30_e08"),                                        type = "symptom",    max_raw = 4),
  c30_SL = list(items = c("c30_e11"),                                        type = "symptom",    max_raw = 4),
  c30_AP = list(items = c("c30_e13"),                                        type = "symptom",    max_raw = 4),
  c30_CO = list(items = c("c30_e17"),                                        type = "symptom",    max_raw = 4),
  c30_DI = list(items = c("c30_e18"),                                        type = "symptom",    max_raw = 4),
  c30_FI = list(items = c("c30_e29"),                                        type = "symptom",    max_raw = 4)
)

cr29_common <- list(
  cr29_UI      = list(items = c("cr29_d01","cr29_d02","cr29_d03","cr29_d04"), type = "symptom",    max_raw = 4),
  cr29_GI      = list(items = c("cr29_d05","cr29_d07"),                      type = "symptom",    max_raw = 4),
  cr29_ChSE    = list(items = c("cr29_d10","cr29_d11","cr29_d12"),           type = "symptom",    max_raw = 4),
  cr29_ANX     = list(items = c("cr29_d13","cr29_d14"),                      type = "symptom",    max_raw = 4),
  cr29_BLD     = list(items = c("cr29_d08"),                                 type = "symptom",    max_raw = 4),
  cr29_MUC     = list(items = c("cr29_d09"),                                 type = "symptom",    max_raw = 4),
  cr29_AP_pain = list(items = c("cr29_d06"),                                 type = "symptom",    max_raw = 4),
  cr29_BI      = list(items = c("cr29_d15","cr29_d16","cr29_d17"),           type = "functional", max_raw = 4),
  cr29_SxI_M   = list(items = c("cr29_d26_male","cr29_d27_male"),            type = "functional", max_raw = 4),
  cr29_SxI_F   = list(items = c("cr29_d28_female","cr29_d29_female"),        type = "functional", max_raw = 4)
)

derive_hrqol <- function(df) {
  for (nm in names(c30_domains)) {
    info <- c30_domains[[nm]]
    df[[nm]] <- calc_scale_score(df, info$items, info$type, info$max_raw)
  }
  for (nm in names(cr29_common)) {
    info <- cr29_common[[nm]]
    df[[nm]] <- calc_scale_score(df, info$items, info$type, info$max_raw)
  }
  # 造口分支 / 排便功能分支
  # 注意：cr29_d18_stoma 在 00 模块已经被解码为 1（有造口袋）/ 2（无造口袋）
  sto_items <- intersect(c("cr29_d19","cr29_d20","cr29_d21","cr29_d22",
                           "cr29_d23","cr29_d24","cr29_d25"), names(df))
  def_items <- intersect(c("cr29_d19","cr29_d20","cr29_d21","cr29_d22",
                           "cr29_d23","cr29_d24"), names(df))
  has_stoma <- safe_num(df$cr29_d18_stoma) == 1

  to_100 <- function(items) {
    if (length(items) == 0) return(rep(NA_real_, nrow(df)))
    m <- as.data.frame(lapply(df[, items, drop = FALSE], safe_num))
    raw <- rowMeans(m, na.rm = TRUE); raw[is.nan(raw)] <- NA_real_
    (raw - 1) / 3 * 100
  }
  df$cr29_STO <- ifelse(has_stoma,  to_100(sto_items), NA_real_)
  df$cr29_DEF <- ifelse(!has_stoma, to_100(def_items), NA_real_)
  df
}

# 4. 一键应用到各数据集 ----------------------------------------
df_main     <- df_main     |> derive_cost() |> derive_eq5d() |> derive_hrqol()
df_nonrecur <- df_nonrecur |> derive_cost() |> derive_eq5d() |> derive_hrqol()
df_recur    <- df_recur    |> derive_cost() |> derive_eq5d() |> derive_hrqol()
df_dabexin  <- df_dabexin  |> derive_cost() |> derive_eq5d() |> derive_hrqol()
df_extend   <- df_extend   |> derive_cost() |> derive_eq5d() |> derive_hrqol()

message("[模块01] 费用、EQ-5D、C30、CR29 维度全部计算完毕。")
message(sprintf("  · 主样本 EQ-5D 效用值非NA数: %d / %d",
                sum(!is.na(df_main$eq5d_utility_mult8r)), nrow(df_main)))
message(sprintf("  · 主样本 C30-PF 非NA数: %d / %d",
                sum(!is.na(df_main$c30_PF)), nrow(df_main)))
message(sprintf("  · 主样本 CR29-UI 非NA数: %d / %d",
                sum(!is.na(df_main$cr29_UI)), nrow(df_main)))
