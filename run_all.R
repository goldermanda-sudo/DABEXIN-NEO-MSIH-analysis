# ==============================================================
# 一键运行入口 —— 新版（单臂 + 探索性达伯欣 + Protocol）
# ==============================================================
# 使用方法：
#   1) 将本压缩包内所有 .R 文件解压到同一文件夹
#   2) 在 RStudio 中：
#      Session → Set Working Directory → Choose Directory → 选这个文件夹
#   3) 打开 00_setup_and_clean.R，把 input_path / output_path
#      改成你本地的"数据"与"输出"文件夹
#   4) 在 R 控制台运行：
#        source("run_all.R")
# ==============================================================

source("00_setup_and_clean.R")
source("01_derive_cost_and_hrqol.R")
source("02_table1_baseline.R")
source("03_econ_and_hrqol_descriptive.R")
source("04_internal_subgroup.R")
source("05_recurrence_subgroup.R")
source("06_convergent_validity.R")
source("07_che_logistic.R")
source("08_dabexin_case_series.R")
source("09_sensitivity.R")

message("==============================================")
message("全部模块运行完毕。")
message("请在 output_path 文件夹下查看以下产物：")
message("  · Table3-1_基线特征.docx")
message("  · Table3-2_经济负担现状.docx")
message("  · Table3-3_HRQoL现状.docx")
message("  · Table3-4_内部分组相关因素.docx")
message("  · Table3-5_转移复发亚组.docx")
message("  · Table3-6_聚合效度.docx")
message("  · Figure3-2_聚合效度热力图.png")
message("  · Table3-7_CHE风险因素.docx")
message("  · Table3-8_达伯欣个案系列.docx")
message("  · Supplementary_S1-S6_敏感性分析.docx")
message("==============================================")
