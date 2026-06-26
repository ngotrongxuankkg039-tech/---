# ================================================================
# 将电影PCA-BP项目输出结果中文化
# 说明：保留 outputs_movie_pca_bp 原始结果不动，另存到 outputs_movie_pca_bp_cn。
# ================================================================

library(tidyverse)
library(corrplot)

if (!exists("PROJECT_DIR", inherits = TRUE)) {
  PROJECT_DIR <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

input_dir <- file.path(PROJECT_DIR, "outputs_movie_pca_bp")
output_dir <- file.path(PROJECT_DIR, "outputs_movie_pca_bp_cn")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# 清理旧版编号输出，避免业务分组PCA改版后出现两套结果文件。
stale_output_files <- c(
  "06_BP神经网络MSE对比.csv",
  "07_新样本预测结果.csv",
  "08_指标来源说明.csv",
  "09_MovieLens数据来源摘要.csv",
  "10_PCA主成分表达式.txt",
  "11_相关性热力图.png",
  "12_全局样本PCA碎石图.png",
  "13_1990年及以前PCA碎石图.png",
  "14_1991-2009年PCA碎石图.png",
  "15_2010年及以后PCA碎石图.png"
)
unlink(file.path(output_dir, stale_output_files), force = TRUE)

group_map <- c(
  global = "全局样本",
  year_le_1990 = "1990年及以前",
  year_1991_2009 = "1991-2009年",
  year_ge_2010 = "2010年及以后"
)

business_group_map <- c(
  traffic_heat = "流量热度",
  commercial_operation = "商业运营",
  content_attribute = "内容属性",
  audience_preference = "受众偏好"
)

year_label_map <- c(
  "<=1990" = "1990年及以前",
  "1991-2009" = "1991-2009年",
  ">=2010" = "2010年及以后"
)

activity_map <- c(
  low_activity_viewer_movies = "低活跃用户层",
  medium_activity_viewer_movies = "中活跃用户层",
  high_activity_viewer_movies = "高活跃用户层"
)

feature_map <- c(
  tmdb_popularity_native = "TMDB原生热度",
  tmdb_popularity_log = "TMDB热度对数",
  tmdb_budget = "TMDB预算",
  tmdb_budget_log = "TMDB预算对数",
  tmdb_revenue = "TMDB票房收入",
  tmdb_revenue_log = "TMDB票房收入对数",
  tmdb_profit_log = "TMDB利润对数",
  tmdb_roi_clean = "投资回报率",
  tmdb_runtime = "片长",
  tmdb_vote_count = "TMDB评分人数",
  tmdb_vote_count_log = "TMDB评分人数对数",
  tmdb_genre_count = "类型数量",
  release_year = "上映年份",
  movie_age = "影片年龄",
  ml_interaction_count = "影片总互动量",
  ml_interaction_count_log = "影片总互动量对数",
  ml_user_count = "评分用户数",
  ml_user_count_log = "评分用户数对数",
  ml_user_activity_mean = "观看用户平均活跃度",
  ml_user_activity_mean_log = "观看用户平均活跃度对数",
  ml_high_activity_user_share = "高活跃用户占比",
  ml_rating_sd = "评分标准差",
  ml_rating_mean = "电影平均评分"
)

source_type_map <- c(
  "TMDB 原生字段" = "TMDB原生字段",
  "MovieLens 衍生分析指标" = "MovieLens衍生分析指标",
  "工程衍生指标" = "工程衍生指标",
  "TMDB/MovieLens 原生日期提取" = "TMDB/MovieLens原生日期提取"
)

read_csv_in <- function(name) {
  readr::read_csv(file.path(input_dir, name), show_col_types = FALSE)
}

write_csv_out <- function(df, name) {
  readr::write_excel_csv(df, file.path(output_dir, name))
}

recode_group <- function(x) recode(x, !!!group_map, .default = x)
recode_business_group <- function(x) recode(x, !!!business_group_map, .default = x)
recode_feature <- function(x) recode(x, !!!feature_map, .default = x)

# 1. 运行摘要
run_summary <- read_csv_in("run_summary.csv") %>%
  mutate(
    item = recode(
      item,
      ratings_path = "MovieLens评分文件路径",
      movies_path = "MovieLens电影文件路径",
      users_path = "MovieLens用户文件路径",
      links_path = "MovieLens链接表路径",
      use_latest_small_supplement_for_post_2010 = "是否补充latest-small用于2010后样本",
      max_nn_rows_per_group = "BP每组最大建模样本量",
      latest_small_ratings_path = "latest-small评分文件路径",
      latest_small_movies_path = "latest-small电影文件路径",
      latest_small_links_path = "latest-small链接表路径",
      tmdb_movies_path = "TMDB电影文件路径",
      imdb_reference_path = "IMDB参考表路径",
      merged_rows = "融合后样本量",
      clean_rows = "清洗后样本量",
      output_dir = "原始输出目录",
      .default = item
    ),
    value = case_when(
      value == "TRUE" ~ "是",
      value == "FALSE" ~ "否",
      TRUE ~ value
    )
  ) %>%
  rename("项目" = item, "取值" = value)
write_csv_out(run_summary, "01_运行摘要.csv")

# 2. 预处理样本量
preprocess <- read_csv_in("preprocess_row_count.csv") %>%
  mutate(
    stage = recode(
      stage,
      after_imdb_merge = "imdb_id融合后",
      after_missing_imputation = "缺失值填补后",
      after_3sigma_filter = "3σ异常值剔除后",
      .default = stage
    )
  ) %>%
  rename("处理阶段" = stage, "样本量" = rows)
write_csv_out(preprocess, "02_预处理样本量.csv")

# 3. 双重分层样本分布
strata <- read_csv_in("double_strata_summary.csv") %>%
  mutate(
    year_group_label = recode(year_group_label, !!!year_label_map, .default = year_group_label),
    user_activity_stratum = recode(user_activity_stratum, !!!activity_map, .default = user_activity_stratum),
    group_share = round(group_share, 4)
  ) %>%
  rename(
    "年代分组" = year_group_label,
    "用户活跃度层" = user_activity_stratum,
    "电影数量" = movie_count,
    "组内占比" = group_share
  )
write_csv_out(strata, "03_双重分层样本分布.csv")

# 4. PCA方差贡献率
pca_variance <- read_csv_in("pca_variance_all_groups.csv") %>%
  mutate(
    group = recode_group(group),
    business_group = recode_business_group(business_group),
    variance_ratio = round(variance_ratio, 4),
    cumulative_ratio = round(cumulative_ratio, 4),
    selected_for_bp = if_else(selected_for_bp, "是", "否")
  ) %>%
  rename(
    "分组" = group,
    "业务组别" = business_group,
    "主成分" = component,
    "组内主成分" = local_component,
    "方差贡献率" = variance_ratio,
    "累计贡献率" = cumulative_ratio,
    "是否用于BP建模" = selected_for_bp
  )
write_csv_out(pca_variance, "04_PCA方差贡献率.csv")

# 5. PCA载荷矩阵
pca_loadings <- read_csv_in("pca_loadings_all_groups.csv") %>%
  mutate(
    group = recode_group(group),
    business_group = recode_business_group(business_group),
    feature = recode_feature(feature)
  ) %>%
  rename(
    "分组" = group,
    "业务组别" = business_group,
    "主成分" = component,
    "组内主成分" = local_component,
    "变量" = feature,
    "载荷系数" = loading
  )
write_csv_out(pca_loadings, "05_PCA主成分载荷矩阵.csv")

# 6. PCA业务组主成分汇总
pca_business_summary <- read_csv_in("pca_business_group_summary.csv") %>%
  mutate(
    group = recode_group(group),
    business_group = recode_business_group(business_group),
    selected_variance_sum = round(selected_variance_sum, 4),
    max_selected_cumulative_ratio = round(max_selected_cumulative_ratio, 4),
    first_pc_variance_ratio = round(first_pc_variance_ratio, 4)
  ) %>%
  rename(
    "分组" = group,
    "业务组别" = business_group,
    "选取主成分数" = selected_pc_n,
    "所选组内方差贡献率合计" = selected_variance_sum,
    "所选组内累计贡献率" = max_selected_cumulative_ratio,
    "组内第一主成分贡献率" = first_pc_variance_ratio
  )
write_csv_out(pca_business_summary, "06_PCA业务组主成分汇总.csv")

# 7. BP模型MSE
bp_mse <- read_csv_in("bp_mse_comparison.csv") %>%
  mutate(
    group = recode_group(group),
    train_mse = round(train_mse, 4),
    test_mse = round(test_mse, 4)
  ) %>%
  rename(
    "分组" = group,
    "样本量" = sample_n,
    "BP建模样本量" = nn_sample_n,
    "选取主成分数" = selected_pc_n,
    "隐藏层节点数" = hidden_units,
    "训练集MSE" = train_mse,
    "测试集MSE" = test_mse
  )
write_csv_out(bp_mse, "07_BP神经网络MSE对比.csv")

# 8. 新样本预测
new_pred <- read_csv_in("new_sample_predictions.csv") %>%
  mutate(
    sample_name = recode(
      sample_name,
      streaming_new_movie = "流媒体新电影",
      classic_old_movie = "经典老电影",
      .default = sample_name
    ),
    prediction_model = recode_group(prediction_model),
    predicted_movielens_rating = round(predicted_movielens_rating, 4)
  ) %>%
  rename(
    "样本名称" = sample_name,
    "场景" = scenario,
    "上映年份" = release_year,
    "预测模型" = prediction_model,
    "预测MovieLens评分" = predicted_movielens_rating
  )
write_csv_out(new_pred, "08_新样本预测结果.csv")

# 9. 指标字典
feature_dict <- read_csv_in("feature_dictionary.csv") %>%
  mutate(
    feature = recode_feature(feature),
    business_group = recode_business_group(business_group),
    source_type = recode(source_type, !!!source_type_map, .default = source_type)
  ) %>%
  rename(
    "指标名称" = feature,
    "业务组别" = business_group,
    "指标来源类型" = source_type,
    "指标说明" = definition
  )
write_csv_out(feature_dict, "09_指标来源说明.csv")

# 10. 建模业务分组
business_features <- read_csv_in("business_feature_groups.csv") %>%
  mutate(
    business_group = recode_business_group(business_group),
    feature = recode_feature(feature)
  ) %>%
  rename("业务组别" = business_group, "建模变量" = feature)
write_csv_out(business_features, "10_业务分组变量清单.csv")

# 11. MovieLens来源摘要
source_summary <- read_csv_in("movielens_source_summary.csv") %>%
  mutate(
    dataset_source = recode(
      dataset_source,
      "MovieLens 1M" = "MovieLens 1M主数据源",
      "MovieLens latest-small supplement" = "MovieLens latest-small补充数据源",
      .default = dataset_source
    )
  ) %>%
  rename("数据来源" = dataset_source, "TMDB融合前电影行数" = movie_rows_before_tmdb_merge)
write_csv_out(source_summary, "11_MovieLens数据来源摘要.csv")

# 12. 中文主成分表达式
expr_lines <- readr::read_lines(file.path(input_dir, "pca_component_expressions.txt"))
for (nm in names(group_map)) {
  expr_lines <- str_replace_all(expr_lines, fixed(nm), group_map[[nm]])
}
for (nm in names(business_group_map)) {
  expr_lines <- str_replace_all(expr_lines, fixed(nm), business_group_map[[nm]])
}
for (nm in names(feature_map)[order(nchar(names(feature_map)), decreasing = TRUE)]) {
  expr_lines <- str_replace_all(expr_lines, fixed(nm), feature_map[[nm]])
}
readr::write_lines(expr_lines, file.path(output_dir, "12_PCA主成分表达式.txt"))

# 13. 中文汇总说明
summary_lines <- c(
  "电影评价预测项目中文结果汇总",
  "",
  "1. 样本处理结果：融合后得到4631部电影，经过缺失值填补和3σ异常值剔除后保留4204部电影。",
  "2. 双重分层结果：1990年及以前640部，1991-2009年2810部，2010年及以后754部；各年代内部按用户活跃度近似三等分。",
  "3. PCA结果：采用业务分组约束，原始变量先划分为流量热度、商业运营、内容属性、受众偏好四组，各组内部单独提取主成分后再输入BP神经网络。",
  "4. 建模约束说明：流量热度组独立形成traffic_PC类主成分，影片年龄仅位于内容属性组，避免仅由上映年份或影片年龄单一变量主导预测。",
  "5. 指标口径说明：TMDB popularity为TMDB原生流量热度指标；用户活跃度、影片总互动量和评分用户数均为MovieLens评分交互衍生指标。"
)
readr::write_lines(summary_lines, file.path(output_dir, "00_中文结果汇总说明.txt"))

# 14. 重新生成中文图片，而不是只复制英文图片
if (.Platform$OS.type == "windows") {
  grDevices::windowsFonts(
    SimSun = grDevices::windowsFont("SimSun"),
    MicrosoftYaHei = grDevices::windowsFont("Microsoft YaHei")
  )
  plot_family <- "MicrosoftYaHei"
} else {
  plot_family <- ""
}

plot_feature_map <- c(
  tmdb_popularity_log = "TMDB热度",
  tmdb_vote_count_log = "TMDB评分数",
  ml_interaction_count_log = "影片互动量",
  ml_user_count_log = "评分用户数",
  tmdb_budget_log = "预算",
  tmdb_revenue_log = "票房",
  tmdb_profit_log = "利润",
  tmdb_roi_clean = "投资回报率",
  tmdb_runtime = "片长",
  tmdb_genre_count = "类型数",
  movie_age = "影片年龄",
  ml_user_activity_mean_log = "用户活跃度",
  ml_high_activity_user_share = "高活跃占比",
  ml_rating_sd = "评分波动"
)

pca_feature_vars <- names(plot_feature_map)
merged_data <- read_csv_in("merged_movie_level_dataset.csv")

corr_matrix <- cor(
  merged_data[, pca_feature_vars],
  use = "pairwise.complete.obs"
)
colnames(corr_matrix) <- unname(plot_feature_map[colnames(corr_matrix)])
rownames(corr_matrix) <- unname(plot_feature_map[rownames(corr_matrix)])

png(
  filename = file.path(output_dir, "13_相关性热力图.png"),
  width = 1800,
  height = 1500,
  res = 160,
  family = plot_family
)
par(family = plot_family)
corrplot(
  corr_matrix,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.cex = 0.78,
  number.cex = 0.55,
  addCoef.col = "black",
  title = "电影建模特征相关性热力图",
  mar = c(0, 0, 2, 0)
)
dev.off()

make_scree_plot <- function(group_code, title_cn, output_name) {
  plot_df <- read_csv_in("pca_variance_all_groups.csv") %>%
    filter(group == group_code) %>%
    mutate(
      business_group_cn = recode_business_group(business_group),
      component_index = row_number(),
      component_label = local_component,
      selected_label = if_else(selected_for_bp, "用于BP建模", "未选入BP")
    )

  p <- ggplot(plot_df, aes(x = local_component)) +
    geom_col(
      aes(y = variance_ratio, fill = "单个主成分方差贡献率"),
      width = 0.72,
      alpha = 0.9
    ) +
    geom_line(
      aes(y = cumulative_ratio, color = "组内累计方差贡献率", group = business_group_cn),
      linewidth = 1,
    ) +
    geom_point(
      aes(y = cumulative_ratio, color = "组内累计方差贡献率"),
      size = 2
    ) +
    facet_wrap(~ business_group_cn, scales = "free_x") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_manual(values = c("单个主成分方差贡献率" = "#2F6F73")) +
    scale_color_manual(values = c("组内累计方差贡献率" = "#C4492D")) +
    labs(
      title = paste0(title_cn, "业务分组约束PCA碎石图"),
      x = "组内主成分",
      y = "组内方差贡献率 / 累计贡献率",
      fill = "",
      color = ""
    ) +
    theme_minimal(base_size = 13, base_family = plot_family) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 0, hjust = 0.5)
    )

  ggsave(
    filename = file.path(output_dir, output_name),
    plot = p,
    width = 10,
    height = 6,
    dpi = 160
  )
}

make_scree_plot("global", "全局样本", "14_全局样本业务分组PCA碎石图.png")
make_scree_plot("year_le_1990", "1990年及以前", "15_1990年及以前业务分组PCA碎石图.png")
make_scree_plot("year_1991_2009", "1991-2009年", "16_1991-2009年业务分组PCA碎石图.png")
make_scree_plot("year_ge_2010", "2010年及以后", "17_2010年及以后业务分组PCA碎石图.png")

message("中文结果已输出到：", normalizePath(output_dir, winslash = "/"))

