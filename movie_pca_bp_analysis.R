# ================================================================
# 基于 PCA 降维与 BP 神经网络的电影作品评价预测
# 数据设计：MovieLens 1M 评分交互 + TMDB Kaggle 电影运营指标
# 分层方案：上映年份分层（<=1990、1991-2009、>=2010），
#           各年代内部再按用户观影活跃度分层。
# ================================================================

# -------------------------
# 0. 环境准备
# -------------------------
# 如本机尚未安装，请先运行：
# install.packages(c("tidyverse", "neuralnet", "corrplot"))
library(tidyverse)
library(neuralnet)
library(corrplot)

set.seed(20260626)

PROJECT_DIR <- getwd()
OUTPUT_DIR <- file.path(PROJECT_DIR, "outputs_movie_pca_bp")
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# 是否允许通过“片名 + 年份”从 Kaggle IMDB 参考表补 imdb_id。
# 课程设计建议优先使用 MovieLens links.csv 或 TMDB 自带 imdb_id；
# title-year fallback 仅用于本地数据缺桥接字段时的应急补齐，报告中应如实说明。
ALLOW_TITLE_YEAR_IMDB_FALLBACK <- TRUE

# PCA 主成分累计贡献率阈值。达到 85% 后停止取主成分。
PCA_CUMULATIVE_THRESHOLD <- 0.85

# 每个分组至少需要的样本数。样本太少时 BP 神经网络结果不稳定。
MIN_GROUP_N <- 30

# MovieLens 1M 发布较早，严格使用 1M 时几乎无法覆盖 2010 年后电影。
# 为满足“<=1990、1991-2009、>=2010”三段年代分层的完整建模流程，
# 可追加 MovieLens latest-small 作为 post-2010 评分交互补充。
# 报告中需要说明：1M 是主数据源，latest-small 仅用于补足 2010 年后年代层。
USE_LATEST_SMALL_SUPPLEMENT_FOR_POST_2010 <- TRUE

# PCA 使用每组全量样本；BP 神经网络阶段对大分组做固定种子抽样，
# 避免 neuralnet 在课程设计电脑上训练时间过长。
MAX_NN_ROWS_PER_GROUP <- 900

# -------------------------
# 1. 通用函数
# -------------------------
first_existing <- function(paths) {
  for (path in paths) {
    direct_path <- path
    project_path <- file.path(PROJECT_DIR, path)

    if (file.exists(direct_path)) {
      return(normalizePath(direct_path, winslash = "/", mustWork = TRUE))
    }
    if (file.exists(project_path)) {
      return(normalizePath(project_path, winslash = "/", mustWork = TRUE))
    }
  }
  NA_character_
}

auto_find_one <- function(pattern) {
  files <- list.files(
    PROJECT_DIR,
    pattern = pattern,
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(files) == 0) {
    return(NA_character_)
  }

  normalizePath(files[[1]], winslash = "/", mustWork = TRUE)
}

standard_name <- function(x) {
  x_low <- tolower(x)
  case_when(
    x_low %in% c("userid", "user_id", "user") ~ "userId",
    x_low %in% c("movieid", "movie_id", "movie") ~ "movieId",
    x_low %in% c("imdbid", "imdb_id", "imdb") ~ "imdb_id",
    x_low %in% c("tmdbid", "tmdb_id") ~ "tmdbId",
    x_low %in% c("id") ~ "id",
    TRUE ~ x
  )
}

ensure_cols <- function(df, cols) {
  for (col in cols) {
    if (!col %in% names(df)) {
      df[[col]] <- NA
    }
  }
  df
}

as_num <- function(x) {
  suppressWarnings(as.numeric(str_replace_all(as.character(x), "[,$]", "")))
}

normalize_imdb_id <- function(x) {
  x_chr <- as.character(x)
  x_chr <- str_trim(x_chr)
  x_chr[x_chr == "" | x_chr == "NA" | is.na(x_chr)] <- NA_character_

  stripped <- str_replace(x_chr, "^tt", "")
  stripped <- str_replace_all(stripped, "[^0-9]", "")
  out <- if_else(
    is.na(stripped) | stripped == "",
    NA_character_,
    paste0("tt", str_pad(stripped, width = 7, side = "left", pad = "0"))
  )

  out
}

extract_year_from_title <- function(title) {
  suppressWarnings(as.integer(str_extract(title, "(?<=\\()\\d{4}(?=\\))")))
}

parse_year <- function(x) {
  date_value <- suppressWarnings(as.Date(x))
  suppressWarnings(as.integer(format(date_value, "%Y")))
}

clean_title_key <- function(title) {
  title %>%
    as.character() %>%
    str_to_lower() %>%
    str_remove("\\s*\\(\\d{4}\\)\\s*$") %>%
    str_replace_all("&", " and ") %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish()
}

signed_log1p <- function(x) {
  sign(x) * log1p(abs(x))
}

median_or_zero <- function(x) {
  med <- suppressWarnings(median(x, na.rm = TRUE))
  if (is.na(med) || is.nan(med)) {
    return(0)
  }
  med
}

safe_scale_matrix <- function(df, vars) {
  mat <- as.matrix(df[, vars, drop = FALSE])
  center <- colMeans(mat, na.rm = TRUE)
  scale_value <- apply(mat, 2, sd, na.rm = TRUE)
  scale_value[is.na(scale_value) | scale_value == 0] <- 1

  scaled <- sweep(mat, 2, center, "-")
  scaled <- sweep(scaled, 2, scale_value, "/")

  list(x = scaled, center = center, scale = scale_value)
}

impute_by_median <- function(df, vars, medians) {
  for (var in vars) {
    df[[var]][is.na(df[[var]])] <- medians[[var]]
  }
  df
}

format_pc_expression <- function(loadings_df, pc_col, digits = 3) {
  coefs <- loadings_df[[pc_col]]
  terms <- paste0(
    sprintf(paste0("%+.", digits, "f"), coefs),
    "*Z(",
    loadings_df$feature,
    ")"
  )

  paste0(pc_col, " = ", str_remove(paste(terms, collapse = " "), "^\\+"))
}

# -------------------------
# 2. 自动定位数据文件
# -------------------------
# 推荐目录结构：
#   ml-1m/ratings.dat
#   ml-1m/movies.dat
#   ml-1m/users.dat
#   ml-1m/links.csv        # 桥接字段：movieId, imdbId, tmdbId
#   TMDB/tmdb_5000_movies.csv 或 TMDB/movies_metadata.csv

ratings_path <- first_existing(c(
  "ml-1m/ratings.dat",
  "MovieLens 1M/ratings.dat",
  "MovieLens/ratings.dat",
  "ratings.dat",
  "ml-latest-small/ratings.csv",
  "MovieLens/ratings.csv",
  "ratings.csv"
))

movies_path <- first_existing(c(
  "ml-1m/movies.dat",
  "MovieLens 1M/movies.dat",
  "MovieLens/movies.dat",
  "movies.dat",
  "ml-latest-small/movies.csv",
  "MovieLens/movies.csv",
  "movies.csv"
))

users_path <- first_existing(c(
  "ml-1m/users.dat",
  "MovieLens 1M/users.dat",
  "MovieLens/users.dat",
  "users.dat",
  "MovieLens/users.csv",
  "users.csv"
))

links_path <- first_existing(c(
  "ml-1m/links.csv",
  "MovieLens 1M/links.csv",
  "MovieLens/links.csv",
  "ml-latest-small/links.csv",
  "links.csv"
))

latest_small_ratings_path <- first_existing(c(
  "downloads/ml-latest-small/ratings.csv",
  "ml-latest-small/ratings.csv",
  "MovieLens latest small/ratings.csv"
))

latest_small_movies_path <- first_existing(c(
  "downloads/ml-latest-small/movies.csv",
  "ml-latest-small/movies.csv",
  "MovieLens latest small/movies.csv"
))

latest_small_links_path <- first_existing(c(
  "downloads/ml-latest-small/links.csv",
  "ml-latest-small/links.csv",
  "MovieLens latest small/links.csv"
))

tmdb_movies_path <- first_existing(c(
  "TMDB/movies_metadata.csv",
  "movies_metadata.csv",
  "TMDB/tmdb_5000_movies.csv",
  "tmdb_5000_movies.csv"
))

imdb_reference_path <- first_existing(c(
  "Kaggle IMDB/Dataset/final_dataset.csv",
  "imdb/final_dataset.csv",
  "final_dataset.csv"
))

if (is.na(ratings_path)) ratings_path <- auto_find_one("^ratings\\.(dat|csv)$")
if (is.na(movies_path)) movies_path <- auto_find_one("^movies\\.(dat|csv)$")
if (is.na(tmdb_movies_path)) tmdb_movies_path <- auto_find_one("tmdb.*movies.*\\.csv$")

if (is.na(ratings_path) || is.na(movies_path)) {
  stop(
    paste(
      "未找到 MovieLens 评分交互文件。",
      "请将 MovieLens 1M 的 ratings.dat、movies.dat、users.dat 放入 ml-1m/ 目录；",
      "若要严格通过 imdb_id 融合，请额外放入 links.csv（movieId, imdbId, tmdbId）。"
    ),
    call. = FALSE
  )
}

if (is.na(tmdb_movies_path)) {
  stop(
    "未找到 TMDB 电影数据文件。请放入 TMDB/tmdb_5000_movies.csv 或 TMDB/movies_metadata.csv。",
    call. = FALSE
  )
}

# -------------------------
# 3. 读取 MovieLens 多表
# -------------------------
read_movielens_ratings <- function(path) {
  if (str_detect(tolower(path), "\\.dat$")) {
    lines <- readr::read_lines(path, locale = readr::locale(encoding = "ISO-8859-1"))
    parts <- str_split_fixed(lines, "::", 4)

    tibble(
      userId = as.integer(parts[, 1]),
      movieId = as.integer(parts[, 2]),
      rating = as_num(parts[, 3]),
      timestamp = as_num(parts[, 4])
    )
  } else {
    readr::read_csv(path, show_col_types = FALSE) %>%
      rename_with(standard_name) %>%
      mutate(
        userId = as.integer(userId),
        movieId = as.integer(movieId),
        rating = as_num(rating),
        timestamp = as_num(timestamp)
      )
  }
}

read_movielens_movies <- function(path) {
  if (str_detect(tolower(path), "\\.dat$")) {
    lines <- readr::read_lines(path, locale = readr::locale(encoding = "ISO-8859-1"))
    parts <- str_split_fixed(lines, "::", 3)

    tibble(
      movieId = as.integer(parts[, 1]),
      title = parts[, 2],
      genres_ml = parts[, 3]
    )
  } else {
    readr::read_csv(path, show_col_types = FALSE) %>%
      rename_with(standard_name) %>%
      rename(genres_ml = genres) %>%
      mutate(movieId = as.integer(movieId))
  }
}

read_movielens_users <- function(path) {
  if (is.na(path)) {
    return(tibble(userId = integer()))
  }

  if (str_detect(tolower(path), "\\.dat$")) {
    lines <- readr::read_lines(path, locale = readr::locale(encoding = "ISO-8859-1"))
    parts <- str_split_fixed(lines, "::", 5)

    tibble(
      userId = as.integer(parts[, 1]),
      gender = parts[, 2],
      age = as.integer(parts[, 3]),
      occupation = parts[, 4],
      zip_code = parts[, 5]
    )
  } else {
    readr::read_csv(path, show_col_types = FALSE) %>%
      rename_with(standard_name) %>%
      mutate(userId = as.integer(userId))
  }
}

read_movielens_links <- function(path) {
  if (is.na(path)) {
    return(tibble(movieId = integer(), imdb_id = character(), tmdbId = integer()))
  }

  links <- readr::read_csv(path, show_col_types = FALSE) %>%
    rename_with(standard_name) %>%
    ensure_cols(c("movieId", "imdb_id", "tmdbId")) %>%
    mutate(
      movieId = as.integer(movieId),
      imdb_id = normalize_imdb_id(imdb_id),
      tmdbId = as.integer(tmdbId)
    ) %>%
    select(movieId, imdb_id, tmdbId) %>%
    distinct()

  links
}

ratings <- read_movielens_ratings(ratings_path)
movies_ml <- read_movielens_movies(movies_path)
users_ml <- read_movielens_users(users_path)
links_ml <- read_movielens_links(links_path)

ratings <- ratings %>%
  mutate(
    dataset_source = "MovieLens 1M",
    userId_raw = userId,
    movieId_raw = movieId
  )

movies_ml <- movies_ml %>%
  mutate(
    dataset_source = "MovieLens 1M",
    movieId_raw = movieId
  )

links_ml <- links_ml %>%
  mutate(
    dataset_source = "MovieLens 1M",
    movieId_raw = movieId
  )

if (
  USE_LATEST_SMALL_SUPPLEMENT_FOR_POST_2010 &&
    !is.na(latest_small_ratings_path) &&
    !is.na(latest_small_movies_path) &&
    !is.na(latest_small_links_path)
) {
  latest_ratings <- read_movielens_ratings(latest_small_ratings_path) %>%
    mutate(
      dataset_source = "MovieLens latest-small supplement",
      userId_raw = userId,
      movieId_raw = movieId,
      # 加偏移量，避免与 MovieLens 1M 的 userId/movieId 发生键冲突。
      userId = userId + 10000000L,
      movieId = movieId + 10000000L
    )

  latest_movies <- read_movielens_movies(latest_small_movies_path) %>%
    mutate(
      dataset_source = "MovieLens latest-small supplement",
      movieId_raw = movieId,
      movieId = movieId + 10000000L
    )

  latest_links <- read_movielens_links(latest_small_links_path) %>%
    mutate(
      dataset_source = "MovieLens latest-small supplement",
      movieId_raw = movieId,
      movieId = movieId + 10000000L
    )

  ratings <- bind_rows(ratings, latest_ratings)
  movies_ml <- bind_rows(movies_ml, latest_movies)
  links_ml <- bind_rows(links_ml, latest_links)
}

movies_ml <- movies_ml %>%
  mutate(
    movie_year = extract_year_from_title(title),
    title_clean = str_remove(title, "\\s*\\(\\d{4}\\)\\s*$"),
    title_key = clean_title_key(title)
  )

# -------------------------
# 4. 读取 IMDB 参考表，用于补齐 imdb_id
# -------------------------
read_imdb_reference <- function(path) {
  if (is.na(path)) {
    return(tibble(imdb_id = character(), title_key = character(), release_year = integer()))
  }

  imdb_raw <- readr::read_csv(path, show_col_types = FALSE, guess_max = 10000) %>%
    rename_with(standard_name)

  # Kaggle IMDB 当前数据中 id 即 tt 开头的 IMDB 编号。
  if (!"imdb_id" %in% names(imdb_raw) && "id" %in% names(imdb_raw)) {
    imdb_raw <- imdb_raw %>%
      mutate(imdb_id = if_else(str_detect(as.character(id), "^tt"), as.character(id), NA_character_))
  }

  imdb_raw <- imdb_raw %>%
    ensure_cols(c("imdb_id", "title", "release_date")) %>%
    mutate(
      imdb_id = normalize_imdb_id(imdb_id),
      release_year = parse_year(release_date),
      title_key = clean_title_key(title)
    ) %>%
    filter(!is.na(imdb_id), !is.na(title_key), !is.na(release_year)) %>%
    select(imdb_id, title_key, release_year) %>%
    arrange(imdb_id) %>%
    distinct(title_key, release_year, .keep_all = TRUE)

  imdb_raw
}

imdb_reference <- read_imdb_reference(imdb_reference_path)

movies_with_ids <- movies_ml %>%
  left_join(links_ml, by = "movieId") %>%
  mutate(
    dataset_source = coalesce(dataset_source.x, dataset_source.y),
    movieId_raw = coalesce(movieId_raw.x, movieId_raw.y)
  ) %>%
  select(-any_of(c("dataset_source.x", "dataset_source.y", "movieId_raw.x", "movieId_raw.y")))

if (ALLOW_TITLE_YEAR_IMDB_FALLBACK && nrow(imdb_reference) > 0) {
  movies_with_ids <- movies_with_ids %>%
    left_join(
      imdb_reference %>%
        rename(movie_year = release_year, imdb_id_fallback = imdb_id),
      by = c("title_key", "movie_year")
    ) %>%
    mutate(
      imdb_id_source = case_when(
        !is.na(imdb_id) ~ "MovieLens links.csv",
        is.na(imdb_id) & !is.na(imdb_id_fallback) ~ "Kaggle IMDB title-year fallback",
        TRUE ~ NA_character_
      ),
      imdb_id = coalesce(imdb_id, imdb_id_fallback)
    ) %>%
    select(-imdb_id_fallback)
} else {
  movies_with_ids <- movies_with_ids %>%
    mutate(imdb_id_source = if_else(!is.na(imdb_id), "MovieLens links.csv", NA_character_))
}

if (movies_with_ids %>% filter(!is.na(imdb_id)) %>% nrow() == 0) {
  stop(
    paste(
      "MovieLens 电影表未获得 imdb_id。",
      "MovieLens 1M 原始数据不自带 imdb_id；",
      "请提供 links.csv，或启用/提供 Kaggle IMDB 参考表后用片名+年份补齐。"
    ),
    call. = FALSE
  )
}

# -------------------------
# 5. 读取 TMDB Kaggle 电影数据
# -------------------------
read_tmdb_movies <- function(path, links_tbl, imdb_ref) {
  tmdb_raw <- readr::read_csv(path, show_col_types = FALSE, guess_max = 10000) %>%
    rename_with(standard_name) %>%
    ensure_cols(c(
      "id", "imdb_id", "title", "original_title", "release_date", "popularity",
      "budget", "revenue", "runtime", "vote_count", "vote_average", "genres"
    ))

  tmdb <- tmdb_raw %>%
    transmute(
      tmdbId = suppressWarnings(as.integer(id)),
      imdb_id = normalize_imdb_id(imdb_id),
      title_tmdb = coalesce(as.character(title), as.character(original_title)),
      release_date = suppressWarnings(as.Date(release_date)),
      release_year_tmdb = parse_year(release_date),

      # TMDB popularity 是 TMDB 数据集原生字段，属于原生流量热度指标。
      tmdb_popularity_native = as_num(popularity),
      tmdb_budget = as_num(budget),
      tmdb_revenue = as_num(revenue),
      tmdb_runtime = as_num(runtime),
      tmdb_vote_count = as_num(vote_count),
      tmdb_vote_average = as_num(vote_average),
      genres_tmdb = as.character(genres),
      tmdb_genre_count = if_else(
        is.na(genres_tmdb) | genres_tmdb == "",
        NA_real_,
        as_num(str_count(genres_tmdb, "\"name\""))
      ),
      title_key = clean_title_key(title_tmdb)
    )

  # 若使用 tmdb_5000_movies.csv，它通常没有 imdb_id，但 id 是 TMDB id。
  # 可以通过 MovieLens links.csv 的 tmdbId -> imdbId 桥接生成 imdb_id。
  if ("tmdbId" %in% names(links_tbl) && nrow(links_tbl) > 0) {
    tmdb <- tmdb %>%
      left_join(
        links_tbl %>%
          filter(!is.na(tmdbId), !is.na(imdb_id)) %>%
          distinct(tmdbId, imdb_id) %>%
          rename(imdb_id_from_link = imdb_id),
        by = "tmdbId"
      ) %>%
      mutate(imdb_id = coalesce(imdb_id, imdb_id_from_link)) %>%
      select(-imdb_id_from_link)
  }

  # 若仍缺 imdb_id，可用 Kaggle IMDB 参考表通过片名+年份补齐。
  if (ALLOW_TITLE_YEAR_IMDB_FALLBACK && nrow(imdb_ref) > 0) {
    tmdb <- tmdb %>%
      left_join(
        imdb_ref %>%
          rename(release_year_tmdb = release_year, imdb_id_fallback = imdb_id),
        by = c("title_key", "release_year_tmdb")
      ) %>%
      mutate(imdb_id = coalesce(imdb_id, imdb_id_fallback)) %>%
      select(-imdb_id_fallback)
  }

  tmdb %>%
    filter(!is.na(imdb_id)) %>%
    arrange(desc(tmdb_vote_count), desc(tmdb_popularity_native)) %>%
    distinct(imdb_id, .keep_all = TRUE)
}

tmdb_movies <- read_tmdb_movies(tmdb_movies_path, links_ml, imdb_reference)

if (nrow(tmdb_movies) == 0) {
  stop(
    paste(
      "TMDB 数据未获得 imdb_id。",
      "若使用 tmdb_5000_movies.csv，请提供 links.csv 以通过 tmdbId 生成 imdb_id；",
      "若使用 movies_metadata.csv，请确认其中包含 imdb_id 字段。"
    ),
    call. = FALSE
  )
}

# -------------------------
# 6. MovieLens 特征工程：用户活跃度与影片互动指标
# -------------------------
# 注意：以下变量均为从 MovieLens 评分交互中聚合得到的衍生分析指标，
# 并不是 MovieLens 数据文件自带字段。
user_activity <- ratings %>%
  group_by(userId) %>%
  summarise(
    user_rating_count = n(),
    user_mean_rating = mean(rating, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    user_activity_level_global = case_when(
      ntile(user_rating_count, 3) == 1 ~ "low_activity_user",
      ntile(user_rating_count, 3) == 2 ~ "medium_activity_user",
      TRUE ~ "high_activity_user"
    )
  )

ratings_with_activity <- ratings %>%
  left_join(user_activity, by = "userId")

movie_interaction_features <- ratings_with_activity %>%
  group_by(movieId) %>%
  summarise(
    # 预测目标：MovieLens 用户对单部电影的平均评分。
    ml_rating_mean = mean(rating, na.rm = TRUE),
    ml_rating_sd = sd(rating, na.rm = TRUE),
    ml_rating_median = median(rating, na.rm = TRUE),

    # MovieLens 衍生指标：影片总互动量、评分用户数。
    ml_interaction_count = n(),
    ml_user_count = n_distinct(userId),

    # MovieLens 衍生指标：观看该电影的用户活跃度画像。
    ml_user_activity_mean = mean(user_rating_count, na.rm = TRUE),
    ml_user_activity_median = median(user_rating_count, na.rm = TRUE),
    ml_high_activity_user_share = mean(user_activity_level_global == "high_activity_user", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(ml_rating_sd = replace_na(ml_rating_sd, 0))

movielens_movie_level <- movies_with_ids %>%
  inner_join(movie_interaction_features, by = "movieId") %>%
  filter(!is.na(imdb_id))

source_summary <- movielens_movie_level %>%
  count(dataset_source, name = "movie_rows_before_tmdb_merge")

readr::write_csv(source_summary, file.path(OUTPUT_DIR, "movielens_source_summary.csv"))

# -------------------------
# 7. 通过 imdb_id 融合 MovieLens 与 TMDB
# -------------------------
movie_level_raw <- movielens_movie_level %>%
  inner_join(tmdb_movies, by = "imdb_id") %>%
  mutate(
    release_year = coalesce(release_year_tmdb, movie_year),
    title_final = coalesce(title_tmdb, title_clean),
    year_group = case_when(
      release_year <= 1990 ~ "year_le_1990",
      release_year >= 1991 & release_year <= 2009 ~ "year_1991_2009",
      release_year >= 2010 ~ "year_ge_2010",
      TRUE ~ NA_character_
    ),
    year_group_label = recode(
      year_group,
      year_le_1990 = "<=1990",
      year_1991_2009 = "1991-2009",
      year_ge_2010 = ">=2010"
    )
  ) %>%
  filter(!is.na(year_group), !is.na(ml_rating_mean))

if (nrow(movie_level_raw) < MIN_GROUP_N) {
  warning(
    paste(
      "融合后的电影样本量较少，BP 神经网络可能不稳定。",
      "请检查 MovieLens 与 TMDB 的 imdb_id 覆盖率。当前样本量：",
      nrow(movie_level_raw)
    )
  )
}

# -------------------------
# 8. 原生字段与衍生指标说明表
# -------------------------
feature_dictionary <- tribble(
  ~feature, ~source_type, ~definition,
  "tmdb_popularity_native", "TMDB 原生字段", "TMDB 数据集自带 popularity，表示平台热度/流量关注度，不能写成衍生指标。",
  "tmdb_budget", "TMDB 原生字段", "TMDB 数据集自带预算。",
  "tmdb_revenue", "TMDB 原生字段", "TMDB 数据集自带票房/收入。",
  "tmdb_runtime", "TMDB 原生字段", "TMDB 数据集自带片长。",
  "tmdb_vote_count", "TMDB 原生字段", "TMDB 数据集自带评分人数，属于平台反馈规模指标。",
  "release_year", "TMDB/MovieLens 原生日期提取", "由 TMDB release_date 或 MovieLens 标题年份提取。",
  "ml_interaction_count", "MovieLens 衍生分析指标", "由 ratings 评分记录按 movieId 聚合得到的影片总互动量。",
  "ml_user_count", "MovieLens 衍生分析指标", "由 ratings 评分记录按 movieId 聚合得到的去重评分用户数。",
  "ml_user_activity_mean", "MovieLens 衍生分析指标", "先统计每个用户评分次数，再计算观看该片用户的平均活跃度。",
  "ml_high_activity_user_share", "MovieLens 衍生分析指标", "观看该片用户中高活跃用户占比。",
  "tmdb_popularity_log", "工程衍生指标", "对 TMDB 原生 popularity 做 log1p 变换，降低长尾偏态。",
  "tmdb_budget_log", "工程衍生指标", "对 TMDB 原生预算做 log1p 变换。",
  "tmdb_revenue_log", "工程衍生指标", "对 TMDB 原生票房/收入做 log1p 变换。",
  "tmdb_profit_log", "工程衍生指标", "由 revenue - budget 得到利润后做带符号 log1p 变换。",
  "tmdb_roi_clean", "工程衍生指标", "由 revenue / budget 得到投资回报率；预算缺失或为 0 时置为缺失后用中位数填补。",
  "movie_age", "工程衍生指标", "由当前年份 - release_year 得到影片年龄。"
)

readr::write_csv(feature_dictionary, file.path(OUTPUT_DIR, "feature_dictionary.csv"))

# -------------------------
# 9. 预处理：缺失、异常、标准化前特征构造
# -------------------------
add_engineered_features <- function(df) {
  current_year <- as.integer(format(Sys.Date(), "%Y"))

  df %>%
    mutate(
      # TMDB 中 budget/revenue/runtime 为 0 往往表示未知，这里先视作缺失。
      tmdb_budget = na_if(tmdb_budget, 0),
      tmdb_revenue = na_if(tmdb_revenue, 0),
      tmdb_runtime = na_if(tmdb_runtime, 0),
      tmdb_vote_count = replace_na(tmdb_vote_count, 0),
      tmdb_genre_count = replace_na(tmdb_genre_count, 0),

      tmdb_profit = tmdb_revenue - tmdb_budget,
      tmdb_roi_clean = if_else(!is.na(tmdb_budget) & tmdb_budget > 0, tmdb_revenue / tmdb_budget, NA_real_),
      movie_age = current_year - release_year,

      tmdb_popularity_log = log1p(pmax(tmdb_popularity_native, 0)),
      tmdb_budget_log = log1p(pmax(tmdb_budget, 0)),
      tmdb_revenue_log = log1p(pmax(tmdb_revenue, 0)),
      tmdb_profit_log = signed_log1p(tmdb_profit),
      tmdb_vote_count_log = log1p(pmax(tmdb_vote_count, 0)),

      ml_interaction_count_log = log1p(pmax(ml_interaction_count, 0)),
      ml_user_count_log = log1p(pmax(ml_user_count, 0)),
      ml_user_activity_mean_log = log1p(pmax(ml_user_activity_mean, 0))
    )
}

movie_level_engineered <- movie_level_raw %>%
  add_engineered_features()

# PCA 与 BP 神经网络使用的解释变量。
# 其中 tmdb_popularity_native 的建模版本为 tmdb_popularity_log；
# 原字段仍在 feature_dictionary 中标注为 TMDB 原生流量指标。
pca_feature_vars <- c(
  "tmdb_popularity_log",
  "tmdb_budget_log",
  "tmdb_revenue_log",
  "tmdb_profit_log",
  "tmdb_roi_clean",
  "tmdb_runtime",
  "tmdb_vote_count_log",
  "movie_age",
  "tmdb_genre_count",
  "ml_interaction_count_log",
  "ml_user_count_log",
  "ml_user_activity_mean_log",
  "ml_high_activity_user_share",
  "ml_rating_sd"
)

target_var <- "ml_rating_mean"

feature_medians <- map_dbl(
  set_names(pca_feature_vars),
  ~ median_or_zero(movie_level_engineered[[.x]])
)

analysis_imputed <- movie_level_engineered %>%
  impute_by_median(pca_feature_vars, feature_medians)

# 3 sigma 异常值剔除：对建模特征和目标变量逐列计算 mean +/- 3*sd。
sigma_vars <- c(pca_feature_vars, target_var)

sigma_stats <- map_dfr(sigma_vars, function(var) {
  x <- analysis_imputed[[var]]
  tibble(
    variable = var,
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    lower = mean - 3 * sd,
    upper = mean + 3 * sd
  )
})

keep_row <- rep(TRUE, nrow(analysis_imputed))
for (i in seq_len(nrow(sigma_stats))) {
  var <- sigma_stats$variable[[i]]
  sd_value <- sigma_stats$sd[[i]]
  if (!is.na(sd_value) && sd_value > 0) {
    keep_row <- keep_row &
      analysis_imputed[[var]] >= sigma_stats$lower[[i]] &
      analysis_imputed[[var]] <= sigma_stats$upper[[i]]
  }
}

analysis_clean <- analysis_imputed[keep_row, ] %>%
  group_by(year_group) %>%
  mutate(
    # 双重分层的第二层：在各年代内部，按观看该片用户的平均活跃度做三分位分组。
    user_activity_tile_in_year = ntile(ml_user_activity_mean, 3),
    user_activity_stratum = case_when(
      user_activity_tile_in_year == 1 ~ "low_activity_viewer_movies",
      user_activity_tile_in_year == 2 ~ "medium_activity_viewer_movies",
      TRUE ~ "high_activity_viewer_movies"
    )
  ) %>%
  ungroup()

row_count_log <- tibble(
  stage = c("after_imdb_merge", "after_missing_imputation", "after_3sigma_filter"),
  rows = c(nrow(movie_level_raw), nrow(analysis_imputed), nrow(analysis_clean))
)

readr::write_csv(row_count_log, file.path(OUTPUT_DIR, "preprocess_row_count.csv"))
readr::write_csv(sigma_stats, file.path(OUTPUT_DIR, "three_sigma_thresholds.csv"))
readr::write_csv(analysis_clean, file.path(OUTPUT_DIR, "merged_movie_level_dataset.csv"))

strata_summary <- analysis_clean %>%
  count(year_group_label, user_activity_stratum, name = "movie_count") %>%
  group_by(year_group_label) %>%
  mutate(group_share = movie_count / sum(movie_count)) %>%
  ungroup()

readr::write_csv(strata_summary, file.path(OUTPUT_DIR, "double_strata_summary.csv"))

# -------------------------
# 10. 相关性可视化
# -------------------------
if (nrow(analysis_clean) >= 3) {
  corr_matrix <- cor(
    analysis_clean[, pca_feature_vars],
    use = "pairwise.complete.obs"
  )

  png(
    filename = file.path(OUTPUT_DIR, "correlation_heatmap.png"),
    width = 1600,
    height = 1300,
    res = 150
  )
  corrplot(
    corr_matrix,
    method = "color",
    type = "upper",
    tl.col = "black",
    tl.cex = 0.72,
    number.cex = 0.58,
    addCoef.col = "black"
  )
  dev.off()
}

# -------------------------
# 11. PCA + BP 神经网络建模函数
# -------------------------
run_group_analysis <- function(df, group_name) {
  if (nrow(df) < MIN_GROUP_N) {
    warning(paste0(group_name, " 样本量小于 ", MIN_GROUP_N, "，跳过 BP 神经网络。"))
    return(NULL)
  }

  scaled <- safe_scale_matrix(df, pca_feature_vars)

  pca_fit <- prcomp(scaled$x, center = FALSE, scale. = FALSE)
  variance_ratio <- pca_fit$sdev^2 / sum(pca_fit$sdev^2)
  cumulative_ratio <- cumsum(variance_ratio)
  n_components <- which(cumulative_ratio >= PCA_CUMULATIVE_THRESHOLD)[1]

  if (is.na(n_components)) {
    n_components <- length(variance_ratio)
  }

  # 控制 BP 输入维度，避免小样本分组中过拟合。
  n_components <- min(
    max(n_components, 2),
    8,
    ncol(scaled$x),
    nrow(df) - 1
  )

  pc_cols <- paste0("PC", seq_len(n_components))

  variance_df <- tibble(
    group = group_name,
    component = paste0("PC", seq_along(variance_ratio)),
    variance_ratio = variance_ratio,
    cumulative_ratio = cumulative_ratio,
    selected_for_bp = seq_along(variance_ratio) <= n_components
  )

  loadings_df <- as.data.frame(pca_fit$rotation[, seq_len(n_components), drop = FALSE]) %>%
    rownames_to_column("feature")

  readr::write_csv(
    variance_df,
    file.path(OUTPUT_DIR, paste0("pca_variance_", group_name, ".csv"))
  )
  readr::write_csv(
    loadings_df,
    file.path(OUTPUT_DIR, paste0("pca_loadings_", group_name, ".csv"))
  )

  scree_plot <- ggplot(variance_df, aes(x = seq_along(variance_ratio), y = variance_ratio)) +
    geom_col(fill = "#2F6F73", width = 0.72) +
    geom_line(aes(y = cumulative_ratio), color = "#C4492D", linewidth = 0.9) +
    geom_point(aes(y = cumulative_ratio), color = "#C4492D", size = 2) +
    scale_x_continuous(breaks = seq_along(variance_ratio)) +
    labs(
      title = paste0("Scree Plot - ", group_name),
      x = "Principal Component",
      y = "Variance / Cumulative Variance"
    ) +
    theme_minimal(base_size = 12)

  ggsave(
    filename = file.path(OUTPUT_DIR, paste0("scree_", group_name, ".png")),
    plot = scree_plot,
    width = 8,
    height = 5,
    dpi = 150
  )

  expressions <- map_chr(pc_cols, ~ format_pc_expression(loadings_df, .x))

  pca_scores <- as_tibble(pca_fit$x[, seq_len(n_components), drop = FALSE])
  names(pca_scores) <- pc_cols

  model_df <- bind_cols(
    pca_scores,
    tibble(ml_rating_mean = df[[target_var]])
  )

  if (nrow(model_df) > MAX_NN_ROWS_PER_GROUP) {
    nn_index <- sample(seq_len(nrow(model_df)), size = MAX_NN_ROWS_PER_GROUP)
    model_df <- model_df[nn_index, ]
  }

  train_index <- sample(
    seq_len(nrow(model_df)),
    size = floor(0.7 * nrow(model_df))
  )
  train_df <- model_df[train_index, ]
  test_df <- model_df[-train_index, ]

  y_min <- min(train_df$ml_rating_mean, na.rm = TRUE)
  y_max <- max(train_df$ml_rating_mean, na.rm = TRUE)
  y_range <- y_max - y_min

  if (y_range == 0) {
    y_range <- 1
  }

  train_df <- train_df %>%
    mutate(rating_scaled = (ml_rating_mean - y_min) / y_range)

  test_df <- test_df %>%
    mutate(rating_scaled = (ml_rating_mean - y_min) / y_range)

  nn_formula <- as.formula(paste("rating_scaled ~", paste(pc_cols, collapse = " + ")))
  hidden_units <- max(3, min(8, ceiling((length(pc_cols) + 1) / 2)))

  nn_fit <- tryCatch(
    neuralnet(
      nn_formula,
      data = train_df[, c("rating_scaled", pc_cols)],
      hidden = hidden_units,
      linear.output = TRUE,
      threshold = 0.01,
      stepmax = 1e6,
      lifesign = "none"
    ),
    error = function(e) {
      warning(paste("BP 神经网络训练失败：", group_name, e$message))
      NULL
    }
  )

  if (is.null(nn_fit)) {
    mse_row <- tibble(
      group = group_name,
      sample_n = nrow(df),
      nn_sample_n = nrow(model_df),
      selected_pc_n = n_components,
      hidden_units = hidden_units,
      train_mse = NA_real_,
      test_mse = NA_real_
    )
  } else {
    train_pred_scaled <- as.numeric(
      neuralnet::compute(nn_fit, train_df[, pc_cols, drop = FALSE])$net.result
    )
    test_pred_scaled <- as.numeric(
      neuralnet::compute(nn_fit, test_df[, pc_cols, drop = FALSE])$net.result
    )

    train_pred <- train_pred_scaled * y_range + y_min
    test_pred <- test_pred_scaled * y_range + y_min

    mse_row <- tibble(
      group = group_name,
      sample_n = nrow(df),
      nn_sample_n = nrow(model_df),
      selected_pc_n = n_components,
      hidden_units = hidden_units,
      train_mse = mean((train_pred - train_df$ml_rating_mean)^2, na.rm = TRUE),
      test_mse = mean((test_pred - test_df$ml_rating_mean)^2, na.rm = TRUE)
    )
  }

  list(
    group = group_name,
    pca = pca_fit,
    feature_center = scaled$center,
    feature_scale = scaled$scale,
    n_components = n_components,
    pc_cols = pc_cols,
    variance = variance_df,
    loadings = loadings_df,
    expressions = expressions,
    nn = nn_fit,
    y_min = y_min,
    y_range = y_range,
    mse = mse_row
  )
}

# 全局数据集 + 三组年代子集分别执行 PCA 和 BP。
analysis_groups <- list(
  global = analysis_clean,
  year_le_1990 = analysis_clean %>% filter(year_group == "year_le_1990"),
  year_1991_2009 = analysis_clean %>% filter(year_group == "year_1991_2009"),
  year_ge_2010 = analysis_clean %>% filter(year_group == "year_ge_2010")
)

group_results <- imap(analysis_groups, run_group_analysis)
group_results <- group_results[!map_lgl(group_results, is.null)]

pca_variance_all <- map_dfr(group_results, "variance")
pca_loadings_all <- imap_dfr(group_results, function(result, group_name) {
  result$loadings %>% mutate(group = group_name, .before = 1)
})
bp_mse_all <- map_dfr(group_results, "mse")

readr::write_csv(pca_variance_all, file.path(OUTPUT_DIR, "pca_variance_all_groups.csv"))
readr::write_csv(pca_loadings_all, file.path(OUTPUT_DIR, "pca_loadings_all_groups.csv"))
readr::write_csv(bp_mse_all, file.path(OUTPUT_DIR, "bp_mse_comparison.csv"))

pc_expression_lines <- imap(group_results, function(result, group_name) {
  c(
    paste0("【", group_name, "】"),
    result$expressions,
    ""
  )
}) %>%
  flatten_chr()

readr::write_lines(
  pc_expression_lines,
  file.path(OUTPUT_DIR, "pca_component_expressions.txt")
)

# -------------------------
# 12. 新样本分年代差异化预测
# -------------------------
# 两条新样本仅用于课程设计演示。真实应用时应替换为业务平台实时/历史特征。
# 注意：新电影尚未积累 MovieLens 互动时，ml_* 衍生指标需要由平台早期互动、
# 预热视频点击、收藏、想看等先验指标近似映射；这里用示例值演示流程。
new_samples_raw <- tribble(
  ~sample_name, ~scenario, ~release_year, ~tmdb_popularity_native, ~tmdb_budget, ~tmdb_revenue, ~tmdb_runtime, ~tmdb_vote_count, ~tmdb_genre_count, ~ml_interaction_count, ~ml_user_count, ~ml_user_activity_mean, ~ml_high_activity_user_share, ~ml_rating_sd,
  "streaming_new_movie", "流媒体新电影", 2024, 85, 25000000, 90000000, 115, 1800, 3, 1200, 1100, 360, 0.48, 0.82,
  "classic_old_movie", "经典老电影", 1975, 28, 3000000, 45000000, 125, 900, 2, 5000, 4300, 220, 0.34, 0.68
) %>%
  mutate(
    year_group = case_when(
      release_year <= 1990 ~ "year_le_1990",
      release_year >= 1991 & release_year <= 2009 ~ "year_1991_2009",
      release_year >= 2010 ~ "year_ge_2010"
    )
  ) %>%
  add_engineered_features()

final_feature_medians <- map_dbl(
  set_names(pca_feature_vars),
  ~ median_or_zero(analysis_clean[[.x]])
)

new_samples <- new_samples_raw %>%
  impute_by_median(pca_feature_vars, final_feature_medians)

predict_with_result <- function(sample_df, result) {
  if (is.null(result$nn)) {
    return(NA_real_)
  }

  mat <- as.matrix(sample_df[, pca_feature_vars, drop = FALSE])
  mat_scaled <- sweep(mat, 2, result$feature_center, "-")
  mat_scaled <- sweep(mat_scaled, 2, result$feature_scale, "/")

  pc_scores <- mat_scaled %*% result$pca$rotation[, seq_len(result$n_components), drop = FALSE]
  pc_scores <- as_tibble(pc_scores)
  names(pc_scores) <- result$pc_cols

  pred_scaled <- as.numeric(neuralnet::compute(result$nn, pc_scores)$net.result)
  pred <- pred_scaled * result$y_range + result$y_min

  # MovieLens 评分范围为 1-5，这里做边界裁剪。
  pmin(pmax(pred, 1), 5)
}

new_sample_predictions <- map_dfr(seq_len(nrow(new_samples)), function(i) {
  sample_row <- new_samples[i, ]
  model_keys <- c("global", sample_row$year_group)

  map_dfr(model_keys, function(model_key) {
    result <- group_results[[model_key]]
    tibble(
      sample_name = sample_row$sample_name,
      scenario = sample_row$scenario,
      release_year = sample_row$release_year,
      prediction_model = model_key,
      predicted_movielens_rating = if (is.null(result)) NA_real_ else predict_with_result(sample_row, result)
    )
  })
})

readr::write_csv(
  new_sample_predictions,
  file.path(OUTPUT_DIR, "new_sample_predictions.csv")
)

# -------------------------
# 13. 输出运行摘要
# -------------------------
run_summary <- tibble(
  item = c(
    "ratings_path",
    "movies_path",
    "users_path",
    "links_path",
    "use_latest_small_supplement_for_post_2010",
    "max_nn_rows_per_group",
    "latest_small_ratings_path",
    "latest_small_movies_path",
    "latest_small_links_path",
    "tmdb_movies_path",
    "imdb_reference_path",
    "merged_rows",
    "clean_rows",
    "output_dir"
  ),
  value = c(
    ratings_path,
    movies_path,
    users_path,
    links_path,
    as.character(USE_LATEST_SMALL_SUPPLEMENT_FOR_POST_2010),
    as.character(MAX_NN_ROWS_PER_GROUP),
    latest_small_ratings_path,
    latest_small_movies_path,
    latest_small_links_path,
    tmdb_movies_path,
    imdb_reference_path,
    as.character(nrow(movie_level_raw)),
    as.character(nrow(analysis_clean)),
    OUTPUT_DIR
  )
)

readr::write_csv(run_summary, file.path(OUTPUT_DIR, "run_summary.csv"))

message("分析完成。输出目录：", OUTPUT_DIR)
message("重点查看：bp_mse_comparison.csv、pca_variance_all_groups.csv、pca_component_expressions.txt、new_sample_predictions.csv")
