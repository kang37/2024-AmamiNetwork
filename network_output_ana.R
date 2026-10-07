# Node index ----
# 加载包。
library(stringr)
library(readxl)
library(scatterpie)
library(ggsci)
library(patchwork)

# 获取所有csv文件路径。
file_paths <- list.files(
  path = "data_raw/gephi_output",
  full.names = TRUE
)

# 定义解析函数
parse_file_info <- function(file_path) {
  # 提取文件名：去除路径和扩展名。
  file_name <- tools::file_path_sans_ext(basename(file_path))
  file_name <- gsub("gephi_node_export_", "", file_name)
  # 使用正则表达式提取季节和群体。
  matches <- str_match(file_name, "(\\w+)_(\\d+)")
  # 返回解析结果。
  tibble(
    season = as.integer(matches[1, 3]),
    vis_src = matches[1, 2],
    file_path = file_path
  )
}

# 解析所有文件信息。
file_info <- map_dfr(file_paths, parse_file_info)

# 读取并合并所有CSV文件。
combined_data <- pmap(
  list(file_info$file_path, file_info$vis_src, file_info$season),
  function(x, y, z) {
    read.csv(x) %>%
      tibble() %>%
      mutate(vis_src = y, season = z, .before = 1)
  }
) %>%
  bind_rows() %>%
  rename_with(~ tolower(gsub("\\.", "_", .x))) %>%
  # 更改列名：在小写字母和"centrality"之间加下划线。
  rename_with(
    ~ gsub("([a-z]centrality)", "", .x),
    matches("centrality$")
  ) %>%
  rename(
    "harmonic" = "harmonicclosnes",
    "betweeness" = "betweenes",
    "closeness" = "closnes"
  ) %>%
  # 删除加計呂麻島节点（ka1-ka8）。
  filter(!grepl("^ka", id))

# 标记每个 vis_src × season 中节点数最多的 modularity_class（top_mod == 1）。
# 暂时注释：gephi_output CSV 缺少 modularity_class 列，需重新从 Gephi 导出后启用。
# combined_data <- combined_data %>%
#   group_by(vis_src, season, modularity_class) %>%
#   mutate(mod_size = n()) %>%
#   group_by(vis_src, season) %>%
#   mutate(top_mod = as.integer(mod_size == max(mod_size))) %>%
#   ungroup() %>%
#   select(-mod_size)

# Demand ----
## 图6 ----
# 函数：画特定群体各季节各中心度地图。
plt_demand_map <- function(visitor_x) {
  ggplot() +
    geom_sf(data = amami, col = "lightgrey") +
    geom_sf(
      data = loc %>%
        st_centroid() %>%
        left_join(
          combined_data %>% filter(vis_src == visitor_x),
          by = c("loc_id" = "id")
        ) %>%
        select(
          "loc_id", "spa_group", "vis_src", "season",
          "degree", "closeness", "harmonic"
        ) %>%
        pivot_longer(
          cols = c(degree, closeness, harmonic),
          names_to = "centrality",
          values_to = "cen_val"
        ) %>%
        # 对各个中心度进行标准化。
        group_by(centrality) %>%
        mutate(
          # Min-Max 标准化公式：(x - min(x)) / (max(x) - min(x))
          cen_val_normalized = (cen_val - min(cen_val, na.rm = TRUE)) /
            (max(cen_val, na.rm = TRUE) - min(cen_val, na.rm = TRUE))
        ) %>%
        ungroup() %>%
        # 修改变量类型。
        mutate(spa_group = factor(spa_group, levels = c(
          "north", "tatsugo", "airport", "city",
          "mangrove", "mid", "uken", "setouchi"
        ))),
      aes(size = cen_val_normalized, col = spa_group), alpha = 0.6
    ) +
    scale_color_npg(labels = function(x) str_to_title(x)) +
    scale_x_continuous(
      breaks = c(129.1, 129.3, 129.5, 129.7),
      labels = c("129.1E", "129.3E", "129.5E", "129.7E")
    ) +
    labs(col = "Location cluster") +
    scale_size_continuous(
      name = "Normalized Centrality", range = c(0.1, 3)
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90),
      panel.grid = element_line(color = "white"),
      legend.text = element_text(size = 13),
      legend.title = element_text(size = 14)
    ) +
    facet_grid(centrality ~ season)
}
# 居民各季节各项中心度。
png(
  "data_proc/loc_demand_map_resident.png",
  width = 3000, height = 2000, res = 300
)
plt_demand_map("local")
dev.off()
# 游客各季节各项中心度。
png(
  "data_proc/loc_demand_map_tourist.png",
  width = 3000, height = 2000, res = 300
)
plt_demand_map("tourist")
dev.off()

# 各中心度季节均值地图：2行（Resident/Tourist）× 3列（Degree/Closeness/Harmonic）。
p_map <- loc %>%
  st_centroid() %>%
  left_join(
    combined_data %>%
      filter(vis_src %in% c("local", "tourist")) %>%
      group_by(id, vis_src) %>%
      summarise(
        across(c(degree, closeness, harmonic), mean, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("loc_id" = "id")
  ) %>%
  select("loc_id", "spa_group", "vis_src", "degree", "closeness", "harmonic") %>%
  filter(!is.na(vis_src)) %>%
  pivot_longer(
    cols = c(degree, closeness, harmonic),
    names_to = "centrality",
    values_to = "cen_val"
  ) %>%
  group_by(centrality, vis_src) %>%
  mutate(
    cen_val_normalized = (cen_val - min(cen_val, na.rm = TRUE)) /
      (max(cen_val, na.rm = TRUE) - min(cen_val, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  mutate(
    spa_group = factor(spa_group, levels = c(
      "north", "tatsugo", "airport", "city",
      "mangrove", "mid", "uken", "setouchi"
    )),
    vis_src = recode(vis_src, "local" = "Resident", "tourist" = "Tourist"),
    centrality = str_to_title(centrality)
  ) %>%
  ggplot() +
  geom_sf(data = amami, col = "lightgrey") +
  geom_sf(aes(size = cen_val_normalized, col = spa_group), alpha = 0.6) +
  scale_color_npg(labels = function(x) str_to_title(x)) +
  scale_x_continuous(
    breaks = c(129.1, 129.3, 129.5, 129.7),
    labels = c("129.1E", "129.3E", "129.5E", "129.7E")
  ) +
  labs(col = "Location cluster") +
  scale_size_continuous(name = "Normalized centrality", range = c(0.1, 3)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90),
    panel.grid = element_line(color = "white"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9)
  ) +
  facet_grid(centrality ~ vis_src)

png(
  paste0("data_proc/loc_demand_map_avg_", Sys.Date(), ".png"),
  width = 1800, height = 2500, res = 300
)
print(p_map)
dev.off()

# 第三部分：各用户群体不同地点组团中分季度中心度的中值对比。
# 2行（Resident/Tourist）× 3列（Degree/Closeness/Harmonic），瘦长版。
p_cen <- loc %>%
  st_drop_geometry() %>%
  left_join(
    combined_data %>%
      filter(vis_src %in% c("local", "tourist")) %>%
      group_by(vis_src) %>%
      mutate(degree = (degree - min(degree, na.rm = TRUE)) /
               (max(degree, na.rm = TRUE) - min(degree, na.rm = TRUE))) %>%
      ungroup(),
    by = c("loc_id" = "id")
  ) %>%
  select(
    "loc_id", "spa_group", "vis_src", "season",
    "degree", "closeness", "harmonic"
  ) %>%
  filter(!is.na(vis_src)) %>%
  group_by(vis_src, spa_group, season) %>%
  summarise(
    across(c(degree, closeness, harmonic), function(x) median(x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(degree, closeness, harmonic),
    names_to = "centrality",
    values_to = "cen_val"
  ) %>%
  mutate(
    spa_group = str_to_title(spa_group),
    vis_src = recode(vis_src, "local" = "Resident", "tourist" = "Tourist"),
    centrality = str_to_title(centrality)
  ) %>%
  ggplot() +
  geom_point(
    aes(spa_group, cen_val, col = as.factor(season)),
    position = position_dodge(width = 0.4), alpha = 0.8
  ) +
  facet_grid(centrality ~ vis_src) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Location cluster", y = "Centrality", col = "Quarter") +
  scale_color_manual(values = c(
    "1" = "#FF9EBC", "2" = "#4DAF4A", "3" = "#E41A1C", "4" = "#377EB8"
  )) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9)
  )

png(
  paste0("data_proc/loc_cen_mid_", Sys.Date(), ".png"),
  width = 1200, height = 2000, res = 300
)
print(p_cen)
dev.off()

# 合并地图（左）与中心度点图（右），两图均为3行×2列，行方向对齐。
# 仅在合并图中放大文字和点，不影响单独保存的图。
p_map_comb <- p_map +
  theme(
    text          = element_text(size = 14),
    axis.text     = element_text(size = 14),
    strip.text    = element_text(size = 16),
    axis.title    = element_text(size = 16),
    legend.text   = element_text(size = 14),
    legend.title  = element_text(size = 16)
  )

p_cen_comb <- p_cen
p_cen_comb$layers[[1]]$aes_params$size <- 3.5   # 放大点（用户已调整）
p_cen_comb <- p_cen_comb +
  theme(
    text         = element_text(size = 14),
    axis.text    = element_text(size = 14),
    strip.text   = element_text(size = 16),
    axis.text.x  = element_text(size = 14, angle = 90, hjust = 1),
    axis.title   = element_text(size = 16),
    axis.title.x = element_blank(),
    legend.text  = element_text(size = 14),
    legend.title = element_text(size = 16)
  )

png(
  paste0("data_proc/loc_demand_cen_combined_", Sys.Date(), ".png"),
  width = 4000, height = 2200, res = 300
)
print(
  p_map_comb + p_cen_comb +
    plot_layout(widths = c(1, 1)) +
    plot_annotation(
      tag_levels = "a", tag_prefix = "(", tag_suffix = ")",
      theme = theme(plot.tag = element_text(size = 16))
    )
)
dev.off()

# Supply ----
## 图7 ----
# 定义不同可达时间段的权重。
poi_access_weight <-
  setNames(sapply(seq(5, 30, 5), function(x) 1/x), seq(5, 30, 5))

# POI表格路径。
poi_file_path <- "data_raw/loc_poi_overlay.xlsx"

# 函数：处理单个POI表格。
proc_poi_sheet <- function(sheet_name) {
  # 读取表格第1行：包含POI类型和列名。
  row_first <- read_excel(poi_file_path, sheet = sheet_name, n_max = 1)
  poi_type <- names(row_first)[[2]]
  # 读取表格主要数据。
  df <- read_excel(poi_file_path, sheet = sheet_name, skip = 1)[, -2] %>%
    rename_with(~ c(
      "loc_id",
      paste(rep(c("walk", "drive"), each = 6), row_first[, 3:14], sep = "_")
    ))

  # 加权计算可达性指标：给行人赋予更高权重。
  walk_score <- as.matrix(select(df, contains("walk"))) %*% poi_access_weight
  drive_score <- as.matrix(select(df, contains("drive"))) %*% poi_access_weight
  tibble(
    loc_id = df$loc_id,
    access = as.numeric(0.6 * walk_score + 0.4 * drive_score)
  ) %>%
    rename_with(~ c("loc_id", poi_type))
}

# 仅处理当前研究使用的六类服务，并合并结果。
poi_service_sheets <- c("Sheet1", "Sheet2", "Sheet3", "Sheet4", "Sheet7", "Sheet8")
loc_poi_access <- lapply(poi_service_sheets, proc_poi_sheet) %>%
  reduce(left_join, by = "loc_id") %>%
  rename_with(~ tolower(.x))
# 查看各地点可达性。
# 定义可达性字段
access_cols <- c(
  "education", "government", "health", "ac",
  "retail", "tourism"
)

png(
  "data_proc/loc_poi_access_map.png",
  width = 3000, height = 2000, res = 300
)
p_access <- ggplot() +
  geom_sf(data = amami, col = "lightgrey") +
  geom_sf(
    data = loc %>%
      filter(!grepl("^ka", loc_id)) %>%
      st_centroid() %>%
      left_join(loc_poi_access, by = "loc_id") %>%
      select(loc_id, spa_group, all_of(access_cols)) %>%
      pivot_longer(
        cols = all_of(access_cols),
        names_to = "poi",
        values_to = "Accessibility"
      ) %>%
      mutate(
        spa_group = factor(spa_group, levels = c(
          "north", "tatsugo", "airport", "city",
          "mangrove", "mid", "uken", "setouchi"
        )),
        poi = factor(
          recode(
            poi,
            "government" = "Government",
            "education" = "Community",
            "health" = "Health",
            "retail" = "Commercial",
            "ac" = "Accommodation & food",
            "tourism" = "Recreation"
          ),
          levels = c(
            "Government",
            "Community",
            "Health",
            "Commercial",
            "Accommodation & food",
            "Recreation"
          )
        )
      ),
    aes(size = Accessibility, col = spa_group), alpha = 0.6
  ) +
  labs(col = "Location Cluster") +
  scale_color_npg(labels = function(x) str_to_title(x)) +
  scale_x_continuous(
    breaks = c(129.1, 129.3, 129.5, 129.7),
    labels = c("129.1E", "129.3E", "129.5E", "129.7E")
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 16.5),
    axis.text.x = element_text(angle = 90),
    panel.grid = element_line(color = "white")
  ) +
  facet_wrap(
    .~ poi, nrow = 2,
    labeller = labeller(poi = label_wrap_gen(width = 20))
  )
print(p_access)
dev.off()

# 导出对应数据。
loc %>%
  st_drop_geometry() %>%
  left_join(loc_poi_access, by = "loc_id") %>%
  select(loc_id, all_of(access_cols)) %>%
  write.csv("data_proc/loc_poi_access.csv")

# 表1：六类服务可达性的描述统计。
service_labels <- c(
  government = "Government",
  education = "Community",
  health = "Health",
  retail = "Commercial",
  ac = "Accommodation & food",
  tourism = "Recreation"
)

access_cluster_data <- loc %>%
  st_drop_geometry() %>%
  filter(!grepl("^ka", loc_id)) %>%
  select(loc_id, spa_group) %>%
  left_join(loc_poi_access, by = "loc_id")

access_table_data <- access_cluster_data %>%
  select(loc_id, all_of(names(service_labels)))

accessibility_service_summary <- map_dfr(
  names(service_labels),
  function(service_name) {
    service_value <- access_table_data[[service_name]]
    tibble(
      Service = unname(service_labels[[service_name]]),
      Median = median(service_value, na.rm = TRUE),
      Mean = mean(service_value, na.rm = TRUE),
      Minimum = min(service_value, na.rm = TRUE),
      Maximum = max(service_value, na.rm = TRUE),
      Variance = var(service_value, na.rm = TRUE)
    )
  }
)

write.csv(
  accessibility_service_summary,
  "data_proc/loc_accessibility_service_summary.csv",
  row.names = FALSE
)

# 表2：各 cluster 的六类服务可达性中位数。
accessibility_cluster_median <- access_cluster_data %>%
  group_by(spa_group) %>%
  summarise(
    across(all_of(names(service_labels)), ~ median(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  rename_with(
    ~ unname(service_labels[.x]),
    all_of(names(service_labels))
  ) %>%
  rename(Cluster = spa_group)

write.csv(
  accessibility_cluster_median,
  "data_proc/loc_accessibility_cluster_median.csv",
  row.names = FALSE
)

# 表3：每类服务内按 cluster 中位可达性从高到低排序。
accessibility_cluster_ranking <- accessibility_cluster_median %>%
  pivot_longer(
    cols = -Cluster,
    names_to = "Service",
    values_to = "Median_accessibility"
  ) %>%
  mutate(Service = factor(Service, levels = unname(service_labels))) %>%
  group_by(Service) %>%
  arrange(desc(Median_accessibility), Cluster, .by_group = TRUE) %>%
  mutate(Rank = row_number()) %>%
  ungroup() %>%
  select(Rank, Service, Cluster) %>%
  pivot_wider(names_from = Service, values_from = Cluster) %>%
  arrange(Rank)

write.csv(
  accessibility_cluster_ranking,
  "data_proc/loc_accessibility_cluster_ranking.csv",
  row.names = FALSE
)

# 表4：六类服务可达性的两两 Spearman 相关系数。
accessibility_spearman <- cor(
  access_table_data %>% select(all_of(names(service_labels))),
  method = "spearman",
  use = "pairwise.complete.obs"
)
rownames(accessibility_spearman) <- unname(service_labels)
colnames(accessibility_spearman) <- unname(service_labels)

write.csv(
  accessibility_spearman,
  "data_proc/loc_accessibility_spearman.csv",
  row.names = TRUE
)

# Demand and supply ----
# Step 1: 对 degree 进行 min-max 归一化（在 vis_src 组内），
# 使其与 closeness/harmonic（已由 Gephi 归一化至 [0,1]）量纲一致。
combined_data_norm <- combined_data %>%
  group_by(vis_src) %>%
  mutate(
    degree_norm = (degree - min(degree, na.rm = TRUE)) /
                  (max(degree, na.rm = TRUE) - min(degree, na.rm = TRUE))
  ) %>%
  ungroup()

# 分客源-季节的各地点供需指数。
loc_dem_sup <-
  list(
    # 本地人。
    combined_data_norm %>%
      left_join(loc_poi_access, by = c("id" = "loc_id")) %>%
      filter(vis_src == "local") %>%
      mutate(
        # Step 2: 加权合成需求指数（各服务类型对应不同中心度）。
        demand_edu    = degree_norm,
        demand_gov    = degree_norm,
        demand_health = closeness,
        # Commercial demand uses the same specification for both groups:
        # direct mobility connectivity plus network-wide reachability.
        demand_retail = 0.7 * degree_norm + 0.3 * harmonic,
        # Step 3: SDI_raw = 可达性 / 需求指数。
        sdi_raw_edu    = education  / demand_edu,
        sdi_raw_gov    = government / demand_gov,
        sdi_raw_health = health     / demand_health,
        sdi_raw_retail = retail     / demand_retail
      ) %>%
      # 需求为0时 SDI_raw 为 Inf/NaN，替换为 NA。
      mutate(across(starts_with("sdi_raw_"),
                    ~ ifelse(is.infinite(.x) | is.nan(.x), NA, .x))) %>%
      # Step 4: min-max 归一化获得最终 SDI（在 vis_src 组内）。
      group_by(vis_src) %>%
      mutate(across(
        starts_with("sdi_raw_"),
        ~ (.x - min(.x, na.rm = TRUE)) / (max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE))
      )) %>%
      ungroup() %>%
      rename(
        ds_edu        = sdi_raw_edu,
        ds_gov        = sdi_raw_gov,
        ds_health     = sdi_raw_health,
        ds_retail_mix = sdi_raw_retail
      ) %>%
      select(vis_src, id, season, ds_edu, ds_gov, ds_health, ds_retail_mix) %>%
      pivot_longer(
        cols = starts_with("ds_"), names_to = "ds_cat", values_to = "ds_val"
      ),
    # 游客。
    combined_data_norm %>%
      left_join(loc_poi_access, by = c("id" = "loc_id")) %>%
      filter(vis_src == "tourist") %>%
      mutate(
        # Step 2: 加权合成需求指数。
        demand_accomfood = 0.7 * degree_norm + 0.3 * closeness,
        demand_retail    = 0.7 * degree_norm + 0.3 * harmonic,
        demand_tour      = 0.5 * degree_norm + 0.3 * closeness + 0.2 * harmonic,
        # Step 3: SDI_raw = 可达性 / 需求指数。
        sdi_raw_accomfood = ac      / demand_accomfood,
        sdi_raw_retail    = retail  / demand_retail,
        sdi_raw_tour      = tourism / demand_tour
      ) %>%
      mutate(across(starts_with("sdi_raw_"),
                    ~ ifelse(is.infinite(.x) | is.nan(.x), NA, .x))) %>%
      # Step 4: 归一化。
      group_by(vis_src) %>%
      mutate(across(
        starts_with("sdi_raw_"),
        ~ (.x - min(.x, na.rm = TRUE)) / (max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE))
      )) %>%
      ungroup() %>%
      rename(
        ds_accomfood_mix = sdi_raw_accomfood,
        ds_retail_mix    = sdi_raw_retail,
        ds_tour_mix      = sdi_raw_tour
      ) %>%
      select(vis_src, id, season, ds_accomfood_mix, ds_retail_mix, ds_tour_mix) %>%
      pivot_longer(
        cols = starts_with("ds_"), names_to = "ds_cat", values_to = "ds_val"
      )
  ) %>%
  bind_rows() %>%
  # 获得经纬度和 spa_group 信息。
  left_join(st_centroid(loc), by = c("id" = "loc_id")) %>%
  st_as_sf() %>%
  mutate(long = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2]) %>%
  st_drop_geometry()

# 基于固定阈值识别相对供需错配：高需求（Q60及以上）且低可达性（Q40及以下）。
# 阈值在每个用户组 × 服务类型的全部地点和四个季度中统一计算，
# 因此不会强制每个季度产生固定数量的“短缺”地点。
loc_mismatch <- list(
  # 居民使用的服务。
  combined_data_norm %>%
    left_join(loc_poi_access, by = c("id" = "loc_id")) %>%
    filter(vis_src == "local") %>%
    transmute(vis_src, id, season, ds_cat = "ds_edu",
              demand = degree_norm, accessibility = education),
  combined_data_norm %>%
    left_join(loc_poi_access, by = c("id" = "loc_id")) %>%
    filter(vis_src == "local") %>%
    transmute(vis_src, id, season, ds_cat = "ds_gov",
              demand = degree_norm, accessibility = government),
  combined_data_norm %>%
    left_join(loc_poi_access, by = c("id" = "loc_id")) %>%
    filter(vis_src == "local") %>%
    transmute(vis_src, id, season, ds_cat = "ds_health",
              demand = closeness, accessibility = health),
  combined_data_norm %>%
    left_join(loc_poi_access, by = c("id" = "loc_id")) %>%
    filter(vis_src == "local") %>%
    transmute(
      vis_src, id, season, ds_cat = "ds_retail_mix",
      demand = 0.7 * degree_norm + 0.3 * harmonic,
      accessibility = retail
    ),
  # 游客使用的服务。
  combined_data_norm %>%
    left_join(loc_poi_access, by = c("id" = "loc_id")) %>%
    filter(vis_src == "tourist") %>%
    transmute(
      vis_src, id, season, ds_cat = "ds_accomfood_mix",
      demand = 0.7 * degree_norm + 0.3 * closeness,
      accessibility = ac
    ),
  combined_data_norm %>%
    left_join(loc_poi_access, by = c("id" = "loc_id")) %>%
    filter(vis_src == "tourist") %>%
    transmute(
      vis_src, id, season, ds_cat = "ds_retail_mix",
      demand = 0.7 * degree_norm + 0.3 * harmonic,
      accessibility = retail
    ),
  combined_data_norm %>%
    left_join(loc_poi_access, by = c("id" = "loc_id")) %>%
    filter(vis_src == "tourist") %>%
    transmute(
      vis_src, id, season, ds_cat = "ds_tour_mix",
      demand = 0.5 * degree_norm + 0.3 * closeness + 0.2 * harmonic,
      accessibility = tourism
    )
) %>%
  bind_rows() %>%
  filter(!is.na(demand), !is.na(accessibility)) %>%
  group_by(vis_src, ds_cat) %>%
  mutate(
    demand_q60 = quantile(demand, 0.60, na.rm = TRUE),
    accessibility_q40 = quantile(accessibility, 0.40, na.rm = TRUE),
    demand_percentile = percent_rank(demand),
    accessibility_percentile = percent_rank(accessibility),
    mismatch_severity = pmax(demand_percentile - accessibility_percentile, 0),
    is_mismatch = demand >= demand_q60 & accessibility <= accessibility_q40
  ) %>%
  ungroup() %>%
  left_join(
    st_centroid(loc) %>%
      filter(!grepl("^ka", loc_id)) %>%
      select(loc_id, spa_group),
    by = c("id" = "loc_id")
  ) %>%
  st_as_sf() %>%
  mutate(
    long = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry()

# 导出完整的连续指标和二元错配判定，供复核与敏感性分析。
write.csv(
  loc_mismatch,
  "data_proc/loc_supply_demand_mismatch.csv",
  row.names = FALSE
)

# 使用原有的饼图地图样式绘制相对供需错配。
ds_mismatch_label <- c(
  "ds_accomfood_mix" = "Accommodation & food",
  "ds_retail_mix" = "Commercial",
  "ds_edu" = "Community",
  "ds_gov" = "Government",
  "ds_health" = "Health",
  "ds_tour_mix" = "Recreation"
)

# 固定服务颜色，确保居民与游客图层中的同一服务始终使用同一种颜色。
ds_mismatch_colors <- setNames(
  pal_npg()(length(ds_mismatch_label)),
  names(ds_mismatch_label)
)
ds_mismatch_colors[c("ds_edu", "ds_health")] <-
  ds_mismatch_colors[c("ds_health", "ds_edu")]
ds_mismatch_colors[c("ds_retail_mix", "ds_accomfood_mix")] <-
  ds_mismatch_colors[c("ds_accomfood_mix", "ds_retail_mix")]

# 按用户组、cluster、服务和季度汇总错配地点数，作为结果陈述的依据。
mismatch_cluster_service_summary <- loc_mismatch %>%
  mutate(is_mismatch = as.integer(is_mismatch)) %>%
  group_by(vis_src, spa_group, ds_cat, season) %>%
  summarise(
    mismatch_n = sum(is_mismatch),
    eligible_n = n(),
    .groups = "drop"
  ) %>%
  mutate(service = unname(ds_mismatch_label[ds_cat])) %>%
  select(vis_src, spa_group, service, season, mismatch_n, eligible_n) %>%
  pivot_wider(
    names_from = season,
    values_from = c(mismatch_n, eligible_n),
    names_glue = "{.value}_quarter_{season}",
    values_fill = 0
  ) %>%
  mutate(
    mismatch_total = rowSums(across(starts_with("mismatch_n_quarter_"))),
    eligible_total = rowSums(across(starts_with("eligible_n_"))),
    mismatch_percent = 100 * mismatch_total / eligible_total
  ) %>%
  arrange(vis_src, spa_group, service)

write.csv(
  mismatch_cluster_service_summary,
  "data_proc/loc_mismatch_cluster_service_summary.csv",
  row.names = FALSE
)

plt_mismatch_map <- function() {
  plt_data <- loc_mismatch %>%
    filter(is_mismatch) %>%
    mutate(
      season = factor(season, levels = 1:4),
      vis_src = factor(
        vis_src, levels = c("local", "tourist"),
        labels = c("Residents", "Tourists")
      ),
      ds_val_fill = 1
    ) %>%
    pivot_wider(
      id_cols = c(vis_src, id, season, long, lat),
      names_from = ds_cat,
      values_from = ds_val_fill,
      values_fill = 0
    ) %>%
    mutate(radius = 0.0324)

  p <- ggplot(data = expand_grid(
    vis_src = factor(c("Residents", "Tourists"),
                     levels = c("Residents", "Tourists")),
    season = factor(1:4, levels = 1:4)
  )) +
    geom_sf(data = amami, fill = "white") +
    geom_sf(
      data = st_centroid(loc) %>% filter(!grepl("^ka", loc_id)),
      size = 1.4, col = "darkgrey", alpha = 0.8
    )

  if (nrow(plt_data) > 0) {
    p <- p + geom_scatterpie(
      data = plt_data,
      aes(x = long, y = lat, r = radius),
      cols = grep("^ds_", names(plt_data), value = TRUE),
      linewidth = 0.1, color = "white", alpha = 0.9
    )
  }

  p +
    scale_fill_manual(
      values = ds_mismatch_colors,
      breaks = names(ds_mismatch_label),
      labels = ds_mismatch_label,
      drop = FALSE
    ) +
    scale_x_continuous(
      breaks = c(129.1, 129.3, 129.5, 129.7),
      labels = c("129.1E", "129.3E", "129.5E", "129.7E")
    ) +
    labs(x = NULL, y = NULL, fill = "Service") +
    theme_bw(base_size = 22) +
    theme(
      axis.text.x = element_text(size = 13.6, angle = 90),
      axis.text.y = element_text(size = 13.6),
      panel.grid = element_line(color = "white")
    ) +
    facet_grid(
      vis_src ~ season, drop = FALSE,
      labeller = labeller(season = c(
        "1" = "Quarter 1", "2" = "Quarter 2",
        "3" = "Quarter 3", "4" = "Quarter 4"
      ))
    )
}

png(
  paste0("data_proc/ds_mismatch_map_combined_", Sys.Date(), ".png"),
  width = 3500, height = 1900, res = 300
)
print(plt_mismatch_map())
dev.off()

# 重叠饼图的避让版本：在每个用户组和季度内移动相互遮挡的饼图，
# 并用引线连接饼图中心与原始地点。
separate_overlapping_pies <- function(data, radius = 0.0324,
                                      min_distance = radius * 2.1,
                                      max_offset = radius * 2.8,
                                      iterations = 250) {
  if (nrow(data) < 2) {
    return(data %>% mutate(plot_long = long, plot_lat = lat))
  }

  original <- as.matrix(data[, c("long", "lat")])
  displaced <- original

  for (iteration in seq_len(iterations)) {
    for (i in seq_len(nrow(data) - 1)) {
      for (j in (i + 1):nrow(data)) {
        delta <- displaced[j, ] - displaced[i, ]
        distance <- sqrt(sum(delta^2))
        if (distance < min_distance) {
          if (distance == 0) {
            angle <- 2 * pi * (j - i) / nrow(data)
            direction <- c(cos(angle), sin(angle))
          } else {
            direction <- delta / distance
          }
          shift <- direction * (min_distance - distance) / 2
          displaced[i, ] <- displaced[i, ] - shift
          displaced[j, ] <- displaced[j, ] + shift
        }
      }
    }

    # 轻微拉回原地点，避免饼图移动得过远。
    displaced <- displaced + 0.015 * (original - displaced)
    offsets <- displaced - original
    offset_distance <- sqrt(rowSums(offsets^2))
    too_far <- offset_distance > max_offset
    if (any(too_far)) {
      displaced[too_far, ] <- original[too_far, ] +
        offsets[too_far, , drop = FALSE] *
        (max_offset / offset_distance[too_far])
    }
  }

  data %>%
    mutate(
      plot_long = displaced[, 1],
      plot_lat = displaced[, 2]
    )
}

ds_mismatch_order <- c(
  "ds_gov", "ds_edu", "ds_health", "ds_retail_mix",
  "ds_accomfood_mix", "ds_tour_mix"
)

plt_mismatch_map_leaderline_group <- function(vis_src_x, panel_title) {
  plt_data <- loc_mismatch %>%
    filter(is_mismatch, vis_src == vis_src_x) %>%
    mutate(
      season = factor(season, levels = 1:4),
      ds_val_fill = 1
    ) %>%
    pivot_wider(
      id_cols = c(id, season, long, lat),
      names_from = ds_cat,
      values_from = ds_val_fill,
      values_fill = 0
    ) %>%
    group_by(season) %>%
    group_modify(~ separate_overlapping_pies(.x)) %>%
    ungroup() %>%
    mutate(radius = 0.0324)

  ggplot(data = tibble(season = factor(1:4, levels = 1:4))) +
    geom_sf(data = amami, fill = "white", color = "grey60") +
    geom_segment(
      data = plt_data,
      aes(x = long, y = lat, xend = plot_long, yend = plot_lat),
      color = "grey35", linewidth = 0.35
    ) +
    geom_scatterpie(
      data = plt_data,
      aes(x = plot_long, y = plot_lat, r = radius),
      cols = grep("^ds_", names(plt_data), value = TRUE),
      linewidth = 0.1, color = "white", alpha = 0.9
    ) +
    scale_fill_manual(
      values = ds_mismatch_colors,
      breaks = ds_mismatch_order,
      labels = ds_mismatch_label,
      drop = TRUE
    ) +
    scale_x_continuous(
      breaks = c(129.1, 129.3, 129.5, 129.7),
      labels = c("129.1E", "129.3E", "129.5E", "129.7E")
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    labs(x = NULL, y = NULL, fill = "Service", title = panel_title) +
    theme_bw(base_size = 22) +
    theme(
      axis.text.x = element_text(size = 13.6, angle = 90),
      axis.text.y = element_text(size = 13.6),
      panel.grid = element_line(color = "white"),
      plot.title = element_text(size = 22, hjust = 0, face = "plain"),
      legend.position = "bottom",
      legend.direction = "horizontal"
    ) +
    facet_wrap(
      . ~ season, nrow = 1, drop = FALSE,
      labeller = labeller(season = c(
        "1" = "Quarter 1", "2" = "Quarter 2",
        "3" = "Quarter 3", "4" = "Quarter 4"
      ))
    )
}

plt_mismatch_map_leaderlines <- function() {
  plt_mismatch_map_leaderline_group("local", "(a) Residents") /
    plt_mismatch_map_leaderline_group("tourist", "(b) Tourists") +
    plot_layout(guides = "keep")
}

png(
  paste0("data_proc/ds_mismatch_map_combined_leaderlines_",
         Sys.Date(), ".png"),
  width = 3500, height = 3000, res = 300
)
print(plt_mismatch_map_leaderlines())
dev.off()

# 离岸排列版本：每组重叠饼图保留一个在原位，其余移到岛屿轮廓外。
move_overlapping_pies_offshore <- function(data, radius = 0.0324,
                                           min_distance = radius * 2.1) {
  data <- data %>% mutate(plot_long = long, plot_lat = lat, moved = FALSE)
  n_points <- nrow(data)
  if (n_points < 2) return(data)

  coordinates <- as.matrix(data[, c("long", "lat")])
  distances <- as.matrix(dist(coordinates))
  adjacency <- distances < min_distance & distances > 0

  # 找出由相互重叠的饼图构成的连通组。
  component <- rep(NA_integer_, n_points)
  component_id <- 0L
  for (start in seq_len(n_points)) {
    if (!is.na(component[start])) next
    component_id <- component_id + 1L
    queue <- start
    component[start] <- component_id
    while (length(queue) > 0) {
      current <- queue[1]
      queue <- queue[-1]
      neighbours <- which(adjacency[current, ] & is.na(component))
      if (length(neighbours) > 0) {
        component[neighbours] <- component_id
        queue <- c(queue, neighbours)
      }
    }
  }

  island_bbox <- st_bbox(amami)
  island_midpoint <- mean(c(island_bbox[["xmin"]], island_bbox[["xmax"]]))
  offshore_x <- c(
    left = island_bbox[["xmin"]] - 0.10,
    right = island_bbox[["xmax"]] + 0.10
  )

  for (group_id in unique(component)) {
    members <- which(component == group_id)
    if (length(members) < 2) next

    group_center <- colMeans(coordinates[members, , drop = FALSE])
    # 将最接近重叠组中心的一个饼图留在原地点。
    keep <- members[which.min(rowSums(
      (coordinates[members, , drop = FALSE] -
         matrix(group_center, nrow = length(members), ncol = 2,
                byrow = TRUE))^2
    ))]
    move <- setdiff(members, keep)
    side <- if (group_center[1] >= island_midpoint) "right" else "left"
    vertical_positions <- group_center[2] +
      (seq_along(move) - mean(seq_along(move))) * 0.085

    data$plot_long[move] <- offshore_x[[side]]
    data$plot_lat[move] <- vertical_positions
    data$moved[move] <- TRUE
  }

  data
}

plt_mismatch_map_offshore <- function() {
  plt_data <- loc_mismatch %>%
    filter(is_mismatch) %>%
    mutate(
      season = factor(season, levels = 1:4),
      vis_src = factor(
        vis_src, levels = c("local", "tourist"),
        labels = c("Residents", "Tourists")
      ),
      ds_val_fill = 1
    ) %>%
    pivot_wider(
      id_cols = c(vis_src, id, season, long, lat),
      names_from = ds_cat,
      values_from = ds_val_fill,
      values_fill = 0
    ) %>%
    group_by(vis_src, season) %>%
    group_modify(~ move_overlapping_pies_offshore(.x)) %>%
    ungroup() %>%
    mutate(radius = 0.0324)

  ggplot(data = expand_grid(
    vis_src = factor(c("Residents", "Tourists"),
                     levels = c("Residents", "Tourists")),
    season = factor(1:4, levels = 1:4)
  )) +
    geom_sf(data = amami, fill = "white") +
    geom_sf(
      data = st_centroid(loc) %>% filter(!grepl("^ka", loc_id)),
      size = 1.4, col = "darkgrey", alpha = 0.8
    ) +
    geom_segment(
      data = plt_data %>% filter(moved),
      aes(x = long, y = lat, xend = plot_long, yend = plot_lat),
      color = "grey35", linewidth = 0.35
    ) +
    geom_scatterpie(
      data = plt_data,
      aes(x = plot_long, y = plot_lat, r = radius),
      cols = grep("^ds_", names(plt_data), value = TRUE),
      linewidth = 0.1, color = "white", alpha = 0.9
    ) +
    scale_fill_manual(
      values = ds_mismatch_colors,
      breaks = names(ds_mismatch_label),
      labels = ds_mismatch_label,
      drop = FALSE
    ) +
    scale_x_continuous(
      breaks = c(129.1, 129.3, 129.5, 129.7),
      labels = c("129.1E", "129.3E", "129.5E", "129.7E")
    ) +
    labs(x = NULL, y = NULL, fill = "Service") +
    theme_bw(base_size = 22) +
    theme(
      axis.text.x = element_text(size = 13.6, angle = 90),
      axis.text.y = element_text(size = 13.6),
      panel.grid = element_line(color = "white")
    ) +
    facet_grid(
      vis_src ~ season, drop = FALSE,
      labeller = labeller(season = c(
        "1" = "Quarter 1", "2" = "Quarter 2",
        "3" = "Quarter 3", "4" = "Quarter 4"
      ))
    )
}

png(
  paste0("data_proc/ds_mismatch_map_combined_offshore_",
         Sys.Date(), ".png"),
  width = 3500, height = 1900, res = 300
)
print(plt_mismatch_map_offshore())
dev.off()

# 多组分位数阈值的敏感性分析：由宽松到严格。
mismatch_thresholds <- tibble(
  demand_prob = c(0.60, 0.65, 0.70, 0.75, 0.80),
  accessibility_prob = c(0.40, 0.35, 0.30, 0.25, 0.20),
  threshold = factor(
    c("Q60/Q40", "Q65/Q35", "Q70/Q30", "Q75/Q25", "Q80/Q20"),
    levels = c("Q60/Q40", "Q65/Q35", "Q70/Q30", "Q75/Q25", "Q80/Q20")
  )
)

loc_mismatch_sensitivity <- loc_mismatch %>%
  select(vis_src, id, season, ds_cat, demand, accessibility,
         spa_group, long, lat) %>%
  crossing(mismatch_thresholds) %>%
  group_by(vis_src, ds_cat, demand_prob, accessibility_prob, threshold) %>%
  mutate(
    demand_threshold = quantile(demand, demand_prob[[1]], na.rm = TRUE),
    accessibility_threshold = quantile(
      accessibility, accessibility_prob[[1]], na.rm = TRUE
    ),
    is_mismatch = demand >= demand_threshold &
      accessibility <= accessibility_threshold
  ) %>%
  ungroup()

# 各阈值、用户组、季度和服务的 mismatch 数量。
mismatch_sensitivity_counts <- loc_mismatch_sensitivity %>%
  filter(is_mismatch) %>%
  count(threshold, demand_prob, accessibility_prob,
        vis_src, season, ds_cat, name = "mismatch_n") %>%
  complete(
    threshold = mismatch_thresholds$threshold,
    vis_src = c("local", "tourist"),
    season = 1:4,
    ds_cat,
    fill = list(mismatch_n = 0)
  ) %>%
  arrange(threshold, vis_src, season, ds_cat)

write.csv(
  mismatch_sensitivity_counts,
  "data_proc/loc_mismatch_sensitivity_counts.csv",
  row.names = FALSE
)

# 敏感性分析：计算Q60/Q40主分析中的错配地点，在更严格阈值下的保留比例。
# 阈值越严格，保留比例越低；下降缓慢表示主分析识别结果更稳定。
mismatch_sensitivity_retention <- mismatch_sensitivity_counts %>%
  group_by(vis_src, season, ds_cat) %>%
  mutate(
    main_n = mismatch_n[threshold == "Q60/Q40"],
    retention_rate = if_else(main_n > 0, mismatch_n / main_n, NA_real_)
  ) %>%
  ungroup()

write.csv(
  mismatch_sensitivity_retention,
  "data_proc/loc_mismatch_sensitivity_retention.csv",
  row.names = FALSE
)

plt_mismatch_sensitivity_retention <- mismatch_sensitivity_retention %>%
  mutate(
    vis_src = factor(
      vis_src, levels = c("local", "tourist"),
      labels = c("Residents", "Tourists")
    ),
    season = factor(
      season, levels = 1:4,
      labels = paste("Quarter", 1:4)
    )
  ) %>%
  ggplot(aes(
    x = threshold, y = retention_rate,
    color = ds_cat, group = ds_cat
  )) +
  geom_hline(yintercept = 1, color = "grey80", linewidth = 0.3) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  geom_point(size = 1.8, na.rm = TRUE) +
  scale_color_manual(
    values = ds_mismatch_colors,
    breaks = names(ds_mismatch_label),
    labels = ds_mismatch_label,
    drop = FALSE
  ) +
  scale_y_continuous(
    limits = c(0, 1), breaks = seq(0, 1, 0.25),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    x = "Demand/accessibility percentile threshold",
    y = "Locations retained from Q60/Q40",
    color = "Service"
  ) +
  facet_grid(vis_src ~ season) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

png(
  paste0("data_proc/ds_mismatch_sensitivity_retention_", Sys.Date(), ".png"),
  width = 3500, height = 1900, res = 300
)
print(plt_mismatch_sensitivity_retention)
dev.off()

plt_mismatch_sensitivity <- function(vis_src_x) {
  threshold_levels <- levels(mismatch_thresholds$threshold)
  plt_data <- loc_mismatch_sensitivity %>%
    filter(vis_src == vis_src_x, is_mismatch) %>%
    mutate(
      season = factor(season, levels = 1:4),
      threshold = factor(threshold, levels = threshold_levels),
      ds_val_fill = 1
    ) %>%
    pivot_wider(
      id_cols = c(threshold, id, season, long, lat),
      names_from = ds_cat,
      values_from = ds_val_fill,
      values_fill = 0
    ) %>%
    mutate(radius = 0.02)

  p <- ggplot(data = expand_grid(
    threshold = factor(threshold_levels, levels = threshold_levels),
    season = factor(1:4, levels = 1:4)
  )) +
    geom_sf(data = amami, fill = "white") +
    geom_sf(
      data = st_centroid(loc) %>% filter(!grepl("^ka", loc_id)),
      size = 1, col = "darkgrey", alpha = 0.8
    )

  if (nrow(plt_data) > 0) {
    p <- p + geom_scatterpie(
      data = plt_data,
      aes(x = long, y = lat, r = radius),
      cols = grep("^ds_", names(plt_data), value = TRUE),
      linewidth = 0.1, color = "white", alpha = 0.9
    )
  }

  p +
    scale_fill_manual(
      values = ds_mismatch_colors,
      breaks = names(ds_mismatch_label),
      labels = ds_mismatch_label,
      drop = FALSE
    ) +
    scale_x_continuous(
      breaks = c(129.1, 129.3, 129.5, 129.7),
      labels = c("129.1E", "129.3E", "129.5E", "129.7E")
    ) +
    labs(fill = "Service") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90),
      panel.grid = element_line(color = "white")
    ) +
    facet_grid(
      threshold ~ season, drop = FALSE,
      labeller = labeller(season = c(
        "1" = "Quarter 1", "2" = "Quarter 2",
        "3" = "Quarter 3", "4" = "Quarter 4"
      ))
    )
}

png(
  paste0("data_proc/ds_mismatch_sensitivity_local_", Sys.Date(), ".png"),
  width = 3500, height = 4500, res = 300
)
print(plt_mismatch_sensitivity("local"))
dev.off()

png(
  paste0("data_proc/ds_mismatch_sensitivity_tourist_", Sys.Date(), ".png"),
  width = 3500, height = 4500, res = 300
)
print(plt_mismatch_sensitivity("tourist"))
dev.off()

# 挑选出各客源-季节-需求中，供需比率最低的地点。
loc_dem_sup_min <- loc_dem_sup %>%
  group_by(vis_src, ds_cat) %>%
  slice_min(order_by = ds_val, n = 30) %>%
  ungroup()

# 密度图：分服务类型和客源，不分季节，比较各组团供给比率。
lapply(
  c("local", "tourist"),
  function(x) {
    loc_dem_sup %>%
      filter(vis_src == x) %>%
      ggplot() +
      geom_density(aes(log(ds_val))) +
      facet_grid(ds_cat ~ spa_group) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90))
  }
)

## 图8 ----
# 供需指数变量名和对应标签。
ds_label <- c(
  "ds_accomfood_mix" = "Accommodation & Food",
  "ds_retail_mix" = "Commerce",
  "ds_edu" = "Education",
  "ds_gov" = "Government",
  "ds_health" = "Health",
  "ds_tour_mix" = "Tourism & Recreation"
)

# 函数：用于画带有供需饼图的地图。
plt_ds_map <- function(vis_src_x) {
  plt_data <- loc_dem_sup_min %>%
    filter(vis_src == vis_src_x) %>%
    mutate(ds_val_fill = 1) %>%
    pivot_wider(
      id_cols = c(id, season, long, lat),
      names_from = ds_cat, values_from = ds_val_fill, values_fill = 0
    ) %>%
    mutate(radius = 0.02)

  ggplot() +
    geom_sf(data = amami, fill = "white") +
    geom_sf(data = st_centroid(loc), size = 1, col = "darkgrey", alpha = 0.8) +
    geom_scatterpie(
      data = plt_data,
      aes(x = long, y = lat, r = radius),
      cols= grep("^ds_", names(plt_data), value = TRUE),
      linewidth = 0.1, color = "white", alpha=0.9
    ) +
    scale_fill_npg(labels = ds_label) +
    scale_x_continuous(
      breaks = c(129.1, 129.3, 129.5, 129.7),
      labels = c("129.1E", "129.3E", "129.5E", "129.7E")
    ) +
    labs(fill = "Service") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90),
      panel.grid = element_line(color = "white")
    ) +
    facet_wrap(
      .~ season, nrow = 1,
      labeller = labeller(season = c(
        "1" = "Quarter 1", "2" = "Quarter 2",
        "3" = "Quarter 3", "4" = "Quarter 4"
      ))
    )
}

# 作图。
# 本地人供需。
png(
  paste0("data_proc/ds_map_local_", Sys.Date(), ".png"),
  width = 3500, height = 1000, res = 300
)
plt_ds_map("local")
dev.off()

# 游客供需。
png(
  paste0("data_proc/ds_map_tourist_", Sys.Date(), ".png"),
  width = 3500, height = 1000, res = 300
)
plt_ds_map("tourist")
dev.off()

# 如果混合起来呢？
plt_data <- loc_dem_sup_min %>%
  mutate(ds_val_fill = 1) %>%
  pivot_wider(
    id_cols = c(vis_src, id, season, long, lat),
    names_from = ds_cat, values_from = ds_val_fill, values_fill = 0
  ) %>%
  mutate(radius = 0.02)

png(
  paste0("data_proc/ds_map_all_", Sys.Date(), ".png"),
  width = 3500, height = 2500, res = 300
)
ggplot() +
  geom_sf(data = amami) +
  geom_scatterpie(
    data = plt_data,
    aes(x = long, y = lat, r = radius),
    cols= grep("^ds_", names(plt_data), value = TRUE),
    linewidth = 0.1, color = "white", alpha=0.9
  ) +
  scale_fill_npg() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(vis_src ~ season)
dev.off()

# 分客源-季节下各供给率低地点对比。
ggplot(filter(loc_dem_sup_min, vis_src == "local")) +
  geom_col(aes(id, ds_val)) +
  facet_grid(ds_cat ~ season) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))
ggplot(filter(loc_dem_sup_min, vis_src == "tourist")) +
  geom_col(aes(id, ds_val)) +
  facet_grid(ds_cat ~ season) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

# 分客源-季节下各供给率低地点对比地图。
png(
  paste0("data_proc/ds_size_local_", Sys.Date(), ".png"),
  width = 3500, height = 2500, res = 300
)
ggplot() +
  geom_sf(data = amami) +
  geom_sf(
    data = loc_dem_sup_min %>%
      st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
      filter(vis_src == "local"),
    # aes(size = ds_val),
    alpha = 0.5, col = "red"
  ) +
  facet_grid(ds_cat ~ season) +
  theme_bw()
dev.off()

png(
  paste0("data_proc/ds_size_tourist_", Sys.Date(), ".png"),
  width = 3500, height = 2500, res = 300
)
ggplot() +
  geom_sf(data = amami) +
  geom_sf(
    data = loc_dem_sup_min %>%
      st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
      filter(vis_src == "tourist"),
    # aes(size = ds_val),
    alpha = 0.5, col = "red"
  ) +
  facet_grid(ds_cat ~ season) +
  theme_bw()
dev.off()

# 分客源-季节下各供给率低地点数量对比地图。
png(
  paste0("data_proc/ds_number_local_", Sys.Date(), ".png"),
  width = 3500, height = 2500, res = 300
)
ggplot() +
  geom_sf(data = amami) +
  geom_sf(
    data = loc_dem_sup_min %>%
      st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
      filter(vis_src == "local") %>%
      group_by(vis_src, season, spa_group, ds_cat) %>%
      summarise(n = n(), geometry = first(geometry), .groups = "drop"),
    aes(size = n),
    alpha = 0.5, col = "red"
  ) +
  geom_sf_text(
    data = loc_dem_sup_min %>%
      st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
      filter(vis_src == "local") %>%
      group_by(vis_src, season, spa_group, ds_cat) %>%
      summarise(n = n(), geometry = first(geometry), .groups = "drop"),
    aes(label = n), size = 3
  ) +
  facet_grid(ds_cat ~ season) +
  theme_bw()
dev.off()

png(
  paste0("data_proc/ds_num_tourist_", Sys.Date(), ".png"),
  width = 3500, height = 2500, res = 300
)
ggplot() +
  geom_sf(data = amami) +
  geom_sf(
    data = loc_dem_sup_min %>%
      st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
      filter(vis_src == "tourist") %>%
      group_by(vis_src, season, spa_group, ds_cat) %>%
      summarise(n = n(), geometry = first(geometry), .groups = "drop"),
    aes(size = n),
    alpha = 0.5, col = "red"
  ) +
  geom_sf_text(
    data = loc_dem_sup_min %>%
      st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
      filter(vis_src == "tourist") %>%
      group_by(vis_src, season, spa_group, ds_cat) %>%
      summarise(n = n(), geometry = first(geometry), .groups = "drop"),
    aes(label = n), size = 3
  ) +
  facet_grid(ds_cat ~ season) +
  theme_bw()
dev.off()

## 图9 ----
# 各地点组团中供给短缺地点数量的比例。
png(
  "data_proc/loc_supply_short_percent.png",
  width = 2000, height = 1000, res = 300
)
loc_dem_sup_min %>%
  group_by(vis_src, ds_cat, spa_group, season) %>%
  summarise(loc_n = n(), .groups = "drop") %>%
  left_join(
    loc %>%
      st_drop_geometry() %>%
      group_by(spa_group) %>%
      summarise(loc_n_tot = n(), .groups = "drop"),
    by = "spa_group"
  ) %>%
  mutate(loc_rate = loc_n / loc_n_tot) %>%
  # 补全所有分类组合，并去除不必要的行。
  complete(ds_cat, vis_src, season, spa_group) %>%
  filter(
    !(
      ds_cat %in% c(
        "ds_tour_mix", "ds_accomfood_mix"
      ) &
        vis_src == "local"
    ),
    !(
      ds_cat %in% c(
        "ds_health", "ds_gov", "ds_edu"
      ) &
        vis_src == "tourist"
    )
  ) %>%
  # 作图。
  ggplot(aes(spa_group, ds_cat)) +
  # 设置格子边框
  geom_tile(aes(fill = loc_rate), color = "black") +
  # 关键修改 1：消除坐标轴两侧的空白间隙，让边框贴齐
  scale_x_discrete(expand = c(0.08, 0.08), labels = str_to_title) +
  scale_y_discrete(
    expand = c(0.08, 0.08),
    labels = ds_label
  ) +
  # 设置颜色和NA值。
  scale_fill_gradient(
    low = "yellow", high = "darkred", na.value = "white",
    name = "Location\nPercentage"
  ) +
  labs(x = "Location cluster", y = "Service") +
  theme_bw() +
  # 关键修改 2：调整主题，移除多余的外框冲突
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    panel.grid = element_blank(),
    # 移除 theme_bw 默认的面板边框，防止双重线
    panel.border = element_blank(),
    # 移除坐标轴线。
    axis.line = element_blank(),
    # 如果有分面标题，去掉其背景边框
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.ticks = element_blank()
  ) +
  facet_grid(
    vis_src ~ season, scale = "free_y",
    labeller = labeller(season = c(
      "1" = "Quater 1", "2" = "Quater 2", "3" = "Quater 3", "4" = "Quater 4"
    ))
  )
dev.off()

# Network index ----
net_index <- read.csv("data_raw/net_index.csv") %>%
  tibble()

# 只分析本地人和外地人的话。
net_index %>%
  pivot_longer(
    cols = -c(vis_src, season), names_to = "index_cat", values_to = "index_val"
  ) %>%
  ggplot() +
  geom_col(aes(season, index_val)) +
  facet_grid(
    index_cat ~ vis_src, scales = "free",
    labeller = labeller(
      vis_src = as_labeller(
        c("local" = "Local", "tourist" = "Tourist", "all_src" = "All visitor")
      ),
      index_cat = as_labeller(
        function(x) {
          x <- gsub("_", "\n", x)
          x <- stringr::str_to_title(x)
          gsub("Avg", "Average", x)
        }
      )
    )
  ) +
  theme_bw() +
  labs(x = "Quarter", y = "Index value")

# Node index ----
# 直方图。
lapply(
  c("indegree", "closeness", "betweeness"),
  function(index_x) {
    combined_data %>%
      filter(vis_src != "local_tourist") %>%
      ggplot() +
      geom_histogram(aes(get(index_x))) +
      facet_grid(vis_src ~ season) +
      theme_bw() +
      labs(x = index_x)
  }
)
# 密度图。
lapply(
  c("indegree", "closeness", "betweeness"),
  function(index_x) {
    combined_data %>%
      filter(vis_src != "local_tourist") %>%
      ggplot() +
      geom_density(aes(get(index_x))) +
      facet_grid(vis_src ~ season) +
      theme_bw() +
      labs(x = index_x)
  }
)

# Abstract subnet ----
# 暂时注释：依赖 modularity_class，需重新从 Gephi 导出 CSV 后启用。
# 根据特定指标，提取各客源各季节点最多的模块中最重要的节点并作图。
# plt_abs_subnet <- function(vis_src_x, index_x) {
  # 首先筛选每个分组特定指标值最大的4个点。
#   top_points <-
#     combined_data %>%
#     filter(vis_src == vis_src_x) %>%
    # 选出节点数最多的module。
    # Bug: 目前仅针对一种指标。
#     filter(top_mod == 1) %>%
#     select(
#       vis_src, season, modularity_class, id, longitude, latitude, all_of(index_x)
#     ) %>%
#     group_by(vis_src, season, modularity_class) %>%
    # Bug: 当出现并列排名的多个点时，强制返回其中一个。
#     slice_max(., order_by = get(index_x), n = 4, with_ties = FALSE) %>%
    # 将4个点按相对位置分配到正方形4个端点：先分左右，再分上下。
#     mutate(x_posi = ifelse(
#       rank(longitude, ties.method = "first") <= 2, "left", "right"
#     )) %>%
#     group_by(vis_src, season, modularity_class, x_posi) %>%
#     mutate(y_posi = ifelse(
#       rank(latitude, ties.method = "first") <= 1, "bottom", "top"
#     )) %>%
#     ungroup() %>%
#     mutate(node_pos = paste0(y_posi, "_", x_posi)) %>%
    # 添加正方形端点坐标。
#     left_join(
#       tibble(
#         node_pos = c("top_left", "top_right", "bottom_left", "bottom_right"),
#         square_x = c(0.1, 0.9, 0.1, 0.9),
#         square_y = c(0.9, 0.9, 0.1, 0.1)
#       ),
#       by = "node_pos"
#     ) %>%
    # 添加季节-组团编号。
#     mutate(season_mod = paste0(season, "-", modularity_class))

  # Bug: 需要还原loc_x_y数据。
#   loc_x_y <- top_points %>%
#     select(vis_src, season, modularity_class, id, square_x, square_y) %>%
#     mutate(id = as.character(id), season = as.character(season)) %>%
#     distinct()
  # 各个节点在不同客源和季节中所属的module。
#   belong_mod <- loc_x_y %>%
#     select(vis_src, season, modularity_class, id) %>%
#     distinct() %>%
#     mutate(season = as.character(season))

#   flow_sub <- od %>%
    # 汇总计算流量。
#     group_by(source, qua, origin, destination) %>%
#     summarise(flow = n(), .groups = "drop")
#   flow_all <- od %>%
    # 汇总计算流量。
#     group_by(qua, origin, destination) %>%
#     summarise(flow = n(), .groups = "drop") %>%
#     mutate(source = "local_tourist")
#   flow <- bind_rows(flow_sub, flow_all) %>%
    # 加入所属module信息。
    # 对起点。
#     left_join(belong_mod, by = c(
#       "source" = "vis_src", "qua" = "season", "origin" = "id"
#     )) %>%
#     rename("origin_mod" = "modularity_class") %>%
#     filter(!is.na(origin_mod)) %>%
    # 对终点。
#     left_join(belong_mod, by = c(
#       "source" = "vis_src", "qua" = "season", "destination" = "id"
#     )) %>%
#     rename("destination_mod" = "modularity_class") %>%
#     filter(!is.na(destination_mod)) %>%
    # 只保留起点和终点属于同一个module的数据。
#     filter(origin_mod == destination_mod) %>%
    # 加入坐标信息。
#     left_join(loc_x_y, by = c(
#       "origin" = "id", "source" = "vis_src", "qua" = "season",
#       "origin_mod" = "modularity_class"
#     )) %>%
#     rename(x_from = square_x, y_from = square_y) %>%
#     left_join(loc_x_y, by = c(
#       "destination" = "id", "source" = "vis_src", "qua" = "season",
#       "destination_mod" = "modularity_class"
#     )) %>%
#     rename(x_to = square_x, y_to = square_y) %>%
    # 去除自己流向自己的部分。
#     filter(!c(x_from == x_to & y_from == y_to))

  # 作图。
#   top_points_plt <- ggplot() +
    # 添加箭头。
#     geom_curve(
      # Bug: 只做本地人。
#       data = flow %>%
#         filter(source == vis_src_x) %>%
#         mutate(season_mod = paste0(qua, "-", origin_mod)),
#       aes(
#         x = x_from, y = y_from,
#         xend = x_to, yend = y_to,
#         linewidth = flow # 箭头粗细代表流量
#       ),
      # 箭头顺时针方向。
#       curvature = -0.16,
#       alpha = 0.5
      # arrow = arrow(
      #   type = "closed",
      #   length = unit(0.1, "cm"),
      #   angle = 20
      # )
#     ) +
    # 定义边界。
#     lims(x = c(-0.5, 1.5), y = c(-0.5, 1.5)) +
    # 绘制节点（大小反映指标1的值）
#     geom_point(data = top_points,
#                aes(x = square_x, y = square_y), fill = "pink",
#                shape = 21, color = "black", stroke = 1, size = 7) +
    # 添加节点标签
#     geom_text(
#       data = top_points, aes(x = square_x, y = square_y, label = id), size = 3
#     ) +
#     facet_wrap(.~ season_mod, nrow = 4) +
#     theme_minimal() +
#     theme(
#       panel.grid = element_blank(),
#       axis.text = element_blank()
#     ) +
#     scale_linewidth_continuous(range = c(0.5, 3)) +
#     labs(x = NULL, y = NULL)

  # 返回结果。
#   return(list(top_points, top_points_plt))
# }
# plt_abs_subnet("local_tourist", "weighted_indegree")

# 对所有客源和网络指标的组合进行作图。
# abs_subnet_comb <-
#   expand.grid(
#     c("local", "tourist", "local_tourist"),
#     c(
#       "indegree", "closness_centrality", "betweeness_centrality"
      # "outdegree", "degree",
      # "weighted_indegree", "weighted_outdegree", "weighted_degree",
      # "eccentricity",
      # "harmonicclosness_centrality"
#     )
#   ) %>%
#   rename_with(~ c("vis_src", "index"))

# map2(
#   abs_subnet_comb$vis_src %>% as.character(),
#   abs_subnet_comb$index %>% as.character(),
#   function(x, y) {
#     png(
#       paste0("data_proc/abs_subnet/", x, "_", y, ".png"),
#       width = 1500, height = 1500, res = 200
#     )
#     plt_abs_subnet(x, y)[[2]] %>% print()
#     dev.off()
#   }
# )

# Results ----
# 计算供需指数时的服务-中心度对应关系。
# 读取数据
library(readxl)
df <- read_excel("data_proc/table_demand_supply_calc.xlsx") %>%
  # 填充Visitor group列
  fill(`Visitor group`, .direction = "down") %>%
  # 转换为长格式
  pivot_longer(
    cols = c(Degree, Closeness, Harmonic),
    names_to = "centrality",
    values_to = "weight"
  ) %>%
  # 设置因子顺序
  mutate(
    centrality = factor(centrality, levels = c("Degree", "Closeness", "Harmonic"))
  )

# 图1: 热力图 (Heatmap)
ggplot(df, aes(x = centrality, y = Service, fill = weight)) +
  geom_tile(color = "white", linewidth = 1) +
  # 添加数值标签
  geom_text(
    aes(label = ifelse(!is.na(weight), sprintf("%.1f", weight), "")),
    col = "white"
  ) +
  facet_wrap(.~ `Visitor group`, ncol = 1, scale = "free_y") +
  # 颜色设置
  scale_fill_gradient(
    low = "#FFF5EB", high = "#8B0000", na.value = "white",
    limits = c(0, 1), name = "Weight"
  ) +
  labs(y = NULL) +
  theme_bw()
# 图2: 圆圈图 (Circle Plot)
ggplot(df %>% filter(!is.na(weight)), aes(x = centrality, y = Service)) +
  geom_point(size = 3) +
  facet_wrap(.~ `Visitor group`, ncol = 1, scale = "free_y") +
  labs(y = NULL) +
  theme_bw()

# Export ----
# 各群体各季节中心度原始数据。
loc %>%
  st_drop_geometry() %>%
  left_join(
    combined_data %>% filter(vis_src %in% c("local", "tourist")),
    by = c("loc_id" = "id")
  ) %>%
  select(
    "loc_id", "spa_group", "vis_src", "season",
    "degree", "closeness", "harmonic"
  ) %>%
  write.csv("data_proc/loc_centrality_raw.csv")

# 各群体各季节中心度按地区汇总数据。
loc %>%
  st_drop_geometry() %>%
  left_join(
    combined_data %>% filter(vis_src %in% c("local", "tourist")),
    by = c("loc_id" = "id")
  ) %>%
  select(
    "loc_id", "spa_group", "vis_src", "season",
    "degree", "closeness", "harmonic"
  ) %>%
  group_by(vis_src, spa_group, season) %>%
  summarise(
    across(
      .cols = c(degree, closeness, harmonic),
      .fns = list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        # 计算四分位数。
        q25 = ~ quantile(.x, probs = 0.25, na.rm = TRUE),
        q50 = ~ quantile(.x, probs = 0.50, na.rm = TRUE), # 中位数
        q75 = ~ quantile(.x, probs = 0.75, na.rm = TRUE)
      ),
      # 定义新列名格式：原始列名_函数名
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) %>%
  write.csv("data_proc/loc_centrality_summary.csv")

# 可达性原始数据。
loc %>%
  st_drop_geometry() %>%
  left_join(loc_poi_access, by = "loc_id") %>%
  select(loc_id, spa_group, all_of(access_cols)) %>%
  write.csv("data_proc/loc_accessibility_raw.csv")

# 可达性汇总数据。
loc %>%
  st_drop_geometry() %>%
  left_join(loc_poi_access, by = "loc_id") %>%
  select(loc_id, spa_group, all_of(access_cols)) %>%
  group_by(spa_group) %>%
  summarise(
    across(
      .cols = all_of(access_cols),
      .fns = list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        # 计算四分位数。
        q25 = ~ quantile(.x, probs = 0.25, na.rm = TRUE),
        q50 = ~ quantile(.x, probs = 0.50, na.rm = TRUE), # 中位数
        q75 = ~ quantile(.x, probs = 0.75, na.rm = TRUE)
      ),
      # 定义新列名格式：原始列名_函数名
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) %>%
  write.csv("data_proc/loc_accessibility_summary.csv")

# 供需指数原始数据。
loc_dem_sup %>%
  select(vis_src, id, , spa_group, season, ds_cat, ds_val) %>%
  write.csv("data_proc/loc_supplydemand_raw.csv")

# 供需指数汇总计算。
loc_dem_sup %>%
  select(vis_src, id, , spa_group, season, ds_cat, ds_val) %>%
  group_by(vis_src, spa_group, season, ds_cat) %>%
  summarise(
    across(
      .cols = ds_val,
      .fns = list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        # 计算四分位数。
        q25 = ~ quantile(.x, probs = 0.25, na.rm = TRUE),
        q50 = ~ quantile(.x, probs = 0.50, na.rm = TRUE), # 中位数
        q75 = ~ quantile(.x, probs = 0.75, na.rm = TRUE)
      ),
      # 定义新列名格式：原始列名_函数名
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) %>%
  write.csv("data_proc/loc_supplydemand_summary.csv")
