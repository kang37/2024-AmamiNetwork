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
        # 计算四分位数。
        q50 = ~ quantile(.x, probs = 0.50, na.rm = TRUE)
      ),
      # 定义新列名格式：原始列名_函数名
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) %>%
  write.csv("data_proc/loc_centrality_mid.csv")

loc %>%
  st_drop_geometry() %>%
  left_join(loc_poi_access, by = "loc_id") %>%
  select(loc_id, spa_group, all_of(access_cols)) %>%
  group_by(spa_group) %>%
  summarise(
    across(
      .cols = all_of(access_cols),
      .fns = list(
        q50 = ~ quantile(.x, probs = 0.50, na.rm = TRUE)
      ),
      # 定义新列名格式：原始列名_函数名
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) %>%
  write.csv("data_proc/loc_accessibility_mid.csv")

loc_dem_sup %>%
  select(vis_src, id, , spa_group, season, ds_cat, ds_val) %>%
  group_by(vis_src, spa_group, season, ds_cat) %>%
  summarise(
    across(
      .cols = ds_val,
      .fns = list(
        q50 = ~ quantile(.x, probs = 0.50, na.rm = TRUE)
      ),
      # 定义新列名格式：原始列名_函数名
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(vis_src, spa_group, season),
    names_from = ds_cat, values_from = ds_val_q50
  ) %>%
  write.csv("data_proc/loc_supplydemand_mid.csv")
