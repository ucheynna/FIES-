
df <- read.csv("s08a_me_sen2021.csv") %>%
  mutate(hhid = paste0(grappe, "_", menage)) %>%
  select(hhid, s08aq01, s08aq02, s08aq03, s08aq04, s08aq05, s08aq06, s08aq07, s08aq08)

fies_items <- df %>%
  rename_with(~ paste0("fies_", str_remove(., "s08aq")), starts_with("s08aq"))

fies_binary <- fies_items %>%
  mutate(across(starts_with("fies_"), ~ ifelse(. == 1, 1, 0)))

sen_hh <- read.csv("sen_ehcvm2122_hh_info.csv") %>%
  mutate(iso3 = "SEN") %>%
  select(hhid, iso3, survey_wgt)

sen_country <- fies_binary %>%
  left_join(sen_hh, by = "hhid")

XX <- sen_country %>% select(starts_with("fies_"))
wt <- sen_country$survey_wgt

rr <- RM.w(XX, wt)

sen_country$raw_score <- rowSums(XX, na.rm = TRUE)

hh_results <- sen_country %>%
  mutate(
    severity = rr$a[raw_score + 1],
    severity_se = rr$se.a[raw_score + 1],
    prob_insecure = 1 / (1 + exp(-severity))
  )

hh_results_sen <- hh_results %>%
  mutate(
    fies_cat = case_when(
      severity < rr$b["fies_05"] ~ "Mild/None",
      severity >= rr$b["fies_05"] & severity < rr$b["fies_08"] ~ "Moderate",
      severity >= rr$b["fies_08"] ~ "Severe"
    )
  )

sen_wa_mpa_quint <- wa_mpa_hdds %>%
  filter(iso3 == "SEN") %>%
  left_join(hh_results_sen, by = "hhid") %>%
  select(hhid, mpa, sep_quintile, fies_cat)

df <- sen_wa_mpa_quint %>%
  mutate(
    wealth_quintile = factor(sep_quintile, levels = 1:5, ordered = TRUE),
    fies_cat = factor(fies_cat, levels = c("Mild/None", "Moderate", "Severe"), ordered = TRUE)
  )

pal_fies <- c("Mild/None" = "#55A868", "Moderate" = "#EFCB68", "Severe" = "red")

p <- ggplot(df, aes(x = wealth_quintile, y = mpa)) +
  geom_boxplot(width = 0.52, outlier.shape = NA, fill = NA, colour = "grey40", linewidth = 0.4) +
  stat_summary(fun = median, geom = "crossbar", width = 0.52, linewidth = 0.7, colour = "black") +
  geom_jitter(aes(color = fies_cat), width = 0.18, height = 0, size = 1.3, alpha = 0.65, stroke = 0.2) +
  scale_color_manual(name = "FIES category", values = pal_fies, breaks = c("Mild/None", "Moderate", "Severe")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), labels = scales::number_format(accuracy = 0.1)) +
  labs(x = NULL, y = NULL) +  # This line removes the axis labels
  theme_minimal(base_size = 9, base_family = "sans") +
  theme(
    legend.position = "none",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    panel.grid.major.x = element_line(linetype = "dashed", linewidth = 0.3, colour = "grey85"),
    panel.grid.major.y = element_line(linetype = "dashed", linewidth = 0.3, colour = "grey85"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(face = "bold", size = 11),
    plot.title.position = "plot"
  )

ggsave("figure_mpa_fies_quintile_single_square_sen.png", p, width = 150, height = 150, units = "mm", dpi = 300, bg = "white")
