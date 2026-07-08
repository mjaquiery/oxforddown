# Simulation figure (like thesis but with middle facet dropped)

source('scripts_and_filters/general_setup.R')
source('papers/rational-egocentric-discounting/helpers.R')

fName <- "cache/evo-ego-bias.Rdata"
data_url <- "https://zenodo.org/record/5543918/files/evo-ego-bias.Rdata?download=1"

if (!file.exists(fName)) {
  # Download cached data
  curl::curl_download(
    data_url,
    fName
  )
}

load(fName)

d %>% 
  select(-run) %>%
  mutate(
    scenario = str_replace(scenario, "communication", "confusion"),
    scenario = str_to_sentence(scenario),
    scenario = str_replace(scenario, " ", "\n"),
    scenario = factor(
      scenario,
      levels = c("Bad\nadvice", "Noisy\nadvice", "Confidence\nconfusion")
    ),
    across(-c(generation, starts_with("selfWeight")), factor)
  ) %>%
  group_by(generation, corruption, scenario, model) %>%
  filter(scenario != "Noisy\nadvice") %>%
  summarise(across(everything(), mean), .groups = "drop") %>%
  ggplot(aes(
    x = generation, y = selfWeight_mean, fill = corruption, colour = corruption
  )) +
  geom_hline(yintercept = .5, linetype = "dashed") +
  geom_ribbon(aes(ymin = selfWeight_ci95_min, ymax = selfWeight_ci95_max), alpha = .5, colour = NA) +
  stat_summary(geom = "line", fun = mean) +
  facet_grid(scenario ~ .) +
  scale_fill_viridis_d(
    option = "E", 
    name = "Manipulation strength",
    aesthetics = c("colour", "fill")
  ) +
  scale_y_continuous(limits = c(.4, 1)) +
  labs(
    x = "Generation",
    y = "Mean weight on own advice"
  ) +
  broken_axis_bottom

save_figure(filename = "./papers/rational-egocentric-discounting/sim-figure.png")
