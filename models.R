plot_usmap(data = states_clean, values = "homeless_rate", regions = "states") +
  scale_fill_gradient(
    name = "Homeless Population",
    low = "lightblue",
    high = "darkblue",
    label = scales::comma  # <- This is the fix!
  ) +
  labs(
    title = "Homelessness rate Population by State",
    subtitle = "Color intensity represents the total homeless population") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    legend.position = "right")


plot_usmap(data = states_clean, values = "homeless", regions = "states") +
  scale_fill_gradient(
    name = "Homeless Population",
    low = "lightblue",
    high = "darkblue",
    label = scales::comma  # <- This is the fix!
  ) +
  labs(
    title = "Homelessness by State",
    subtitle = "Color intensity represents the total homeless population") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    legend.position = "right")


states_clean$funding_per_capita <- states_clean$funding_total / states_clean$population


plot_usmap(data = states_clean, values = "funding_per_capita", regions = "states") +
  scale_fill_continuous(
    name = "Funding per Capita", 
    low = "lightblue",
    high = "darkblue",
    label = label_dollar(scale = 1)  # Makes it show as $X.XX
  ) +
  labs(title = "State Funding per Capita for Homelessness") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    legend.position = "right")


plot_usmap(data = states_clean, values = "funding_total", regions = "states") +
  scale_fill_continuous(
    name = "Funding per Capita", 
    low = "lightblue",
    high = "darkblue",
    label = label_dollar(scale = 1)  # Makes it show as $X.XX
  ) +
  labs(title = "State Funding ") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    legend.position = "right")

