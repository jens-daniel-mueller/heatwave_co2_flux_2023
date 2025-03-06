library(marelac)
library(tidyverse)



gas_exchange <- expand_grid(
  temp = seq(0, 30, 1),
  u10 = seq(2, 10, 2),
  method_def = "Wanninkhof2"
)

gas_exchange <-
  gas_exchange %>%
  mutate(
    kw = gas_transfer(
      t = temp,
      u10 = u10,
      species = "CO20",
      method = method_def
      ),
    k0 = gas_solubility(t = temp, species = "CO2"),
    kwk0 = kw * k0
  )


gas_exchange <-
  gas_exchange %>%
  pivot_longer(starts_with("k")) %>%
  group_by(name, u10) %>%
  mutate(value_change = 100 * (value - first(value)) / first(value)) %>%
  ungroup()


gas_exchange %>%
  mutate(u10 = as.factor(u10)) %>%
  ggplot(aes(temp, value_change, col = u10)) +
  geom_hline(yintercept = 0) +
  geom_path() +
  facet_wrap(~ name) +
  scale_y_continuous(breaks = seq(-200,200,10)) +
  theme_bw()
