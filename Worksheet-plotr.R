library(ggplot2); library(tidyr)

x11()

form.data = data.frame(ids = 1:10, n.05 = rnorm(10, 5, 25), n.15 = rnorm(10, 5, 10))

colnames(form.data)[2:3] = c("n.05", "n.15")

form.data.flip = form.data |>
  pivot_longer(cols = colnames(form.data)[2:3])

ggplot(data = form.data.flip, aes(x = value)) +
  geom_histogram(bins = 15, fill = "lightblue", col = "black") + 
  facet_wrap(~ name) +
  theme_bw() +
  xlab("Sample Mean Height (in)") +
  ylab("Frequency")