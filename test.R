p <-
  ggplot(data = NULL, aes(0, 1:10)) +
  geom_point(alpha = 0) +
  geom_vline(aes(xintercept = 0),linetype = 4, linewidth = 1, color = "red") +
  scale_y_continuous(breaks = 1:10) +
  # scale_x_continuous(limits = c(-0.25, 0.25), expand = c(0, 0)) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  labs(
    x = "Mean"
  )

d <- distr6::Normal$new()
q <- qnorm(1 - 0.05/2)
n <- 200
for (i in seq_len(10)) {
  x <- d$rand(n)
  data <- tibble(
    i = i,
    mean = mean(x),
    var = var(x),
    lower = mean - q*var/sqrt(n),
    upper = mean + q*var/sqrt(n),
    color = if_else(between(0, lower, upper), "green", "red")
  )

  p <-
    p +
    geom_segment(data = data, aes(x = lower, xend = upper, y = i, yend = i),
                 color = data$color, linewidth = 1)
  plot(p)
  Sys.sleep(1)
}
