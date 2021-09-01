library(tidyverse)

time = c('2Q20', '2Q21')
fin = c(657, 744)

df = data.frame(
  time = seq_along(time),
  fin,
  stringsAsFactors = FALSE
)

black_rectangle = element_rect(fill = 'black', color = 'black')

draw_plot = function(df, y_limits = c(0, 840)) {
  col_rxt = '#43ccb7'
  y_range = range(y_limits)
  nudge_y = round((y_range[2] - y_range[1]) / 14)
  nudge_y_extra = nudge_y / 5
  nudge_x = .20
  label_increase = '13%'
  text_size = 3.5

  df %>%
    ggplot(aes(x = time, y = fin)) +
    geom_col(fill = col_rxt, width = 0.30) +
    geom_text(aes(x = time[1], y = fin[1], label = paste0('$', fin[1])),
              col = 'white', nudge_y = nudge_y, size = text_size) +
    geom_text(aes(x = time[2], y = fin[2], label = paste0('$', fin[2])),
              col = 'white', nudge_y = nudge_y, size = text_size, fontface = 'bold') +
    geom_curve(aes(x = time[1] + nudge_x, xend = time[2] - nudge_x,
                   y = fin[1] + nudge_y + nudge_y_extra, yend = fin[2] + nudge_y - nudge_y_extra),
               arrow = arrow(length = unit(0.25, 'cm'), type = 'open'),
               curvature = 0, size = 0.9, col = col_rxt) +
    geom_label(aes(x = 1.5, y = mean(fin) + nudge_y_extra/2, label = label_increase),
               nudge_y = nudge_y, fill = 'black', col = 'white', size = text_size, label.size = NA, fontface = 'bold') +
    geom_hline(yintercept = y_limits[1], col = 'white') +
    scale_x_continuous(breaks = seq_along(time), labels = time, expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_void() +
    theme(panel.background = black_rectangle) +
    theme(plot.background = black_rectangle) +
    theme(axis.text.x = element_text(color = 'white', size = text_size*2)) +
    theme(title = element_text(color = 'white', face = 'bold', size = text_size * 2.5)) +
    theme(plot.margin = unit(c(0, 1.5, 0, 1.5), "lines")) +
    labs(title = 'Total Revenue') +
    expand_limits(x = c(0.6, 2.4), y = y_limits) +
    coord_cartesian(y = y_limits)
}



best = draw_plot(df)
bad = draw_plot(df, y_limits = c(615, 760))
worst = draw_plot(df, y_limits = c(652, 755))

library(patchwork)


empty_plot = plot_spacer() + theme(plot.background = black_rectangle, plot.margin = unit(c(0, 1.5, 0, 1.5), "lines"))


p = (worst + bad) / (best + empty_plot) +
  plot_layout(tag_level = 'new') +
  plot_annotation(tag_levels = 'A',
                  caption = paste(
                    'Data: Rackspace Q2 Earnings report (https://ir.rackspace.com/static-files/474fde80-f203-4227-a438-57b062992d46, page 11)',
                    'Author: recreated by Paul van der Laken (paulvanderlaken.com) using R ggplot',
                    sep = '\n'),
                  theme = theme(plot.background = black_rectangle,
                                plot.caption = element_text(color = 'lightgrey', face = 'italic'),
                  ))

layout = p & theme(plot.tag = element_text(color = 'red', face = 'italic', size = 16))

w = 8000
h = 3600
dpi = 800

ggsave('rackspace/rackspace_2021q2_barplot.png', plot = layout, width = w, height = h, dpi = dpi, units = 'px')

ggsave('rackspace/rackspace_2021q2_barplot_worst.png', plot = worst, width = w/2, height = h/2, dpi = dpi, units = 'px')
ggsave('rackspace/rackspace_2021q2_barplot_bad.png', plot = bad, width = w/2, height = h/2, dpi = dpi, units = 'px')
ggsave('rackspace/rackspace_2021q2_barplot_best.png', plot = best, width = w/2, height = h/2, dpi = dpi, units = 'px')

