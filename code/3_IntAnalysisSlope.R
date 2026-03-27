# Purpose: 1.Create line chart of yield curve slope in intervention period and 
#             mark significant event dates.
#          
# Input:  Twist_Dates, MergedDat, Int_101_Cum
# Output: <output>/slope_IntAnalysis.png

# 1. Create slope plot --------------------------------------------------------

# Subset slope data for the analysis period.
PlotDat = MergedDat[c("Date", "s101")]
PlotDat = PlotDat[Period[,"Int"],]

slope_pval = (1-pnorm(abs(Int_101_Cum$coef)/sqrt(diag(Int_101_Cum$var.coef))))*2
D_Ann_pval = slope_pval[grepl("D_Ann_[1-9][0-9]?", names(slope_pval))]

ann_lines = data.frame(
  Date = Twist_Dates$Announcement,
  LineType = ifelse(D_Ann_pval <= 0.05, "Significant", "Not significant"),
  row.names = NULL
)
ann_lines = rbind(ann_lines,
                  data.frame(Date = as.Date("2020-03-26"), LineType = "Stimulus"))
date_labels = rbind(subset(ann_lines, LineType == "Significant"),
                    data.frame(Date = as.Date("2020-03-26"),
                               LineType = "Stimulus"))
date_labels$y = max(PlotDat$s101, na.rm = TRUE)
date_labels$Label = format(date_labels$Date, "%m-%d-%Y")


slope_plot <- ggplot(PlotDat, aes(x = Date, y = s101)) +
  geom_vline(data = ann_lines,
             aes(xintercept = Date, color = LineType, linetype = LineType),
             linewidth = 0.7,  show.legend = FALSE) +
  geom_line(linewidth = 0.9) +
  geom_text(data = date_labels, 
            aes(x = Date, y = y, label = Label, color = LineType),
            inherit.aes = FALSE,  show.legend = FALSE,
            angle = 90, vjust = -0.4, hjust = 1,  size = 4.5) +
  scale_color_manual(
    values = c("Significant" = "forestgreen", "Not significant" = "grey60",
               "Stimulus" = "red")) +
  scale_linetype_manual(
    values = c("Significant" = "solid", "Not significant" = "dashed",
               "Stimulus" = "solid")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%m-%Y") +
  labs(x = NULL, y = "Slope (in bps)" ) +
  theme_minimal() +
  theme(axis.text         = element_text(size = 15),
        axis.title        = element_text(size = 17),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.2))

# 1. Save plot --------------------------------------------------------

filename = "slope_IntAnalysis.png"
ggsave(file.path(OUTPUT, filename), plot = slope_plot,
       width = 10,  height = 6,  dpi = 300)

message(sprintf("slope_plot saved as %s",
                paste(getwd(), OUTPUT, filename, sep = "/")
) )

rm(PlotDat, slope_pval, D_Ann_pval, ann_lines, date_labels, filename)
