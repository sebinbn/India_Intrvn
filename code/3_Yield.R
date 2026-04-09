# Purpose: 1.Create line chart of 1-year and 10-year government bond yield from 
#             2018-04-01 to 2021-06-30
#          2.Create line chart of 1-year yield and 1yr OIS rate from 
#             2018-04-01 to 2021-06-30 (unused in paper)
#          
# Input:  BBYield
# Output: <output>/Yield_10_1
#         <output>/Yield_OIS_1


# 1. Create yield plot --------------------------------------------------------

# Subset yield data for the analysis period.
PlotDat = BBYield[c("Date", "GIND1Y", "GIND10Y")]
PlotDat[c("GIND1Y", "GIND10Y")] = lapply(PlotDat[c("GIND1Y", "GIND10Y")],
                                         na.locf, na.rm = F)
PlotDat = PlotDat[PlotDat$Date >= as.Date("2018-04-01") &
                    PlotDat$Date <= as.Date("2021-06-30"),] 

# Reshape data for plotting two series with one legend.
PlotDat_long <- rbind(
  data.frame(Date = PlotDat$Date, Tenor = "1Yr", Yield = PlotDat$GIND1Y),
  data.frame(Date = PlotDat$Date, Tenor = "10Yr", Yield = PlotDat$GIND10Y)
)


yield_plot <- ggplot(PlotDat_long, aes(x = Date, y = Yield, colour = Tenor)) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = c("1Yr" = "blue", "10Yr" = "red")) +
  labs(x = NULL, y = "Yield (%)", colour = "Series" ) +
  theme_minimal() +
  theme(axis.text         = element_text(size = 15),
        axis.title        = element_text(size = 17),
        legend.position   = c(0.2, 0.2),
        legend.title      = element_blank(),
        legend.background = element_rect(linetype = "solid", colour = "black"))


filename_path = file.path(OUTPUT,"Yield_10_1.png")
ggsave(filename_path, plot = yield_plot,
       width = 10,  height = 6,  dpi = 300)

message(sprintf("yield_plot saved as %s",
                paste(getwd(), filename_path, sep = "/")
) )



# 2. Create OIS and yield plot --------------------------------------------------------

# Subset yield data for the analysis period.
PlotDat <- merge(BBYield[c("Date", "GIND1Y")], OISYield[c("Date", "IRSW1")],
                 by = "Date")
PlotDat <- PlotDat[PlotDat$Date >= as.Date("2018-04-01") &
                      PlotDat$Date <= as.Date("2021-06-30"),]

# Reshape data for plotting two series with one legend.
PlotDat_long <- rbind(
  data.frame(Date = PlotDat$Date, Tenor = "1Yr", Yield = PlotDat$GIND1Y),
  data.frame(Date = PlotDat$Date, Tenor = "OIS1Yr", Yield = PlotDat$IRSW1)
)


yield_OIS_plot <- ggplot(PlotDat_long, aes(x = Date, y = Yield, colour = Tenor)) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = c("1Yr" = "blue", "OIS1Yr" = "red")) +
  labs(x = NULL, y = "Yield (%)", colour = "Series" ) +
  theme_minimal() +
  theme(axis.text         = element_text(size = 15),
        axis.title        = element_text(size = 17),
        legend.position   = c(0.2, 0.2),
        legend.title      = element_blank(),
        legend.background = element_rect(linetype = "solid", colour = "black"))

filename_path = file.path(OUTPUT,"Yield_OIS_1.png")
ggsave(filename_path, plot = yield_OIS_plot,
       width = 10,  height = 6,  dpi = 300)

message(sprintf("yield_OIS_plot saved as %s",
                paste(getwd(), filename_path, sep = "/")
) )

# Remove temp variables -----------------------------------------------------

rm(PlotDat, PlotDat_long, filename_path)