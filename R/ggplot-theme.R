# LULA's --------------------------------------------------------------------------------------


# ggplot theme --------------------------------------------------------------------------------
# ----- Ensure ggplot2 is loaded
require(ggplot2)
require(grDevices)
require(RColorBrewer)
require(grDevices)

#sysfonts::font_add_google(name = "Roboto Condensed", family =  "Condensed")
sysfonts::font_add(family = "Roboto Condensed", regular =  "~/Applications/Roboto_Condensed/RobotoCondensed-Regular.ttf")

# ----- Define my_theme ------
my_theme <-     # theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
    theme(legend.position = "right",
          #panel.background = element_rect(fill = NA),
          panel.border = element_rect(fill = NA, color = "grey75"),
          axis.ticks = element_line(color = "grey85"),
          axis.text.x = element_text(size = 9,
                                     # inclinato
                                     angle = 45, hjust = 1),
          axis.text.y = element_text(size = 9, face = "bold"), # they are flipped!!!
          panel.grid.major = element_line(color = "grey95", size = 0.2),
          panel.grid.minor = element_line(color = "grey95", size = 0.2),
          # Bold legend titles
          legend.title = element_text(face = "bold"),
          legend.key = element_blank(),
          strip.text = element_text(face = "bold"),
          plot.title = element_text(face = "bold",family = "Roboto")

    )

# color palettes -----
mycolors_gradient <- c("#ccf6fa", "#80e8f3", "#33d9eb", "#00d0e6", "#0092a1")
mycolors_contrast <- c("#E7B800", "#a19100", "#0084e6","#005ca1", "#e60066" )


# ----- To USE: simply add to plot `+ my_theme`


# ggplot functions -------------------------------------------------------------------------------

# ----- 1) FUNC BAR PLOT of FACETED COUNTS BY CATEGORYCAL VARIABLE -------------------------------
facet_count_plot_func <- function(data, x_str, xl, yl, t, subt, palette, factor, lt = "Type Supplier") {
  # I declaring the dataset inside for the project
  ggplot(
    data = data,
    # define aesthetic mappings programmatically!!!!!
    aes_string(x = x_str, fill = factor)
  ) +
    geom_bar(position = position_dodge2(width = 0.9, preserve = "single")) + # avoids variable widthwith dodge
    labs(fill = lt) +
    scale_fill_manual(
      values = palette, # values=c("#00008B", "#BCEE68","#458B74") ,
      na.value = "grey",
      guide = guide_legend(reverse = F)
    ) +
    labs(x = xl) +
    labs(y = yl) +
    labs(title = t, subtitle = subt) +
    facet_grid(paste("~", factor)) + # !!!??? facet_wrap(~factor) # ERROR
    coord_flip() +
    my_theme
}

# ----- To USE: EXAMPLEs
# https://stackoverflow.com/questions/28064774/plot-gradient-colours-of-stack-without-ggplot2
# ----- PALETTE of 3 divergent colors
cols_div_3 <- c(colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(8)[c(1, 4, 8)])
cols_div_3
# ----- PALETTE of 5 sequential values in the blue palette so it is not too clear
cols_blue_seque <- c( colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(6)[3:7]  )
cols_blue_seque


## ----- EXAMPLE 1.1) CALL FUNC with factor =  Q46_CEOelected_Who_bis_F,
# Q46hireCEO_own_plot <- facet_count_plot_func(
# 		  data = Bra1_compl_Gov,
# 		  palette = cols_div_3,
# 		  x_str = "Q46_CEOelected_Who_bis_F",
# 		  xl = "",
# 		  yl = "Count of Suppliers",
# 		  t = "Who elects the CEO/President of the agency?",
# 		  subt = "(Count by election method across Ownership types)",
# 		  factor = "Q1_ownership_lbl"
# )
#
# Q46hireCEO_own_plot
# ggsave(Q46hireCEO_own_plot, filename = here::here("07_output", "Q46hireCEO_own_plot.png"))
#
# # ----- EXAMPLE 1.2) CALL FUNC with factor =  Q46_CEOelected_Who_bis_F,
# # ----- CALL FUNC  Q46_CEOelected_Who_bis_F,
# Q46hireCEO_size_plot <- facet_count_plot_func(
#   data = Bra1_compl_Gov,
#   palette = cols_blue_seque,
#   x_str = "Q46_CEOelected_Who_bis_F",
#   xl = "",
#   yl = "Count of Suppliers",
#   t = "Who elects the CEO/President of the agency?",
#   subt = "(Count by election method across Coverage types)",
#   factor = "Cov_pop_rango_lbl2"
# )
#
# Q46hireCEO_size_plot
# ggsave(Q46hireCEO_size_plot, filename = here::here("07_output", "Q46hireCEO_size_plot.png"))



# ----- 2) FUNC to create scatter plot of 2 Cont Var + BY 1 Cat var -------------------------------
point_plot_func <- function(factor, x_str, y_str, xl, yl, t, rvrs = T, lt = "Factor levels") {
	ggplot(data = Bra1_complete,
			 aes_string(x = x_str, y = y_str, color = factor)) +
		geom_point(size = 3, alpha = 0.5, position = "jitter") +
		xlab(xl) +
		ylab(yl) +
		ggtitle(t) +
		scale_color_brewer(
			palette = "Spectral",
			guide = guide_legend(
				title = lt,
				reverse = rvrs
			)
		) +
		my_theme
}


## ----- EXAMPLE 2.1) CALL FUNC with factor =   Q9a_d_Mix_comb
# dot1 <- point_plot_func(
#   factor = "Q9a_d_Mix_comb",
#   x_str = "Q27_IndivMeter_Perc",
#   y_str = "NRWm3_pct",
#   xl = "Perc Conn with Indiv Meter",
#   yl = "Percent NRW",
#   t = "", # t = "Perc of NRW in system Vs. Perc of Indiv Meters BY Service Mix",
#   rvrs = F
# ) +
#   coord_cartesian(ylim = c(0, 100)) +
#   geom_rect(aes(xmin = 0, xmax = Inf, ymin = 50, ymax = Inf), alpha = 0.005, fill = "red", linetype = "blank")
#
# ggsave(dot1, filename = here::here("07_output", "Plot_DotsNWR_Meter_byMIX.png"))


# ----- 3) FUNC to create box plot object 1 Cont Var + BY 1 Cat var -------------------------------
box_plot_func <- function(factor,
								  lt = "Factor levels",
								  x_str = "quality.rank",
								  y_str,
								  xl = "Quality rank",
								  yl,
								  t){
	ggplot(data = Bra1_complete, aes_string(x=x_str, y = y_str, color=factor)) +
		geom_boxplot() +
		xlab(xl) +
		ylab(yl) +
		ggtitle(t) +
		my_theme
}

# ----- EXAMPLE 3.1) call Func :   NRWm3_pct BY Q41_ClandConn_Analyzed
# box_plot_func(factor = NULL,
# 				  x_str = "Q41_ClandConn_Analyzed",
# 				  y_str = "NRWm3_pct",
# 				  xl = "Is there a process to detect informal connections?",
# 				  yl = "Percent NRW",
# 				  t = "Perc of NRW vs. Having a system to detect IRREGULAR CONNECTIONS") +
# 	coord_cartesian(ylim=c(0,100)) +
# 	geom_rect(aes(xmin=0,xmax=Inf,ymin=50,ymax=Inf), alpha=0.005,fill="red", linetype="blank") -> box1
#
# # -----save
# ggsave(box1, filename=here::here("07_output", "Plot_BOXNWR_Cland.png"))
