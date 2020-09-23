# DIsegno FLow chart (???) ---------------------------------------------------------------------
# https://mikeyharper.uk/flowcharts-in-r-using-diagrammer/ FLOWCHART
# https://rstudio-pubs-static.s3.amazonaws.com/90261_ad00e95221e14a33a50f2ebb56d34ab8.html




# Pckgs ---------------------------------------------------------------------------------------
if(!require("pacman")){
	install.packages("pacman")
}
library(pacman) # for loading packages
p_load(DiagrammeR, tidyverse, readxl, janitor, ggrepel, tibble, ggpubr)


diagram <- "
sequenceDiagram
  EMPLOYER-->>DOL: STEP 1 - files labor certif application
  Note right of EMPLOYER: ETA Form 9089 etc.
  DOL->>EMPLOYER: approves
  EMPLOYER-->>USCIS: STEP 2 - files immigrant visa petition
  Note right of DOL: Form I-140 etc.
  USCIS->>EMPLOYER: approves
  opt backlog?
  USCIS->>USCIS: depending on country/visa type: can take 10 years!!!
  end
  FOREIGN WORKER-->>USCIS: STEP 3 - files adjustment of status (green card)
  Note right of USCIS: Form I-485 etc.
  USCIS->>FOREIGN WORKER: green card

 "

mermaid(diagram)

# cant figure out how to save



# USCIS ---------------------------------------------------------------------------------------

USCIS  <- read_excel("rawdata/USCIS-processingtime.xlsx", trim_ws = TRUE) %>%
	dplyr::filter (Form == "I-485") %>%
	dplyr::select(-Title , - 9) %>%
	clean_names() %>%
	dplyr::rename( type = classification_or_basis_for_filing )

USCIS$type <- factor (USCIS$type,
							 levels = c("All Other Adjustment of Status", "Based on grant of asylum more than 1 year ago",
							 			  "Based on refugee admission more than 1 year ago", "Employment-based adjustment applications",
							 			  "Family-based adjustment applications"),
							 labels = c("All Other", "Asylum granted",
							 			  "Refugee admission", "Employment-based",
							 			  "Family-based"))

dput(levels(USCIS$type))



# Lne chart -----------------------------------------------------------------------------------
# ---- wide to  Long format
USCIS_Long <- USCIS %>%
	tidyr::gather(key= "Year" , value = "I_485ProcessingTime",
			 fy_2015:fy_2019,
			 na.rm = FALSE)  %>%
	dplyr::select(-form)
library(directlabels)
library(ggrepel)
# ---- plot
plot <- USCIS_Long %>%
	dplyr::mutate(label = if_else(Year == "fy_2019", as.character(type), NA_character_)) %>%
	ggplot() +
	aes(x = Year,  y = I_485ProcessingTime,  color = type) +
	geom_point() +
	geom_line(aes(group = type))  +
	labs(title="USCIS Processing time for Permanent Residence Application (I-485 form)",
		  subtitle = "Since 2015, processing time increased between 30% (Asylum) - and 89% (Employment-Based)",
		  caption = "Source: USCIS",
		  x =NULL, y = "Avg N of months") +
	ggthemes::theme_hc() +
# ylim(0,13 )+
scale_y_continuous ( limits = c( 4, 13) ) +
geom_label_repel(aes(label = label),
					  #nudge_x = 1,
					  na.rm = TRUE) +
	theme(
		plot.title = element_text(size = 13, face = "bold"),
		plot.subtitle = element_text(size = 11), # face = "bold"),
		plot.caption = element_text(size = 10, face = "italic", hjust = 1),
		legend.key.width = unit(3, "line"),
		legend.position = "none"
	)
    ##  theme(text=element_text(family="Garamond", size=14))
	#facet_wrap((~ type))

ggsave( plot, filename =  "gg_USCIStime.png")

# By the way ----------------------------------------------------------------------------------
USCIS %>%
	group_by(type) %>%
	summarise(diff = (fy_2019 - fy_2015)/fy_2015*100)

# Compared to FY2015,  of processing time have increased between 30% (asylum) - and 89% (Employment-Based)
# A tibble: 5 x 2
# type               diff
# <fct>             <dbl>
# 	1 All Other          77.9
# 2 Asylum granted     28.8
# 3 Refugee admission  57.1
# 4 Employment-based   89.2
# 5 Family-based       77.3



# Immigration by category ---------------------------------------------------------------------

# immi_type<- tibble::tribble(
# 	~type, ~N2018, ~Perc,
# 	"immediate relatives", 236526, (236526/533557)*100,
# 	"special/vietnam", 9375+92,  ((9375+92)/533557)*100,
# 	"family sponsored", 211641, (211641/533557)*100,
# 	"employment based", 27345,  (27345/533557)*100,
# 	"diversity", 48578, (48578/533557)*100
# )

# Source ./rawdata/state... + https://www.migrationpolicy.org/article/refugees-and-asylees-united-states#Adjusting_to_Lawful_Permanent_Resident_Status
immi_type <- tibble::tribble(
	~type, ~N2017, ~Perc,
	"immediate relatives", 254430, (254430/705539)*100,
	"special/vietnam",20034+36,  ((20034+36)/705539)*100,
	"family sponsored", 212155, (212155/705539)*100,
	"employment based", 23814,  (23814/705539)*100,
	"diversity", 49067, (49067/705539)*100,
	"refugees", 120356, (120356/705539)*100,
	"asylees", 25647, (25647/705539)*100,
)

immi_type$Perc <- round(immi_type$Perc, 1)
immi_type$type<- as.factor(immi_type$type)
# inputs
qualitative <- c(
	"#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
	"#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a"
)


# Show group names and value as labels
labs <- if_else (immi_type$type == "employment based",
					   paste0(immi_type$type, " (",
					   		 round(immi_type$Perc,digits = 1 ),

					   		 "%)"),
					  ""
	)


p <- ggpie(data = immi_type, x = "Perc", #label = labs,
		#lab.pos = "in", lab.font = "white",lab.adjust = 1,
		fill = "type", color = "white",
		palette = qualitative ,
		title = 'Permanent residency granted by type in 2017 ',
		subtitle = "(Tot = 705,539)")

pie<- ggpar(p, legend = "bottom", legend.title ="Type of green card", font.title =  c(14, "bold"))

ggsave ( pie, filename =  "gg_pie.png")


# gridExtra::grid.arrange(pie, plot, nrow = 1)
