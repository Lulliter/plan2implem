# Pckgs ---------------------------------------------------------------------------------------
packages <- c("here",
             "tidyverse",
             "magrttr",
             "lubridate",
             "janitor",
             "assertr",
             "paint",
             "skimr",
             "scales",
             "plotly",
             "ggiraph",
             "patchwork",
             "reactable",
             "htmltools"
)

for (package in packages) {
    args <- list(character.only = TRUE, warn.conflicts = FALSE, quietly = TRUE)
    if(!do.call(require, c(package, args))) {
        install.packages(package)
        do.call(require, c(package, args))
    }
}

# turn off scientific notaton
options(scipen = 999) #  to turn back on options(scipen = 0)

# Functions -----------------------------------------------------------------------------------
# source(here::here("R", "helpers.R"))
#source(here::here("R", "ggplot-theme.R"))

# color palettes -----
mycolors_gradient <- c("#ccf6fa", "#80e8f3", "#33d9eb", "#00d0e6", "#0092a1")
mycolors_contrast <- c("#E7B800", "#a19100", "#0084e6","#005ca1", "#e60066" )

# load data  --------------------------------------------------------------

# [NOPE] the FULL ESIF data
# # This is ......
# ESIF_2014_20_plan2imp <- readRDS("~/Github/plan2implem/data/rawdata_oct2022/ESIF_2014_20_plan2imp.Rds")
# # This is ......
# ESIF_2014_20_fin <- readRDS("~/Github/plan2implem/data/rawdata_oct2022/ESIF_2014_20.Rds")

# Filter ***ERDF*** & ***ITALY*** only --------------------------------------------------------
# [YEP] the small ERDF & ITA subset data

# This is ...... 253
ERDF_plan <- readRDS("~/Github/plan2implem/data/rawdata_oct2022/ERDF_plan.Rds")
# This is ...... 1887
ERDF_imp <- readRDS("~/Github/plan2implem/data/rawdata_oct2022/ERDF_imp.Rds")
# This is ......23311
ERDF_plan2imp <- readRDS("~/Github/plan2implem/data/rawdata_oct2022/ERDF_plan2imp.Rds")

# OK ......
categ <- readRDS("~/Github/plan2implem/data/rawdata_oct2022/categ.Rds") # 322
indic <- readRDS("~/Github/plan2implem/data/rawdata_oct2022/indic.Rds") # 30886
indic <- indic %>%
    filter(ms == "IT")
# just name
rm( args)

# --- --- --- --- --- ------ --- ------ --- ------ --- ------ --- ------ --- --- #
# WHAT DATA ARE WE LOOKING AT?  --------------------------------------
# --- --- --- --- --- ------ --- ------ --- ------ --- ------ --- ------ --- --- #

# From ESIF --> ERDF fund subset
# Only for ms == Italy
# Data are available in three financial datasets related to planned, implemented
# and payed resources and on a single achievement dataset with
# data on selected common indicators targets and implementation.
# These data are available disaggregated by fund, Operational Programme, Priority Axis, Thematic140
# Objectives (i.e. the macro priorities of investment of the policy) and category of regions
# (more developed, less developed, transition)

# --- --- --- --- --- ------ --- ------ --- ------ --- ------ --- ------ --- --- #
# 1) ERDF_plan [4v6-qrrq] Finance Planning Details -------------------
# INFO HERE https://cohesiondata.ec.europa.eu/2014-2020/ESIF-2014-2020-FINANCES-PLANNED-DETAILS/e4v6-qrrq
# This dataset provides information on ***planned total and EU financing*** under the different ESI Funds (2014-2020) in current prices.
# The data is taken from the adopted financial tables and is broken down by:
#fund,--> programme,--> priority axis,--> thematic objective--> and category of region--> (more developed, less developed, etc. where available). It is updated # daily to reflect any modifications (i.e; thematic reallocations) agreed between the Member States and the Commission.


#dplyr::glimpse(ERDF_imp)
paint(ERDF_plan)

## De-ERDF_plan un-needed stuff -----------------------------------------------------
ERDF_plan <-  ERDF_plan %>%
    select(-eafrd_measure_code, -measure_short_description,
           -eafrd_fa, -focus_area_short_description)

# --- --- --- --- --- ------ --- ------ --- ------ --- ------ --- ------ --- --- #
# 2) ERDF_imp [99js-gm52] Finance Implementation Details -------------------
# INFO HERE https://dev.socrata.com/foundry/cohesiondata.ec.europa.eu/99js-gm52
# https://cohesiondata.ec.europa.eu/2014-2020-Finances/ESIF-2014-2020-Finance-Implementation-Details/99js-gm52
# Finance Implementation Details
# FAQs: https://ec.europa.eu/regional_policy/en/faq/about_open_data/#8
#
# This data set provides **time series information on the financial implementation** on the ground of the 530+ ESI Funded # programmes.
# Each row compares a planned allocation to a detailed category (by country, programme, fund, priority axis, category of region) to the reported value of selected projects (operations) and to the expenditure reported by those projects.-> different years MUST NOT BE AGGREGATED

#dplyr::glimpse(ERDF_imp)
paint(ERDF_imp)

## De-Select un-needed stuff -----------------------------------------------------
ERDF_imp <-  ERDF_imp %>%
    select(-eafrd_measure, -measure_short_description,
           -eafrd_fa, -focus_area_short_description)
paint(ERDF_imp)

# ## Check Unique -- ID `cci` ---------------------------------------------------------
# # assert + not_na
# ERDF_imp %>%  assert(not_na, cci  ) %>% summarise(N = n_distinct(cci)) #   OK 30
#
# # assert + is_uniq
# ERDF_imp %>%  assert(is_uniq, cci  ) %>% summarise(N = n_distinct(cci)) # not unique... of course bc it is cumulative data!
#
# ## Check Unique -- 'reference_date' ---------------------------------------------------------
# # assert + not_na
# ERDF_imp %>%  assert(not_na, reference_date  ) %>% summarise(N = n_distinct(reference_date))#   has na
# table(ERDF_imp$reference_date, ERDF_imp$year, useNA = "ifany")
# # replace missing date
# ERDF_imp <- ERDF_imp %>%
#     dplyr::mutate(reference_date = replace_na(reference_date, as.Date("2014-12-31",format = '%d-%m-%Y')))
# # assert + is_uniq
# ERDF_imp %>%  assert(is_uniq, reference_date) %>%
#     summarise(N = n_distinct(reference_date))#  not unique!
# table( ERDF_imp$reference_date)
# table(ERDF_imp$cci)
# table(ERDF_imp$cci, ERDF_imp$reference_date) # At any given point, I have multiple instances of 1 cci bc THESE DATA ARE CUMULATIVE TIME SERIES

# I eliminated the June 30 instances (to keep it clean and yearly progress)
ERDF_imp  <- ERDF_imp %>%
    filter( reference_date != "2014-09-30") %>% # half years
    filter( reference_date != "2015-06-30") %>%
    filter( reference_date != "2016-06-30") %>%
    filter( reference_date != "2017-06-30") %>%
    filter( reference_date != "2018-06-30") %>%
    filter( reference_date != "2019-06-30") %>%
    filter( reference_date != "2020-06-30") %>%
    filter( reference_date != "2021-06-30") %>%
    filter( reference_date != "2022-06-30")  %>%  #16732 -> 13803
    # just Italy
    filter(ms == "IT") # --> 1467

table( ERDF_imp$reference_date)

## Check Unique -- THEME 'to' | to_short |  to_long---------------------------------------------------------
ERDF_imp %>% skim (tidyselect::all_of(c("cci", "to", "to_short","to_long"))) %>%
    tibble::as_tibble() %>% dplyr::select(skim_variable, n_missing  , n_unique =character.n_unique)

## Check Unique -- priority  'priority' --------------------------------------------------------
ERDF_imp %>% skim (tidyselect::matches("priority")) %>%
    tibble::as_tibble() %>% dplyr::select(skim_variable, n_missing  , n_unique =character.n_unique)

# look_fin <-  ERDF_imp %>%
#     select(cci, priority, to_short, reference_date, year, total_amount, eu_amount, national_amount, total_eligible_cost, total_eligible_expenditure ) %>%
#     filter(cci == "2014IT16M2OP001") %>%
#     arrange(cci, to_short, year)
#
#     #view(look_fin)
#
# count_fin <- ERDF_imp %>%
#     group_by(cci) %>%
#     summarise(N_row_percci = n(),
#               #n_uniq_prio = n_distinct(priority), just like theme
#               n_uniq_theme = n_distinct(to_short),
#               n_uniq_reg = n_distinct(category_of_region),
#               n_uniq_year = n_distinct(year),
#               n_uniq_total_amount = n_distinct(total_amount),
#               n_uniq_total_eligible_cost = n_distinct(total_eligible_cost)
#     )
# #view(count_fin)


## Understand variables ----------------------------------------------------
# https://dev.socrata.com/foundry/cohesiondata.ec.europa.eu/99js-gm52

# ----- category_of_region
# The three main categories of region are: [less developed, transition, and more developed] + "Outermost or Northern Sparsely Populated" regions
# ?? What are REACT-EU ???
# ?? What are VOID ???
skimr::skim(ERDF_imp)
table(ERDF_imp$category_of_region, useNA = "ifany")

# give easy labels...
ERDF_imp$region <- factor(ERDF_imp$category_of_region,
                                          levels = c("Less developed", "More developed","Transition",
                                                     "Outermost or Northern Sparsely Populated",
                                                     "VOID",
                                                     "REACT-EU" ),
                                          labels = c("LessDev", "MoreDev","Transition",
                                                     "Remote",
                                                     "InterRegio", # VOID
                                                     "REACT"  ))

table(ERDF_imp$region, useNA = "ifany")

# ----- theme
table( ERDF_imp$to)
table( ERDF_imp$to_short)

## Inpute missing reference date  Datatypes ---------------------------------------------------------
table(ERDF_imp$year, ERDF_imp$reference_date )
table( ERDF_imp$reference_date, useNA = "ifany" )

# ? RESHAPE DATA TO LONG?  (it's already (mostly) in LONG form !!!!!)

## Important $ VARIABLES -------------------------------------------------
# ALLOCATED
# total_amount
#   eu_amount
#   national_amount (co-financing)
# v
#
# total_eligible_cost ?? (Total amount (EU+National) allocated to the projects (operations) selected by the programme managers)
#
# v
# total_eligible_expenditure

## Attaccare quel che serve -----------
ERDF_imp$Perc_spent <- ERDF_imp$total_eligible_expenditure/ERDF_imp$total_eligible_cost


# --- --- --- --- --- ------ --- ------ --- ------ --- ------ --- ------ --- --- #
# Plot time series - planned/done by ERDF region  ---------------------------------------
# --- --- --- --- --- ------ --- ------ --- ------ --- ------ --- ------ --- --- #

### check and prep data set -------------------------------

### gather FINANCIAL vars in one col --> longER shape -------------------------------
ERDF_imp_L <- ERDF_imp %>%
    # MOVE SO THAT ALL FIN VARS ARE CLOSE
    relocate (year, .after = category_of_region) %>%
    relocate (eu_co_financing, .after = year) %>%
    relocate( Perc_spent , .after = total_eligible_expenditure) %>%
    pivot_longer(cols = eu_amount:Perc_spent,
                 names_to = "fin_vars",
                 values_to = "fin_values"
    ) %>%
    select(#-eu_amount ,
        #-national_amount,
        #-total_amount,
        #-total_eligible_cost,
        #-total_eligible_expenditure,
        -ir_last_version)

### Filtering out unneeded stuff-------------------------------
ERDF_imp_L_plot <- ERDF_imp_L %>%
    # deselect empty OR uniteresting
    filter (!year %in% c("2014" ) ) %>%
    filter (!to_short %in% c("VOID", "Multiple Thematic Objectives (ERDF/CF/ESF)",
                             "Technical Assistance") ) %>%

    # chose less vars/vals
    filter (fin_vars %in% c( "total_amount",
                             "total_eligible_cost", "total_eligible_expenditure")) %>%
    mutate(year = as.numeric(as.character(year)))


unique(ERDF_imp_L_plot$region)

### >>PLOT1: facet multiples plots over time by region -------------------------------
p1 <- ggplot(data = ERDF_imp_L_plot,
                mapping = aes(x = year, y = fin_values, fill = fin_vars)) +
    #geom_col(size = 1) +
    geom_col(position = "dodge") +
    # custom color
    scale_fill_manual(values = mycolors_contrast) +
    facet_wrap(vars(region)) +
    labs(x = NULL,
         y = NULL,
         title = "ERDF 2014-2020: funds implementation in Italy by region",
         subtitle = "Data updated on 28 October, 2022",
         caption = "Source: ESIF 2014-2020 Finance Implementation Details (99js-gm52)") +

    facet_wrap(~region, ncol =3,
               labeller = labeller(region = label_wrap_gen(width = 20))) +
    # change format for y axis (--> euro)
    scale_y_continuous(labels= scales::label_number(scale = 1/1000000,
                                                    prefix = "€",
                                                    suffix = " mln",
                                                    big.mark = ".",
                                                    decimal.mark = ",")) +
    # see all the breaks in x axis (year)
    scale_x_continuous(n.breaks = 7
                       #limit = c("2015", "2016", "2017","2018", "2019", "2020",  "2021")
    ) +
    # theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
    theme(legend.position = "bottom",
          panel.background = element_rect(fill = NA),
          panel.border = element_rect(fill = NA, color = "grey75"),
          axis.ticks = element_line(color = "grey85"),
          axis.text.x = element_text(size = 9,
                                     # inclinato
                                     angle = 45, hjust = 1),
          axis.text.y = element_text(size = 9, face = "bold"), # they are flipped!!!
          panel.grid.major = element_line(color = "grey95", size = 0.2),
          panel.grid.minor = element_line(color = "grey95", size = 0.2),
          strip.text = element_text(face = "bold"),
          plot.title = element_text(face = "bold" ),
          # Bold legend titles
          legend.title = element_text(face = "bold"),
          # legend.title = element_blank(), # to remove it
          legend.key = element_blank()) +
    guides(fill=guide_legend(title="Financial Values"))

#ggplotly(p1)

## save Plot 1 -------------------------------------------------------------
p1 %T>%
    print() %T>%
    ggsave(., filename = here("fig",  "ERDF_eligVspent_reg_time_facet.pdf") ,
           #width = 4, height = 2.25, units = "in",
           device = cairo_pdf
           ) %>%
    ggsave(., filename = here("fig",  "ERDF_eligVspent_reg_time_facet.png"),
           #width = 4, height = 2.25, units = "in",
           #type = "cairo",
           dpi = 300)

# >>>>>>>>> NEXT  - SME interactive  plot --------------------------------------------------------
# drill down to Competitiveness theme
# plot interactive with the tooltips telling the % spent

# Plot time series - planned/done by ERDF theme  ---------------------------------------

### >>PLOT2a: facet multiples plots over time by themes -------------------------------
p2 <- ggplot(data = ERDF_imp_L_plot,
             mapping = aes(x = year, y = fin_values, fill = fin_vars )
) +
    #geom_col(size = 1) +
    geom_col(position = "dodge") +
    # custom color
    scale_fill_manual(values = mycolors_contrast) +
    facet_wrap(vars(to_short)) +
    labs(x = NULL,
         y = NULL,
         title = "ERDF 2014-2020: funds implementation in Italy by theme",
         subtitle = "Data updated on 28 October, 2022",
         caption = "Source: ESIF 2014-2020 Finance Implementation Details (99js-gm52)") +
    facet_wrap(~to_short, ncol =3,
               labeller = labeller(to_short = label_wrap_gen(width = 20))) +
    # change format for y axis (--> euro)
    scale_y_continuous(labels= scales::label_number(scale = 1/1000000,
                                                    prefix = "€",
                                                    suffix = " mln",
                                                    big.mark = ".",
                                                    decimal.mark = ",")) +
    # see all the breaks in x axis (year)
    scale_x_continuous(n.breaks = 7
                       #limit = c("2015", "2016", "2017","2018", "2019", "2020",  "2021")
    ) +
    # theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
    theme(legend.position = "bottom",
          panel.background = element_rect(fill = NA),
          panel.border = element_rect(fill = NA, color = "grey75"),
          axis.ticks = element_line(color = "grey85"),
          axis.text.x = element_text(size = 9,
                                     # inclinato
                                     angle = 45, hjust = 1),
          axis.text.y = element_text(size = 9, face = "bold"), # they are flipped!!!
          panel.grid.major = element_line(color = "grey95", size = 0.2),
          panel.grid.minor = element_line(color = "grey95", size = 0.2),
          strip.text = element_text(face = "bold"),
          plot.title = element_text(face = "bold" ),
          # Bold legend titles
          legend.title = element_text(face = "bold"),
          # legend.title = element_blank(), # to remove it
          legend.key = element_blank()) +
    guides(fill=guide_legend(title="Financial Values"))

## save Plot 2a -------------------------------------------------------------
p2 %T>%
     print() %T>%
    ggsave(., filename = here("fig",  "ERDF_eligVspent_theme_time_facet.pdf"),
           #width = 4, height = 2.25, units = "in",
            device = cairo_pdf  #-- mess with ragg ....
           ) %>%
    ggsave(., filename = here("fig",  "ERDF_eligVspent_theme_time_facet.png"),
           #width = 4, height = 2.25, units = "in",
          # type = "cairo",
           dpi = 300)

### >>PLOT2b: (interactive) facet multiples plots over time by themes -------------------------------
### check and prep data set -------------------------------
unique(ERDF_imp_L$fin_vars)

ERDF_imp_L_plot_int <- ERDF_imp_L %>%
    # filter (cci == "2014IT05M2OP002")
    # create synth VARS to show in girafe
    dplyr::group_by (cci, year) %>%
    mutate (N_proj = n(),
            Avg_EU_cof = mean(eu_co_financing)) %>%
    mutate(AvgPercSpent = ifelse (fin_vars == "Perc_spent",  paste0(round(fin_values*100,1), "%"), "" ) )%>%  # ), .groups = "drop")
    ungroup() %>%
    # avoid error discrete values to continuous scale
    mutate(year = as.numeric(as.character(year))) %>%
# deselect empty OR uniteresting
filter (!year %in% c("2014" ) ) %>%
    filter (!to_short %in% c("VOID", "Multiple Thematic Objectives (ERDF/CF/ESF)",
                             "Technical Assistance") ) %>%

    # chose less vars/vals
    filter (fin_vars %in% c( "total_amount",
                             "total_eligible_cost", "total_eligible_expenditure"))

### plot interactive -------------------------------
paint(ERDF_imp_L_plot_int)
p2_int <- ERDF_imp_L_plot_int  %>%
    ggplot(mapping = aes(x = year, y = fin_values, fill = fin_vars,
                         # interactive part
                         # https://github.com/davidgohel/ggiraph/issues/234
                         group =  fin_vars  ,
                         # group = paste (year, fin_vars ),
                        tooltip = paste0("N. projs ", N_proj)# ,

                        # tooltip = paste0("Perc_spent ", AvgPercSpent)# , DOESNT WORK!
                         #data_id = year
    )) +
    #geom_col(size = 1) +
    geom_col_interactive(position = "dodge") +
    # custom color
    scale_fill_manual(values = mycolors_contrast) +
    #facet_wrap(vars(to_short)) +
    labs(x = NULL,
         y = NULL,
         title = "ERDF 2014-2020: funds implementation in Italy by theme",
         subtitle = "Data updated on 28 October, 2022",
         caption = "Source: ESIF 2014-2020 Finance Implementation Details (99js-gm52)") +
    facet_wrap(~to_short, ncol =3,
               labeller = labeller(to_short = label_wrap_gen(width = 20))) +
    # change format for y axis (--> euro)
    scale_y_continuous(labels= scales::label_number(scale = 1/1000000,
                                                    prefix = "€",
                                                    suffix = " mln",
                                                    big.mark = ".",
                                                    decimal.mark = ",")) +
    # see all the breaks in x axis (year)
    scale_x_continuous(n.breaks = 7
                       #limit = c("2015", "2016", "2017","2018", "2019", "2020",  "2021")
    ) +
    # theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
    theme(legend.position = "bottom",
          panel.background = element_rect(fill = NA),
          panel.border = element_rect(fill = NA, color = "grey75"),
          axis.ticks = element_line(color = "grey85"),
          axis.text.x = element_text(size = 9,
                                     # inclinato
                                     angle = 45, hjust = 1),
          axis.text.y = element_text(size = 9, face = "bold"), # they are flipped!!!
          panel.grid.major = element_line(color = "grey95", size = 0.2),
          panel.grid.minor = element_line(color = "grey95", size = 0.2),
          strip.text = element_text(face = "bold"),
          plot.title = element_text(face = "bold" ),
          # Bold legend titles
          legend.title = element_text(face = "bold"),
          # legend.title = element_blank(), # to remove it
          legend.key = element_blank()) +
    guides(fill=guide_legend(title="Financial Values"))


x <- girafe(ggobj = p2_int)
if( interactive() ) print(x)

## save Plot 2b -------------------------------------------------------------
saveRDS(p2_int ,file = "fig/p2_int.Rds" )

# --- --- --- --- --- ------ --- ------ --- ------ --- ------ --- ------ --- --- #
# Interactive Table 2021 - ERDF planned/done by ... ---------------------------------------
# --- --- --- --- --- ------ --- ------ --- ------ --- ------ --- ------ --- --- #

# https://glin.github.io/reactable/articles/twitter-followers/twitter-followers.html


### check and prep data set -------------------------------
# I'll keep only 2021
unique(ERDF_imp_L$year)

ERDF_imp_L_tab_int <- ERDF_imp %>%
    mutate( total_amount_mln = total_amount/1000000,
            total_eligible_cost_mln = total_eligible_cost/1000000,
            total_eligible_expenditure_mln = total_eligible_expenditure/1000000) %>%
    select(theme = to_short,
           unique_programs = cci, title,  priority, region,  reference_date,
           amount_planned = total_amount_mln,
           amount_allocated = total_eligible_cost_mln,
           amount_spent = total_eligible_expenditure_mln) %>%
    mutate( PercSpent = ifelse (amount_spent != 0,
                                 round(amount_spent/amount_allocated,1), 0)) %>%
    # last year only
    filter (reference_date == "2021-12-31") %>%
    select(-priority, -region,  -reference_date)

paint(ERDF_imp_L_tab_int)


# react T (no barcharts) --------------------------------------------------
t_react <- reactable(
    data = ERDF_imp_L_tab_int,
    # formatting
    pagination = FALSE,
    defaultSorted = "amount_planned",
    defaultSortOrder = "desc",
    showSortIcon = TRUE,
    style = list(fontFamily = "Work Sans, sans-serif", fontSize = "0.875rem"),
    # data
    groupBy = c("theme"#, "region"
    ),
    searchable = TRUE,
    columns = list(
        #region = colDef(aggregate = "unique"),
        unique_programs = colDef(name = "N. | Id of Programs",
                                 aggregate = "count",
                                 format = colFormat(#locales = "it-IT",
                                     separators = TRUE, digits = 0 )),
        amount_planned = colDef(name = "Amount Planned",
                                aggregate = "sum",
                                format = colFormat(#locales = "it-IT",
                                    separators = TRUE, digits = 0, prefix = "€", suffix = " mln")),
        amount_allocated = colDef(name = "Amount Allocated",
                                  aggregate = "sum",
                                  format = colFormat(#locales = "it-IT",
                                      separators = TRUE, digits = 0, prefix = "€", suffix = " mln")),
        amount_spent = colDef(name = "Amount Spent", aggregate = "sum",
                              format = colFormat(#locales = "it-IT",
                                  separators = TRUE,
                                  digits = 0, prefix = "€", suffix = " mln")),
        # PercSpent = colDef(name = "Spent/Allocated at 31/12/2021",
        #                    aggregate = "mean",
        #                    format = colFormat(#locales = "it-IT",
        #                        #percent = TRUE,
        #                        digits = 1,suffix = "%",
        #                        separators = TRUE)),
        PercSpent = colDef(name = "Spent/Allocated at 31/12/2021",
                           defaultSortOrder = "desc",
                           format = colFormat(percent = TRUE, digits = 1)
        )))


# react T (YES  barcharts) --------------------------------------------------
### Add mini barcharts ------------------------------------------------------
# Render a bar chart with a label on the left
bar_chart <- function(label, width = "100%", height = "0.875rem", fill = "#00bfc4", background = NULL) {
    bar <- div(style = list(background = fill, width = width, height = height))
    chart <- div(style = list(flexGrow = 1, marginLeft = "0.375rem", background = background), bar)
    div(style = list(display = "flex", alignItems = "center"), label, chart)
}

# same table with JS additions
t_react2 <- reactable(
    data = ERDF_imp_L_tab_int,
    # formatting
    pagination = FALSE,
    compact = TRUE,
    defaultSorted = "amount_planned",
    defaultSortOrder = "desc",
    showSortIcon = TRUE,
    style = list(fontFamily = "Work Sans, sans-serif", fontSize = "0.875rem"),
    #searchable = TRUE,
    # data
    groupBy = c("theme"#, "region"
    ),
    columns = list(
        theme = colDef(name = "Thematic objective",
                       style = list(fontWeight = "bold",background = "rgba(0, 0, 0, 0.03)")),

        #region = colDef(aggregate = "unique"),
        unique_programs = colDef(name = "N. & Id of Programs",
                                 aggregate = "count",
                                 format = colFormat(#locales = "it-IT",
                                     separators = TRUE, digits = 0 )),
        title = colDef(name = "Title of Programs",
                       style = list(background = "rgba(0, 0, 0, 0.03)")),

        amount_planned = colDef(name = "Amount Planned",
                                aggregate = "sum",
                                format = colFormat(#locales = "it-IT",
                                    separators = TRUE, digits = 0, prefix = "€", suffix = " mln")),
        amount_allocated = colDef(name = "Amount Allocated",
                                  aggregate = "sum",
                                  format = colFormat(#locales = "it-IT",
                                      separators = TRUE, digits = 0, prefix = "€", suffix = " mln")),
        amount_spent = colDef(name = "Amount Spent", aggregate = "sum",
                              format = colFormat(#locales = "it-IT",
                                  separators = TRUE,
                                  digits = 0, prefix = "€", suffix = " mln")),
        PercSpent = colDef(name = "Spent/Allocated at 31/12/2021",
                           aggregate = "mean",
                           format = colFormat(percent = TRUE, digits = 1),
                           defaultSortOrder = "desc",
                           # Render the bar charts using a custom cell render function
                           cell = function(value) {
                               # Format as percentages with 1 decimal place
                               value <- paste0(format(value * 100, nsmall = 0), "%")
                               bar_chart(value, width = value, fill = "#fc5185", background = "#e1e1e1")
                           },
                           # And left-align the columns
                           align = "left",


                           )
        )
    )


t_react2
## save Table 1 -------------------------------------------------------------
saveRDS(t_react2 ,file = "fig/t_react2.Rds" )








# >>>>>>>>> NEXT  ?????  --------------------------------




# --- --- --- --- --- ------ --- ------ --- ------ --- ------ --- ------ --- --- #
# 3) "ESIF_2014_20_plan2imp" [3kkx-ekfq] planned vs implemented ---------------------------------------
# INFO HERE https://dev.socrata.com/foundry/cohesiondata.ec.europa.eu/3kkx-ekfq
#
# ERDF-ESF-CF categorization by fund and comparison of PLANNED AMOUNTS TO IMPLEMENTED INVESTMENTS.
# STRUCTURE OF DATASET: The planned data is provided in tables one for each priority axis (investment pillar) of each programme. The categorisation systems is defined in Implementing Regulation 215/2014 with 8 dimensions in total
#In analysing the content of the dataset THE DIFFERENT DIMENSIONS SHOULD BE ANALYSED SEPARATELY AS OTHERWISE DOUBLE COUNTING OF FINANCIAL AMOUNTS WILL OCCUR.


### De-Select un-needed stuff -----------------------------------------------------

### Check Unique ID `cci` ---------------------------------------------------------
# assert + not_na
ERDF_plan2imp %>%  assert(not_na, cci  ) %>% summarise(N = n_distinct(cci)) #   OK
# assert + is_uniq
ERDF_plan2imp %>%  assert(is_uniq, cci  ) %>% summarise(N = n_distinct(cci)) # not unique... of course bc it is cumulative data BY DIMENSION!

# assert + is_uniq | by dimension
table(ERDF_plan2imp$cci) #  not unique!
ERDF_plan2imp %>%  assert(is_uniq, dimension_type) %>%
    summarise(N = n_distinct(dimension_type))#  not unique!

table(ERDF_plan2imp$cci, ERDF_plan2imp$dimension_type) # For any given dimension type, I have multiple instances of 1 cci bc THESE DATA ARE CUMULATIVE BY DIMENSION


### Understand variables ----------------------------------------------------
# https://cohesiondata.ec.europa.eu/2014-2020-Finances/ESIF-2014-2020-Finance-Implementation-Details/99js-gm52

# ----- DIMENSION
ERDF_plan2imp %>% skim (starts_with("dimension")) %>%
    tibble::as_tibble() %>% dplyr::select(skim_variable, n_missing  , n_unique =character.n_unique)
# --> dimension_title_short IS THE MOST GRANULAR

# -----Version of the financial data reflected in the dataset
# all financial data are referred to it
table(ERDF_plan2imp$financial_data_version)

# -----Priority axis of the programme
table(ERDF_plan2imp$priority)
# --> 3 numerazioni diverse !?!?!?!?!? YEs "There is no harmonised numbering or naming of Priority Axes.  Programmes were free to structure the Priority Axes as needed."

# --> $ amount are divided in priority
look <- ERDF_plan2imp[ERDF_plan2imp$financial_data_version == as.character('202101.0' ) ,
                      c("cci", "ms", "title","dimension_type","dimension_title_short", "category_of_region",
                        "priority", "reference_date",
                        "total_eligible_costs_selected_fin_data",
                        "total_elig_expenditure_declared_fin_data"

                        )]

paint(ERDF_plan2imp)

# ----- region category
table(ERDF_plan2imp$category_of_region)
# give easy labels...
ERDF_plan2imp$region <- factor(ERDF_plan2imp$category_of_region,
                                              levels = c("Less developed", "More developed","Transition",
                                                         "Outermost or Northern Sparsely Populated",
                                                         "VOID",
                                                         "REACT-EU" ),
                                              labels = c("LessDev", "MoreDev","Transition",
                                                         "Remote",
                                                         "InterRegio", # VOID
                                                         "REACT"  ))

table(ERDF_plan2imp$region)

# ----- Year for which cumulated data is provided
table(ERDF_plan2imp$year)



# >>>>>>>>> NEXT  - I'm HERE  --------------------------------------------------
# VOGLIO FARE UQESTO
#https://cohesiondata.ec.europa.eu/2014-2020/ESIF-2014-2020-Fin-Implementation-total-costs-by-T/vyjd-jfhd
### Important $ VARIABLES -------------------------------------------------



# --- --- --- --- --- ------ --- ------ --- ------ --- ------ --- ------ --- --- #
# 4) ESIF_2014_20_categ lookup categorization -----------------------------------


# # Loading files --------------------------------------------------------------
# from_dir <- here::here("rawdata/")
#
# from_dir %>%
#   list.files() %>%
#   .[str_detect(., "Data_FY")] -> files_xls # selecting my ones
# files_xls
#
# # Load everything into the Global Environment
# files_xls %>%
#   purrr::map(function(file_name) { # iterate through each file name
#     assign(
#       x = str_remove(file_name, ".xlsx"), # Remove file extension ".csv"
#       value = read_xlsx(paste0(from_dir, file_name)),
#       envir = .GlobalEnv
#     )
#   })


# Checking variables across FY ----------------------------------------------------------------




# save clean ds compressed --------------------------------------------------------------------
# saveRDS(dat2,
#         file = "dat2.rds",
#         ascii = FALSE, version = NULL, compress = TRUE
# )

# readRDS(file, refhook = NULL)
# readRDS (file = "FY_15_19_s.rds")



# Final save data -----------------------------------------------------------------------------
# dat3 <- dat2
# saveRDS(dat3, file = "dat3.Rds")


