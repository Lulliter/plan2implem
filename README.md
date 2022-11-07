> _(work in progress)_

# Project Title

"Xchè in Italia non riusciamo a realizzare quello che abbiamo pianificato & gia finanziato?"

## Background 
 
The European structural and investment funds are:

1) **European regional development fund (ERDF)** - _all countries_ – promotes balanced development in the different regions of the EU 
2) **European social fund (ESF)** - _all countries_ - supports employment-related projects throughout Europe and invests in Europe’s human capital – its workers, its young people and all those seeking a job.
3) **Cohesion fund (CF)** - _15 countries (not Italy)_ – funds transport and environment projects in countries where the gross national income (GNI) per inhabitant is less than 90% of the EU average. In 2014-20, these are Bulgaria, Croatia, Cyprus, the Czech Republic, Estonia, Greece, Hungary, Latvia, Lithuania, Malta, Poland, Portugal, Romania, Slovakia and Slovenia.
4) **European agricultural fund for rural development (EAFRD)** - _all countries_ – focuses on resolving the particular challenges facing EU's rural areas.
5) **European maritime and fisheries fund (EMFF)** – helps fishermen to adopt sustainable fishing practices and coastal communities to diversify their economies, improving quality of life along European coasts.
European maritime and fisheries fund (EMFF)

Important fields

+ `cci` Unique identifier of each operational programme (looked at for 2014/15/16/17/18/19/20)
+ `title` Short title of the operational programme (may differ from formal name)
+ `ver` Version of the operational programme reflected in the dataset
+ `fund` Name of the ESI funds (ERDF, CF,...)
+ `to` Thematic Objective
+ `to_short`Short name for the thematic objective. For example: "Research and Innovation" stands for "Strengthening research, technological development and innovation."
+ `to_long` Full name for the thematic objective
+ `year` Filtering the data using the column "year" provides a shapshot of the decided amounts, project selection data and eligible expenditure declared by projects at that date.


## Purpose 
In the dataset, **ESIF** includes the 5 funds (ERDF, CF, ESF, EAFRD and EMFF).

Here, I'll focus on the **European regional development fund (ERDF)** first ... 

> [here](https://cohesiondata.ec.europa.eu/browse?category=2014+%2F+2020+Finances&limitTo=datasets) some more ESIF datasets

## Project Structure
The analysis can be reproduced running in sequence the R scripts `01_*.R`:`04_*.R` which will generate the charts `gg_*.png` and clean data files `dat*.Rds`. 
Then `05_Analysis.Rmd` should compile the article as html and PDF. (`tree -C -L 1`)

```.
    ├── 01_ingest-clean.R
    ├── 02_explore-data.R
    ├── 03_Analysis.Rmd
    ├── Plan2Imp.bib
    ├── R/*
    ├── README.html
    ├── README.md
    ├── ___render-deploy.sh
    ├── data/*
    ├── plan2implem.Rproj
    └── reference/*
```
 
## Data Sources
<!--  + [EU cohesion structural funds](https://cohesiondata.ec.europa.eu/stories/s/Information-maps-tracking-progress-in-investment-a/wjiv-jyr9m) -->
+ [EU cohesion overview 2014-2020](https://cohesiondata.ec.europa.eu/cohesion_overview/14-20#)
+ [Opencoesione](https://opencoesione.gov.it/it/nature/infrastrutture/)

+ **DEF** has a section on "Monitooraggio dei Fondi Strtutturali" ----> go check!!
che viene monitorato da [IGRUE-Ispettorato Generale per i Rapporti finanziari con l'Unione Europea](http://www.rgs.mef.gov.it/VERSIONE-I/e_government/amministrazioni_pubbliche/igrue/index.html)

## Reference
> "Corruptissima re publica plurimae leges"" 
 Tacito (Annales, Libro III, 27), che significa: "moltissime sono le leggi quando lo Stato è corrotto". 

1. Sabino Cassese "troppa politica (e schieramenti politici) e troppo poca amministrazione (see _Il buon governo. L'età dei doveri_ )

2. Aspen Institute: Il futuro dell'italia - strettoie burocratiche 
"Il “modello Genova” per il ponte, ma anche le regole per Expo 2015 e quelle della legge Obiettivo sono indicazioni opportune (cfr. anche infra, cap. INFRASTRUTTURE, par.4.3)."

3. Luisa Torchia 

## Relevant R projects

+ [Roberto Palloni ](https://github.com/rpalloni/ESIFy/tree/cc0dd1c24953e113a909051e84747fc5a2aea87b)
+ [ESIF climate taggin Praga](http://rstudio-pubs-static.s3.amazonaws.com/6975_c4943349b6174f448104a5513fed59a9.html)
+ [ITALIANO COESIONE](https://github.com/andreoliant/octk)


## Inspiring open source R projects

+ [Fabio Votta: article on refugees and great data viz](https://favstats.eu/post/exploring_us_refugee_data/)
+ [Paul Williamson: Custom `crosstab` function](http://rstudio-pubs-static.s3.amazonaws.com/6975_c4943349b6174f448104a5513fed59a9.html)

## Author

**Luisa M. Mimmi**  

<!-- ## License
This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details
 -->
 
