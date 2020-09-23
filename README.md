# Project Title

"Xche in Italia non riusciamo a realizzare quello che dobbiamo pianificato & gia finanziato?"

## Author

**Luisa M. Mimmi**  

<!-- ## License
This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details
 -->
 
## Project Structure
The analysis can be reproduced running in sequence the R scripts `01_*.R`:`04_*.R` which will generate the charts `gg_*.png` and clean data files `dat*.Rds`. 
Then `05_Analysis.Rmd` should compile the article as html and PDF. 

	├── 01_ingest-clean.R
	├── 02_explore-data.R

	├── 04_flowchart.R
	├── 05_Analysis.Rmd
	├── 05_Analysis.html
	├── 05_Analysis.md
	├── 05_Analysis.pdf
	├── .png
	├──  .Rproj
	├── R/
	├── dat2.rds
	├── dat3.Rds
	├── gg_USCIStime.png

	├── makefile
	├── rawdata/
 
## Acknowledgments
Below are the sources of datasets, R code chunks, and other interesting articles that served as inputs or inspiration for this analysis.

##### Data
+ [EU cohesion structural funds](https://cohesiondata.ec.europa.eu/stories/s/Information-maps-tracking-progress-in-investment-a/wjiv-jyr9m)
+ [Opencoesione](https://opencoesione.gov.it/it/nature/infrastrutture/)



##### Reference
1. Sabino Cassese "troppa politica (e schieramenti politici) e troppo poca amministrazione (see _Il buon governo. L'età dei doveri_ )

2. 


##### Inspiring open source R projects

+ [Fabio Votta: article on refugees and great data viz](https://favstats.eu/post/exploring_us_refugee_data/)
+ [Paul Williamson: Custom `crosstab` function](http://rstudio-pubs-static.s3.amazonaws.com/6975_c4943349b6174f448104a5513fed59a9.html)
