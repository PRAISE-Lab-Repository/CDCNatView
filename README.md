# CDCNatView

`CDCNatView` is an R Shiny web application to faciliate the exploration of the [CDC Wonder Natality Data] (https://wonder.cdc.gov/natality.html).

Preprint about RadaR available: https://doi.org/10.1101/347534
<br>
<br>
You can use the application by visiting the link [here](https://mmo7d7-adam-lin.shinyapps.io/CDCNatView/).

## Dependencies
`CDCNatView` was built in [R](https://www.r-project.org) using the [Shiny package](https://shiny.rstudio.com), an open source R package for developing web applications. Visualizations are generated through the Highcharter package, an R interface to the Highcharts JavaScript graphics library.

## Data
`CDCNatView` is able to pull aggregation tables from CDC Wonder using a custom API that generates `.rsd` files. We present four snapshots of data across year brackets 2016-2021, 2007-2021, 2003-2006, and 1995-2002. The tables below summarize the features present across each of the brackets:


#### Risk Factors
| Feature             	| Year Available                 |
|----------------------	|-------------------------------------------------------------------------------|
| Anemia | 2003-2006, 1995-2002 |
| Cardiac Disease | 2003-2006, 1995-2002 |
| Hydramnios / Oligohydramnios | 2003-2006, 1995-2002 |
| Diabetes / Pre-pregnancy Diabetes | All |
| Gestational Diabetes | 2016-2021 (Expanded)  |
| Pre-pregnancy Hypertension / Chronic Hypertension | All  |
| Gestational (Pregnancy-Associated) Hypertension | All |
| Eclampsia | All |
| Previous Preterm Birth | 2016-2021 (Expanded) |
| Infertility Treatment Used | 2016-2021 (Expanded) |
| Fertility Enhancing Drugs | 2016-2021 (Expanded)|
| Assistive Reproductive Technology | 2016-2021 (Expanded) |
| Previous Cesarean Delivery | 2016-2021 (Expanded) |
| Incompetent Cervix | 2003-2006, 1995-2002 |
| Lung Disease | 2003-2006, 1995-2002 |


#### Infections
| Feature             	| 2016-2021 (Expanded) | 2007-2021 | 2003-2006 | 1995-2002                      |
|----------------------	|----------------------------------------------------------------------------------	|
| Chlamydia | :heavy_check_mark:  | | | |
| Gonorrhea | :heavy_check_mark: | | | |
| Hepatitis B | :heavy_check_mark:  | | | |
| Hepatitis C | :heavy_check_mark:  | | | |
| Syphilis | :heavy_check_mark:  | | | |


#### Maternal Morbidities
| Feature             	| 2016-2021 (Expanded) | 2007-2021 | 2003-2006 | 1995-2002                      |
|----------------------	|----------------------------------------------------------------------------------	|
| Admission to Intensive Care Unit | :heavy_check_mark:  | | | |
| Maternal Transfusion | :heavy_check_mark:  | | | |
| Perineal Laceration | :heavy_check_mark:  | | | |
| Ruptured Uterus | :heavy_check_mark:  | | | |
| Unplanned Hysterectomy | :heavy_check_mark:  | | | |




#### Demographic
| Feature             	| 2016-2021 (Expanded) | 2007-2021 | 2003-2006 | 1995-2002                      |
|----------------------	|----------------------------------------------------------------------------------	|
| Race | :heavy_check_mark:  | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: |
| Age | :heavy_check_mark:  | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: |
| Education | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: |

#### Behavioral
| Feature             	| 2016-2021 (Expanded) | 2007-2021 | 2003-2006 | 1995-2002                      |
|----------------------	|----------------------------------------------------------------------------------	|
| Start of Prenatal Care | :heavy_check_mark:  | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: |
| Tobacco Use | :heavy_check_mark:  | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: |

### Child
| Feature             	| 2016-2021 (Expanded) | 2007-2021 | 2003-2006 | 1995-2002                      |
|----------------------	|----------------------------------------------------------------------------------	|
| Delivery Method | :heavy_check_mark:  | :heavy_check_mark: | :heavy_check_mark: |  |
| Gestational Age | :heavy_check_mark:  | :heavy_check_mark: | :heavy_check_mark: | :heavy_check_mark: |
| Birth Weight | :heavy_check_mark:  | :heavy_check_mark: | :heavy_check_mark: | |
