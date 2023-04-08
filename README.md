# CDCNatView

`CDCNatView` is an R Shiny web application to faciliate the exploration of the [CDC Wonder Natality Data] (https://wonder.cdc.gov/natality.html)

<br>
<br>
You can use the application by visiting the link [here](https://mmo7d7-adam-lin.shinyapps.io/CDCNatView/).

## Dependencies
`CDCNatView` was built in [R](https://www.r-project.org) using the [Shiny package](https://shiny.rstudio.com), an open source R package for developing web applications. Visualizations are generated through the Highcharter package, an R interface to the Highcharts JavaScript graphics library.

## Data
`CDCNatView` is able to pull aggregation tables from CDC Wonder using a custom API that generates `.rsd` files. We present four snapshots of data across year brackets 2016-2021, 2007-2021, 2003-2006, and 1995-2002. The tables below summarize the features present across each of the brackets:


#### Risk Factors
| Feature             	| Year Brackets Available                 |
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
| Feature             	| Year Brackets Available                 |
|----------------------	|----------------------------------------------------------------------------------	|
| Chlamydia |  2016-2021 (Expanded) |
| Gonorrhea |  2016-2021 (Expanded) |
| Hepatitis B |  2016-2021 (Expanded) |
| Hepatitis C |  2016-2021 (Expanded) |
| Syphilis |  2016-2021 (Expanded) |


#### Maternal Morbidities
| Feature             	| Year Brackets Available                 |
|----------------------	|----------------------------------------------------------------------------------	|
| Admission to Intensive Care Unit | 2016-2021 (Expanded)|
| Maternal Transfusion | 2016-2021 (Expanded) |
| Perineal Laceration | 2016-2021 (Expanded) |
| Ruptured Uterus | 2016-2021 (Expanded) | 
| Unplanned Hysterectomy | 2016-2021 (Expanded) |




#### Demographic
| Feature             	| Year Brackets Available                   |
|----------------------	|----------------------------------------------------------------------------------	|
| Race | All |
| Age | All |
| Education | All |

#### Behavioral
| Feature             	| Year Brackets Available                      |
|----------------------	|----------------------------------------------------------------------------------	|
| Start of Prenatal Care | All |
| Tobacco Use | All |

### Child
| Feature             	| Year Brackets Available                      |
|----------------------	|----------------------------------------------------------------------------------	|
| Delivery Method | 2016-2021 (Expanded), 2007-2021, 2003-2006 |
| Gestational Age | All |
| Birth Weight | 2016-2021 (Expanded), 2007-2021, 2003-2006 |
