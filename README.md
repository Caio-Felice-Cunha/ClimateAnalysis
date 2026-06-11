# ClimateAnalysis

Analysis of US land temperatures from the Berkeley Earth dataset, built in R with a companion Power BI dashboard.

[Full PDF report](https://github.com/Caio-Felice-Cunha/ClimateAnalysis/blob/main/V2_Climate-Report.pdf) <br>
[Power BI Dashboard (publish to web)](https://app.powerbi.com/view?r=eyJrIjoiZTg4MzA5OTYtZjBjZC00YjdiLTgzYTYtOTNkNmIyMjVhNWQwIiwidCI6IjA4OTM0YTNmLWFkNmUtNDgzZS1hNjhlLTUxYWI3OTI1YmFiNyJ9)

> Note: an earlier "Power BI Power Apps" link was removed because it required signing in to the owner's tenant and was not usable by public visitors. The publish-to-web dashboard above is public.

![Climate](https://user-images.githubusercontent.com/111542025/225629592-184b2cba-a3fd-45ed-bab7-e0c3b5a12c8e.png)

## What this is

A small portfolio analysis that asks a simple question: how have US land temperatures changed over time, and how does Texas compare to the national average? The data is the Berkeley Earth by-city monthly temperature series. The R scripts clean the data, compare a state-capitals subset against all US cities, and zoom into five Texas cities.

## Business framing

There is no single client company here. Temperature predictability matters for agriculture, construction planning, and livestock, so the analysis focuses on two things: the long-run rise in average temperature and the narrowing of the annual temperature range.

## Results

All numbers below are taken directly from the stored report outputs (`V2_Climate-Report.pdf` and `Version 1/V1_Climate-Report.pdf`). They are not estimates.

* US dataset after cleaning: 659,468 monthly city records spanning 1743-11-01 to 2013-09-01. Average temperature minimum -25.16 C, median 14.96 C, mean 13.97 C, maximum 34.38 C.
* State-capital subset: 98,410 records, mean temperature 13.02 C, same date span. Its distribution closely matches the full US set, which is why the capitals are used as a lighter-weight proxy.
* Capital-cities warming: mean annual temperature rose from 10.79774 C (years before 1760) to 14.14854 C (years after 1990), an increase of 3.35 C. Over the same window the annual range narrowed from [-5.84, 21.05] to [4.01, 23.57].
* Missing data: about 8% of rows (V2, MySQL-sourced) and about 7% of rows (V1, CSV-sourced) had at least one missing value and were dropped with `na.omit`.
* Texas vs national: US mean is 13.97 C, while the Texas city means run 18.09 C (Dallas) to 20.25 C (Houston), so Texas sits roughly 4 to 6 C above the national average.

### A note on the Texas cities

The Berkeley Earth by-city dataset assigns nearby cities the same grid series. In the stored outputs Fort Worth is byte-identical to Dallas, and San Antonio is byte-identical to Austin. So the five Texas cities are effectively three distinct series (Austin, Dallas, Houston), not five.

## How to reproduce

### Common requirements

* R 4.x and the packages used by the scripts: `data.table`, `dplyr`, `ggplot2`, `viridis`, `hrbrthemes`, `plotly`. Version 2 additionally needs `DBI` and `RMariaDB`.
* The Berkeley Earth by-city CSV. Download it from [Berkeley Earth](https://berkeleyearth.org/data/). The CSV is large and is not committed to this repo.

### Version 1 (CSV based)

1. Put `GlobalClimate.csv` in the same folder as `Version 1/V1_Climate_Analysis.R`.
2. Run the script. It reads the CSV with `fread`, filters to the United States, cleans missing values, and produces the charts.

### Version 2 (MySQL / MariaDB based)

1. Load the Berkeley Earth CSV into a MySQL or MariaDB database as a table named `globalclimate`.
2. Set connection details as environment variables (for example in `~/.Renviron`):
   ```
   MYSQL_USER=root
   MYSQL_PASSWORD=your_password
   MYSQL_DB=climate
   MYSQL_HOST=127.0.0.1
   MYSQL_PORT=3306
   ```
   The script reads these with `Sys.getenv`, so no credentials are hardcoded.
3. Run `V2_Climate_Analysis.R`. It queries the US rows, cleans the data, and produces the capitals and Texas analyses.

## Version history

* Version 1: data loaded from a local CSV with `fread`.
* Version 2: data loaded from MySQL/MariaDB; more cities and charts; the capitals-vs-Texas comparison.
* Version 3: a Power BI dashboard (linked above) for an interactive view.

## Data insight

Temperatures have risen steadily over the years and the trend points up. The annual range has also narrowed, so for businesses that depend on temperature predictability the recent period is more stable. Activities that depend on consistent cold will need cooler regions or active cooling.

## Conclusions

Based on the stored data, US temperatures have risen and the long-run direction is up. Each year's change is small, but the cumulative shift from the earliest records to the most recent is large enough to matter across sectors.

## Next steps

* Improve the US statistical and regional analysis.
* Add histograms and tooltips to the Power BI charts.
* Study the correlation between the variables.

## Disclaimer

A good part of this project was done as part of the Data Science Academy "Big Data Analytics with R and Microsoft Azure Machine Learning" course (part of the Data Scientist training).
