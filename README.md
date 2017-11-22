# fpmoutliers

[![Build Status](https://travis-ci.org/jaroslav-kuchar/fpmoutliers.svg?branch=master)](https://travis-ci.org/jaroslav-kuchar/fpmoutliers)

R implementation of algorithms for detection of outliers based on frequent pattern mining.

If you would like to cite our work, please use:

```bib
@InProceedings{kuchar:2017:FPI,
  title =    {Spotlighting Anomalies using Frequent Patterns},
  author =   {Jaroslav Kuchař and Vojtěch Svátek},
  booktitle =    {Proceedings of the KDD 2017 Workshop on Anomaly Detection in Finance},
  year =   {2017},
  volume =   {71},
  series =   {Proceedings of Machine Learning Research},
  address =    {Halifax, Nova Scotia, Canada},
  month =    {14 Aug},
  publisher =    {PMLR},
  issn = {1938-7228}
}
```

Available implementations:

- FPI, WFPI - Frequent Pattern Isolation, Weighted Frequent Pattern Isolation
  * J. Kuchar, V. Svatek: Spotlighting Anomalies using Frequent Patterns, Proceedings of the KDD 2017 Workshop on Anomaly
Detection in Finance, Halifax, Nova Scotia, Canada, PMLR, 2017.  [link](https://sites.google.com/view/kdd-adf-2017/accepted-papers)
- FPCOF - Frequent Pattern Contradiction Outlier Factor
  * X. Tang, G. Li and G. Chen, "Fast Detecting Outliers over Online Data Streams," 2009 International Conference on Information Engineering and Computer Science, Wuhan, 2009, pp. 1-4. [link](http://ieeexplore.ieee.org/abstract/document/5363123/)
- FPOF - Frequent Pattern Outlier Factor
  * He, Z., Xu, X., Huang, J. Z., Deng, S.: FP-Outlier: Frequent Pattern Based Outlier Detection. Computer Science and Information Systems, Vol. 2, No. 1, 103-118. (2005). [link](http://www.comsis.org/archive.php?show=pprnnn-2106)
- LFPOF - L. Frequent Pattern Outlier Factor
  * W. Zhang, J. Wu and J. Yu, "An Improved Method of Outlier Detection Based on Frequent Pattern," Information Engineering (ICIE), 2010 WASE International Conference on, Beidaihe, Hebei, 2010, pp. 3-6. [link](http://ieeexplore.ieee.org/document/5571194/)
- MFPOF - Maximal Frequent Pattern Outlier Factor
  * Feng Lin, Wang Le, Jin Bo - Research on Maximal Frequent Pattern Outlier Factor for Online HighDimensional Time-Series Outlier Detection. Journal of Convergence Information Technology 5(10):66-71 · December 2010. [link](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.592.2752)
- WCFPOF - Weighted Closed Frequent Pattern Outlier Factor
  * Jiadong Ren, Qunhui Wu, Changzhen Hu, and Kunsheng Wang. 2009. An Approach for Analyzing Infrequent Software Faults Based on Outlier Detection. In Proceedings of the 2009 International Conference on Artificial Intelligence and Computational Intelligence - Volume 04 (AICI '09), Vol. 4. IEEE Computer Society, Washington, DC, USA, 302-306. [link](http://dl.acm.org/citation.cfm?id=1723929)
- WFPOF - Weighted Frequent Pattern Outlier Factor
  * ZHOU Xiao-Yun+, SUN Zhi-Hui, ZHANG Bai-Li, YANG Yi-Dong - A Fast Outlier Detection Algorithm for High Dimensional Categorical Data Streams. Journal of Software 18(4) · April 2007. [link](http://en.cnki.com.cn/Article_en/CJFDTOTAL-RJXB200704015.htm)

## Installation

The package is available in CRAN repository:

- https://cran.r-project.org/package=fpmoutliers

```R
install.packages('fpmoutliers',dependencies=TRUE, repos="http://cran.us.r-project.org")
```


## Development Version Installation

Package installation from GitHub:
```R
library("devtools")
devtools::install_github("jaroslav-kuchar/fpmoutliers")
```

## Usage

### Basic example

```R
library(fpmoutliers)
dataFrame <- read.csv(system.file("extdata", "fp-outlier-customer-data.csv", package = "fpmoutliers"))
model <- FPI(dataFrame, minSupport = 0.001)
dataFrame <- dataFrame[order(model$scores, decreasing = TRUE),]
print(dataFrame[1,]) # instance with the highest anomaly score
print(dataFrame[nrow(dataFrame),]) # instance with the lowest anomaly score
```

### Experimental explanations

#### Graphical explanation using bar plots 

Currently not suitable for large datasets - the plot is limited by the number of rows and columns of the input data.

```R
library("fpmoutliers")
dataFrame <- read.csv(
     system.file("extdata", "fp-outlier-customer-data.csv", package = "fpmoutliers"))
model <- FPI(dataFrame, minSupport = 0.001)
# sort data by the anomaly score
dataFrame <- dataFrame[order(model$scores, decreasing = TRUE),]
visualizeInstance(dataFrame, 1) # instance with the highest anomaly score
visualizeInstance(dataFrame, nrow(dataFrame)) # instance with the lowest anomaly score
```

#### Textual explanation

```R
library("fpmoutliers")
dataFrame <- read.csv(
     system.file("extdata", "fp-outlier-customer-data.csv", package = "fpmoutliers"))
model <- FPI(dataFrame, minSupport = 0.001)
# sort data by the anomaly score
dataFrame <- dataFrame[order(model$scores, decreasing = TRUE),]
# instance with the highest anomaly score
out <- describeInstance(dataFrame, model, 1)
# instance with the lowest anomaly score
out <- describeInstance(dataFrame, model, nrow(dataFrame))
```

### Other available functionalities
#### Experimental automatic build
```R
library("fpmoutliers")
data("iris")
model <- fpmoutliers::build(iris)
```

#### Save the model to an experimental PMML format

- Kuchar, Jaroslav et al. “Outlier (Anomaly) Detection Modelling in PMML.” RuleML+RR (2017).
  * [link](http://ceur-ws.org/Vol-1875/paper9.pdf)

```R
library(fpmoutliers)
library(XML)
dataFrame <- read.csv(system.file("extdata", "fp-outlier-customer-data.csv", package = "fpmoutliers"))
model <- FPI(dataFrame, minSupport = 0.001)
saveXML(generatePMML(model, dataFrame), "example_out.xml")
```

## Model Output

All implemented methods return a list with following parameters:
- `minSupport` - minimum support setting for frequent itemsets mining
- `maxlen` - maximum length of frequent itemsets
- `model` - frequent itemset model represented as [itemsets-class](https://cran.r-project.org/package=arules)
- `scores` - outlier/anomaly scores for each observation/row of the input dataframe

## Contributors

- Jaroslav Kuchař (https://github.com/jaroslav-kuchar)

## Licence

Apache License Version 2.0
