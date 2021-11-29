---
title: '\bf Replication Paper'
author: 'Catherine Darin and Zagreb Mukerjee'
date: '\today'
fontsize: 12pt
output:
  pdf_document: default
urlcolor: red
header-includes:
  - \usepackage{bm}
---

\renewcommand{\arraystretch}{1.1}
\renewcommand{\topfraction}{.85}
\renewcommand{\bottomfraction}{.7}
\renewcommand{\textfraction}{.15}
\renewcommand{\floatpagefraction}{.66}
\setcounter{topnumber}{3}
\setcounter{bottomnumber}{3}
\setcounter{totalnumber}{4}


## Outline
- Framing story: the trajectory of XYZ county that swung from O to T and suffered big manufacturing layoffs
- Substantive importance: Why did Trump win in 2016? A longstanding debate about relative importance of race and economic factors. Complicated by the interaction of the two. We can tease out 
- Empirical strategy: Differential manuf. exposure across counties allows for identification of the causal effect of deindustrialization on change in Dem vote share. Further differences in racial exposure to mfg layoffs allows for identification of the interaction between race and deindustrialization. 
- Data: census Quarterly Workforce Indicators, which break down employment by industry, race and ethnicity. Compute net change in mfg (long term job loss)



## Findings

### Descriptive Statistics
\begin{table}[!h]

\caption{\label{tab:unnamed-chunk-1}Manufacturing Job Changes 2012-2015}
\centering
\resizebox{\linewidth}{!}{
\begin{tabular}[t]{lrrrrr}
\toprule
  & Mean & Std Dev & 25th Pctile & Median & 75th Pctile\\
\midrule
Change in Dem Vote Share & -0.06 & 0.05 & -0.10 & -0.05 & -0.03\\
Mfg Share of Emp & 0.20 & 0.16 & 0.08 & 0.16 & 0.28\\
Mfg Share of Emp (White) & 0.15 & 0.13 & 0.06 & 0.12 & 0.22\\
Mfg Share of Emp (Nonwhite) & 0.05 & 0.07 & 0.01 & 0.03 & 0.06\\
Change in Mfg Jobs/ Worker & -0.01 & 0.05 & -0.02 & 0.00 & 0.01\\
\addlinespace
Change in Mfg Jobs/ Worker (W) & 0.00 & 0.03 & -0.01 & 0.00 & 0.01\\
Change in Mfg Jobs/ Worker (NW) & 0.00 & 0.02 & -0.01 & 0.00 & 0.00\\
\bottomrule
\end{tabular}}
\end{table}

\begin{table}[!h]

\caption{\label{tab:unnamed-chunk-1}Manufacturing Job Changes 2004-2015}
\centering
\resizebox{\linewidth}{!}{
\begin{tabular}[t]{lrrrrr}
\toprule
  & Mean & Std Dev & 25th Pctile & Median & 75th Pctile\\
\midrule
Mfg Share of Emp & 0.20 & 0.15 & 0.09 & 0.17 & 0.29\\
Mfg Share of Emp (White) & 0.16 & 0.13 & 0.06 & 0.13 & 0.22\\
Mfg Share of Emp (Nonwhite) & 0.05 & 0.07 & 0.01 & 0.02 & 0.06\\
Change in Mfg Jobs/ Worker & 0.04 & 0.09 & -0.01 & 0.02 & 0.06\\
Change in Mfg Jobs/ Worker (W) & 0.03 & 0.07 & 0.00 & 0.02 & 0.06\\
\addlinespace
Change in Mfg Jobs/ Worker (NW) & 0.00 & 0.04 & -0.01 & 0.00 & 0.01\\
\bottomrule
\end{tabular}}
\end{table}

## Figures




### Example


### Regression summaries


% Table created by stargazer v.5.2.2 by Marek Hlavac, Harvard University. E-mail: hlavac at fas.harvard.edu
% Date and time: Mon, Nov 29, 2021 - 12:18:08 AM
\begin{table}[!htbp] \centering 
  \caption{Effect of Manufacturing Layoffs on Democratic Vote Share} 
  \label{} 
\begin{tabular}{@{\extracolsep{5pt}}lcccc} 
\\[-1.8ex]\hline 
\hline \\[-1.8ex] 
 & \multicolumn{4}{c}{\textit{Dependent variable:}} \\ 
\cline{2-5} 
\\[-1.8ex] & \multicolumn{4}{c}{Change in Share (2012-2016)} \\ 
\\[-1.8ex] & (1) & (2) & (3) & (4)\\ 
\hline \\[-1.8ex] 
 Manufacturing Layoffs & 0.36$^{***}$ & 0.21$^{**}$ &  &  \\ 
  & (0.12) & (0.10) &  &  \\ 
  & & & & \\ 
 White Manufacturing Layoffs &  &  & 6.39$^{***}$ & 1.57$^{***}$ \\ 
  &  &  & (1.21) & (0.57) \\ 
  & & & & \\ 
 Nonwhite Manufacturing Layoffs &  &  & $-$3.92$^{***}$ & $-$0.95$^{***}$ \\ 
  &  &  & (0.74) & (0.35) \\ 
  & & & & \\ 
\hline \\[-1.8ex] 
Controls For White Share/Service Layoffs & No & Yes & No & Yes \\ 
Observations & 2,930 & 2,930 & 2,707 & 2,707 \\ 
Adjusted R$^{2}$ & 0.72 & 0.74 & 0.73 & 0.75 \\ 
\hline 
\hline \\[-1.8ex] 
\textit{Note:}  & \multicolumn{4}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\ 
\end{tabular} 
\end{table} 




% Table created by stargazer v.5.2.2 by Marek Hlavac, Harvard University. E-mail: hlavac at fas.harvard.edu
% Date and time: Mon, Nov 29, 2021 - 12:18:08 AM
\begin{table}[!htbp] \centering 
  \caption{Effect of Manufacturing Layoffs on Democratic Vote Share (since 2004)} 
  \label{} 
\begin{tabular}{@{\extracolsep{5pt}}lcccc} 
\\[-1.8ex]\hline 
\hline \\[-1.8ex] 
 & \multicolumn{4}{c}{\textit{Dependent variable:}} \\ 
\cline{2-5} 
\\[-1.8ex] & \multicolumn{4}{c}{Change in Share (2016-2012)} \\ 
\\[-1.8ex] & (1) & (2) & (3) & (4)\\ 
\hline \\[-1.8ex] 
 Manufacturing Layoffs & $-$0.31$^{***}$ & $-$0.31$^{*}$ &  &  \\ 
  & (0.10) & (0.17) &  &  \\ 
  & & & & \\ 
 White Manufacturing Layoffs &  &  & $-$0.24$^{***}$ & $-$0.20$^{***}$ \\ 
  &  &  & (0.05) & (0.07) \\ 
  & & & & \\ 
 Nonwhite Manufacturing Layoffs &  &  & 0.17$^{***}$ & 0.13$^{***}$ \\ 
  &  &  & (0.03) & (0.04) \\ 
  & & & & \\ 
\hline \\[-1.8ex] 
Controls For White Share/Service Layoffs & No & Yes & No & Yes \\ 
Observations & 2,966 & 2,966 & 2,720 & 2,720 \\ 
Adjusted R$^{2}$ & 0.71 & 0.73 & 0.73 & 0.75 \\ 
\hline 
\hline \\[-1.8ex] 
\textit{Note:}  & \multicolumn{4}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\ 
\end{tabular} 
\end{table} 

### Counterfactual assessment of election


  |                                                                              |                                                                      |   0%  |                                                                              |======                                                                |   9%  |                                                                              |=============                                                         |  18%  |                                                                              |===================                                                   |  27%  |                                                                              |========================                                              |  34%  |                                                                              |==============================                                        |  42%  |                                                                              |====================================                                  |  51%  |                                                                              |=======================================                               |  55%  |                                                                              |=============================================                         |  64%  |                                                                              |================================================                      |  68%  |                                                                              |============================================================          |  86%  |                                                                              |===============================================================       |  90%  |                                                                              |======================================================================| 100%

\includegraphics{Draft_files/figure-latex/unnamed-chunk-4-1} 




### example state: Michigan

### 2020

## Potential Future Work

- Disaggregation: Instead of instrumenting by manufacturing share, perhaps we can look at layoffs by industry in each county. While aggregate manufacturing employment may be endogeneous, there's less reason to believe that specific industries would be (cf. Autor, Dorn and Hanson). This may also allow us to get further away from ecological inference problems.
- Trend-cycle estimation: There are techniques from econometrics and other places that could potentially be used to decompose gross layoffs into seasonal and non-seasonal components. 

## Appendix
