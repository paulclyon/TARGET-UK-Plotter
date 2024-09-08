---
title: "Stats-2023"
output:
  pdf_document: default
  html_document: default
date: "2024-01-29"
---




These are some variables you might wish to choose, I had to do an ugly global variable hack to get them visible from this file, but you dont need to worry about that! Thanks Paul

Audit Period from:
2023-05-28
2024-05-27

For organs:
Kidney, Liver, Lung, Multiple Organs, Other/Unspecified

Generate on date:
<!--html_preserve--><div id="out0845b1d27bb864a0" class="shiny-text-output"></div><!--/html_preserve-->

Lovely stuff


<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> summary_level </th>
   <th style="text-align:left;"> Organs </th>
   <th style="text-align:right;"> mean </th>
   <th style="text-align:right;"> median </th>
   <th style="text-align:right;"> w10 </th>
   <th style="text-align:right;"> w21 </th>
   <th style="text-align:right;"> w10% </th>
   <th style="text-align:right;"> w21% </th>
   <th style="text-align:right;"> n </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> grouped </td>
   <td style="text-align:left;"> Kidney </td>
   <td style="text-align:right;"> 27.8 </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> 19.2 </td>
   <td style="text-align:right;"> 53.8 </td>
   <td style="text-align:right;"> 26 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> grouped </td>
   <td style="text-align:left;"> Liver </td>
   <td style="text-align:right;"> 21.0 </td>
   <td style="text-align:right;"> 18 </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 31.4 </td>
   <td style="text-align:right;"> 57.1 </td>
   <td style="text-align:right;"> 35 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> grouped </td>
   <td style="text-align:left;"> Lung </td>
   <td style="text-align:right;"> 18.4 </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 24 </td>
   <td style="text-align:right;"> 17.5 </td>
   <td style="text-align:right;"> 60.0 </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> grouped </td>
   <td style="text-align:left;"> Multiple Organs </td>
   <td style="text-align:right;"> 21.0 </td>
   <td style="text-align:right;"> 21 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 100.0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ungrouped </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 21.7 </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 23 </td>
   <td style="text-align:right;"> 59 </td>
   <td style="text-align:right;"> 22.5 </td>
   <td style="text-align:right;"> 57.8 </td>
   <td style="text-align:right;"> 102 </td>
  </tr>
</tbody>
</table>


```
## Adding missing grouping variables: `summary_level`
```

<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> summary_level </th>
   <th style="text-align:left;"> Organs </th>
   <th style="text-align:right;"> mean </th>
   <th style="text-align:right;"> median </th>
   <th style="text-align:right;"> w31% </th>
   <th style="text-align:right;"> w45% </th>
   <th style="text-align:right;"> w60% </th>
   <th style="text-align:right;"> n </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> grouped </td>
   <td style="text-align:left;"> Kidney </td>
   <td style="text-align:right;"> 62.8 </td>
   <td style="text-align:right;"> 62 </td>
   <td style="text-align:right;"> 3.8 </td>
   <td style="text-align:right;"> 23.1 </td>
   <td style="text-align:right;"> 46.2 </td>
   <td style="text-align:right;"> 26 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> grouped </td>
   <td style="text-align:left;"> Liver </td>
   <td style="text-align:right;"> 50.3 </td>
   <td style="text-align:right;"> 51 </td>
   <td style="text-align:right;"> 14.3 </td>
   <td style="text-align:right;"> 40.0 </td>
   <td style="text-align:right;"> 74.3 </td>
   <td style="text-align:right;"> 35 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> grouped </td>
   <td style="text-align:left;"> Lung </td>
   <td style="text-align:right;"> 59.9 </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:right;"> 10.0 </td>
   <td style="text-align:right;"> 30.0 </td>
   <td style="text-align:right;"> 45.0 </td>
   <td style="text-align:right;"> 40 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> grouped </td>
   <td style="text-align:left;"> Multiple Organs </td>
   <td style="text-align:right;"> 41.0 </td>
   <td style="text-align:right;"> 41 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 100.0 </td>
   <td style="text-align:right;"> 100.0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ungrouped </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 57.1 </td>
   <td style="text-align:right;"> 57 </td>
   <td style="text-align:right;"> 9.8 </td>
   <td style="text-align:right;"> 32.4 </td>
   <td style="text-align:right;"> 55.9 </td>
   <td style="text-align:right;"> 102 </td>
  </tr>
</tbody>
</table>

<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Tumour Type </th>
   <th style="text-align:right;"> mean </th>
   <th style="text-align:right;"> median </th>
   <th style="text-align:right;"> w90% </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Kidney (N=26) </td>
   <td style="text-align:right;"> 90.0 </td>
   <td style="text-align:right;"> 94.5 </td>
   <td style="text-align:right;"> 50.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Liver (N=35) </td>
   <td style="text-align:right;"> 71.3 </td>
   <td style="text-align:right;"> 72.0 </td>
   <td style="text-align:right;"> 82.9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Lung (N=40) </td>
   <td style="text-align:right;"> 77.5 </td>
   <td style="text-align:right;"> 76.0 </td>
   <td style="text-align:right;"> 70.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> All Organs (N=102) </td>
   <td style="text-align:right;"> 78.4 </td>
   <td style="text-align:right;"> 76.0 </td>
   <td style="text-align:right;"> 69.6 </td>
  </tr>
</tbody>
</table>


<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> clockstopped </th>
   <th style="text-align:right;"> n </th>
   <th style="text-align:right;"> mean </th>
   <th style="text-align:right;"> median </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Not Stopped </td>
   <td style="text-align:right;"> 85 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Stopped </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> -55.4 </td>
   <td style="text-align:right;"> -52 </td>
  </tr>
</tbody>
</table>


<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> clockstopped </th>
   <th style="text-align:right;"> n </th>
   <th style="text-align:right;"> mean </th>
   <th style="text-align:right;"> median </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Not Stopped </td>
   <td style="text-align:right;"> 85 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Stopped Post </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> -55.2 </td>
   <td style="text-align:right;"> -36.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Stopped Pre </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> -55.6 </td>
   <td style="text-align:right;"> -62.0 </td>
  </tr>
</tbody>
</table>


<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> ID </th>
   <th style="text-align:left;"> ClockStopWhy </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 10124709-1 </td>
   <td style="text-align:left;"> Clock Stops: 1:Consultant strike delayed his Rx planned 4th Oct (0 days pre-DTT, -28 days post-DTT) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10580196-1 </td>
   <td style="text-align:left;"> Clock Stops: 1:Needed imaging (-20 days pre-DTT, 0 days post-DTT) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1076896-1 </td>
   <td style="text-align:left;"> Clock Stops: 1:Patient Choice (-21 days pre-DTT, 0 days post-DTT) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10908501-1 </td>
   <td style="text-align:left;"> Clock Stops: 1:Patient DNA (0 days pre-DTT, -62 days post-DTT) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10971236-1 </td>
   <td style="text-align:left;"> Clock Stops: 1:Delayed by lung surgery and re-imaging (-136 days pre-DTT, 0 days post-DTT) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10976753-1 </td>
   <td style="text-align:left;"> Clock Stops: 1:Had to wait for VATs and Histology (-52 days pre-DTT, 0 days post-DTT) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1117534-1 </td>
   <td style="text-align:left;"> Clock Stops: 1:Patient choice (0 days pre-DTT, -70 days post-DTT) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 128716-1 </td>
   <td style="text-align:left;"> Clock Stops: 1:Offered a date in Nov but deferred it 2:Delayed due to needing to cease smoking (0 days pre-DTT, -90 days post-DTT) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1319170-2 </td>
   <td style="text-align:left;"> Clock Stops: 1:Need reimaging -  ?pseudo-aneurysm vs tumour. (0 days pre-DTT, -87 days post-DTT) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 201561-1 </td>
   <td style="text-align:left;"> Clock Stops: 1:Needed iron infusion (0 days pre-DTT, -39 days post-DTT) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 268708-2 </td>
   <td style="text-align:left;"> Clock Stops: 1:DNA OPA (-14 days pre-DTT, 0 days post-DTT) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 323761-1 </td>
   <td style="text-align:left;"> Clock Stops: 1:Episodes of VT on ICD 2:Uncontrolled Hyperglycaemia (0 days pre-DTT, -50 days post-DTT) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4489127-1 </td>
   <td style="text-align:left;"> Clock Stops: 1:Patient unwell (0 days pre-DTT, -62 days post-DTT) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 57469-1 </td>
   <td style="text-align:left;"> Clock Stops: 1:got right pneumonia (-116 days pre-DTT, 0 days post-DTT) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 686908-1 </td>
   <td style="text-align:left;"> Clock Stops: 1: (-62 days pre-DTT, 0 days post-DTT) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 764410-1 </td>
   <td style="text-align:left;"> Clock Stops: 1:Concommitent treatment for other tumour. (-21 days pre-DTT, 0 days post-DTT) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 815570-1 </td>
   <td style="text-align:left;"> Clock Stops: 1:Delay due to decision by patient re: SABR or Ablation. (0 days pre-DTT, -12 days post-DTT) </td>
  </tr>
</tbody>
</table>


## Including Plots



```
## Warning: Removed 2 rows containing missing values (`geom_point()`).
## Removed 2 rows containing missing values (`geom_point()`).
```

```
## Warning: Removed 94 rows containing missing values (`geom_point()`).
```

```
## Warning: Removed 93 rows containing missing values (`geom_point()`).
```

![plot of chunk ttt_plot](figure/ttt_plot-1.png)

![plot of chunk organ_pie](figure/organ_pie-1.png)

## Long waits


<table class="table" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> ID </th>
   <th style="text-align:right;"> Ref_RxDone </th>
   <th style="text-align:right;"> ClockStopDaysPreDTT </th>
   <th style="text-align:right;"> ClockStopDaysPostDTT </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 10669029-1 </td>
   <td style="text-align:right;"> 143 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10854015-1 </td>
   <td style="text-align:right;"> 128 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4006699-1 </td>
   <td style="text-align:right;"> 133 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 57469-1 </td>
   <td style="text-align:right;"> 121 </td>
   <td style="text-align:right;"> -116 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 686908-1 </td>
   <td style="text-align:right;"> 135 </td>
   <td style="text-align:right;"> -62 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 904992-1 </td>
   <td style="text-align:right;"> 168 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
</tbody>
</table>

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
