---
title: "Stats-2023"
output:
  pdf_document: default
  html_document: default
date: "2024-01-29"
---




These are some variables you might wish to choose, I had to do an ugly global variable hack to get them visible from this file, but you dont need to worry about that! Thanks Paul

Audit Period from:
2023-09-13
2024-09-12

For organs:
Kidney, Liver, Lung, Multiple Organs

Generate on date:
<!--html_preserve--><div id="out42ba040e05eee651" class="shiny-text-output"></div><!--/html_preserve-->

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
   <td style="text-align:right;"> 23.6 </td>
   <td style="text-align:right;"> 14.0 </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 19 </td>
   <td style="text-align:right;"> 32.1 </td>
   <td style="text-align:right;"> 67.9 </td>
   <td style="text-align:right;"> 28 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> grouped </td>
   <td style="text-align:left;"> Liver </td>
   <td style="text-align:right;"> 16.4 </td>
   <td style="text-align:right;"> 20.5 </td>
   <td style="text-align:right;"> 12 </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> 28.6 </td>
   <td style="text-align:right;"> 52.4 </td>
   <td style="text-align:right;"> 42 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> grouped </td>
   <td style="text-align:left;"> Lung </td>
   <td style="text-align:right;"> 21.0 </td>
   <td style="text-align:right;"> 17.0 </td>
   <td style="text-align:right;"> 10 </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 20.0 </td>
   <td style="text-align:right;"> 62.0 </td>
   <td style="text-align:right;"> 50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> grouped </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 96.5 </td>
   <td style="text-align:right;"> 96.5 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ungrouped </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 21.3 </td>
   <td style="text-align:right;"> 18.0 </td>
   <td style="text-align:right;"> 31 </td>
   <td style="text-align:right;"> 72 </td>
   <td style="text-align:right;"> 25.4 </td>
   <td style="text-align:right;"> 59.0 </td>
   <td style="text-align:right;"> 122 </td>
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
   <td style="text-align:right;"> 66.4 </td>
   <td style="text-align:right;"> 56.0 </td>
   <td style="text-align:right;"> 14.3 </td>
   <td style="text-align:right;"> 35.7 </td>
   <td style="text-align:right;"> 60.7 </td>
   <td style="text-align:right;"> 28 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> grouped </td>
   <td style="text-align:left;"> Liver </td>
   <td style="text-align:right;"> 39.8 </td>
   <td style="text-align:right;"> 42.5 </td>
   <td style="text-align:right;"> 38.1 </td>
   <td style="text-align:right;"> 57.1 </td>
   <td style="text-align:right;"> 83.3 </td>
   <td style="text-align:right;"> 42 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> grouped </td>
   <td style="text-align:left;"> Lung </td>
   <td style="text-align:right;"> 52.4 </td>
   <td style="text-align:right;"> 57.0 </td>
   <td style="text-align:right;"> 18.0 </td>
   <td style="text-align:right;"> 42.0 </td>
   <td style="text-align:right;"> 56.0 </td>
   <td style="text-align:right;"> 50 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> grouped </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 100.0 </td>
   <td style="text-align:right;"> 100.0 </td>
   <td style="text-align:right;"> 100.0 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ungrouped </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 50.4 </td>
   <td style="text-align:right;"> 50.0 </td>
   <td style="text-align:right;"> 25.4 </td>
   <td style="text-align:right;"> 46.7 </td>
   <td style="text-align:right;"> 67.2 </td>
   <td style="text-align:right;"> 122 </td>
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
   <td style="text-align:left;"> Kidney (N=28) </td>
   <td style="text-align:right;"> 90.0 </td>
   <td style="text-align:right;"> 74.0 </td>
   <td style="text-align:right;"> 64.3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Liver (N=42) </td>
   <td style="text-align:right;"> 56.3 </td>
   <td style="text-align:right;"> 62.5 </td>
   <td style="text-align:right;"> 88.1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Lung (N=50) </td>
   <td style="text-align:right;"> 73.4 </td>
   <td style="text-align:right;"> 72.5 </td>
   <td style="text-align:right;"> 72.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> All Organs (N=2) </td>
   <td style="text-align:right;"> 96.5 </td>
   <td style="text-align:right;"> 96.5 </td>
   <td style="text-align:right;"> 50.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> All Organs (N=122) </td>
   <td style="text-align:right;"> 71.7 </td>
   <td style="text-align:right;"> 69.5 </td>
   <td style="text-align:right;"> 75.4 </td>
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
   <td style="text-align:right;"> 111 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Stopped </td>
   <td style="text-align:right;"> 11 </td>
   <td style="text-align:right;"> -41.6 </td>
   <td style="text-align:right;"> -39 </td>
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
   <td style="text-align:right;"> 111 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Stopped Post </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> -83.0 </td>
   <td style="text-align:right;"> -52.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Stopped Pre </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:right;"> -7.2 </td>
   <td style="text-align:right;"> -33.5 </td>
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
   <td style="text-align:left;"> 10792357-1 </td>
   <td style="text-align:left;"> Clock Stops: 1:Pt - Stroke.  Paused from waiting list &amp; referrer informed.  (0 days pre-DTT, 188 days post-DTT) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10908501-1 </td>
   <td style="text-align:left;"> Clock Stops: 1:Patient DNA (0 days pre-DTT, -62 days post-DTT) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10976753-1 </td>
   <td style="text-align:left;"> Clock Stops: 1:Had to wait for VATs and Histology (-52 days pre-DTT, 0 days post-DTT) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10989311-1 </td>
   <td style="text-align:left;"> Clock Stops: 1:DNA clinic x 2 new address* (-233 days pre-DTT, 0 days post-DTT) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 128716-1 </td>
   <td style="text-align:left;"> Clock Stops: 1:Offered a date in Nov but deferred it 2:Delayed due to needing to cease smoking (0 days pre-DTT, -90 days post-DTT) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 201561-1 </td>
   <td style="text-align:left;"> Clock Stops: 1:Needed iron infusion (0 days pre-DTT, -39 days post-DTT) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 243837-4 </td>
   <td style="text-align:left;"> Clock Stops: 1:Letter went to wrong P. Lyon in radiotherapy (-89 days pre-DTT, 0 days post-DTT) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 815570-1 </td>
   <td style="text-align:left;"> Clock Stops: 1:Delay due to decision by patient re: SABR or Ablation. (0 days pre-DTT, -12 days post-DTT) </td>
  </tr>
</tbody>
</table>


## Including Plots



```
## Warning: Removed 117 rows containing missing values or values outside the scale range (`geom_point()`).
```

```
## Warning: Removed 116 rows containing missing values or values outside the scale range (`geom_point()`).
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
   <td style="text-align:left;"> 10792357-1 </td>
   <td style="text-align:right;"> 415 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 188 </td>
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
   <td style="text-align:left;"> 4288489-1 </td>
   <td style="text-align:right;"> 131 </td>
   <td style="text-align:right;"> NA </td>
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
