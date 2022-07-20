<!-- README.md is generated from README.Rmd. Please edit that file -->

# Salmon studies

## Updated in Chinook in March 2018

1.  Acquired LF for age compositions
2.  Extracted Bayes estimates from 2011-2016 of stock compositions
3.  Compiled age compositions (still using old ALK…should be improved…)

<!-- -->

    new order for stock id is:      

<table style="width:100%;">
<colgroup>
<col style="width: 11%" />
<col style="width: 22%" />
<col style="width: 16%" />
<col style="width: 16%" />
<col style="width: 4%" />
<col style="width: 4%" />
<col style="width: 4%" />
<col style="width: 4%" />
<col style="width: 4%" />
<col style="width: 5%" />
<col style="width: 5%" />
</colgroup>
<thead>
<tr class="header">
<th>1</th>
<th>2</th>
<th>3</th>
<th>4</th>
<th>5</th>
<th>6</th>
<th>7</th>
<th>8</th>
<th>9</th>
<th>10</th>
<th>11</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Russia</td>
<td>Coastal W. AK</td>
<td>Mid-Yukon</td>
<td>Up. Yukon</td>
<td>N. AK Penn.</td>
<td>NW GOA</td>
<td>Copper</td>
<td>NE GOA</td>
<td>Coast. SE AK</td>
<td>BC</td>
<td>WA/OR/CA</td>
</tr>
</tbody>
</table>

dropped 2-strata genetics option for B-season since the data haven’t
been broken out that way…

## Simulation tests for process error estimation

`simpe.tpl`–simulates process error data  
`simpe.dat`–input for simulations

### procedure

block of 20 observations on 20 RE Objective function contains
observation error part and process error bit  
several years of data, multiple observations per year?

## ID-AEQ model

sia.tpl  
sia.dat

## Notes

### Modifications for chinook

1.  Fit the model to “b” season separately from “A” season
2.  Covariance (MVNormal) option unavailable for 2-season model
3.  B-season strata are geographic (not sub-seasonal like chum)

## Stocks

The original model had nine breakouts:

<table>
<colgroup>
<col style="width: 13%" />
<col style="width: 24%" />
<col style="width: 18%" />
<col style="width: 18%" />
<col style="width: 4%" />
<col style="width: 4%" />
<col style="width: 4%" />
<col style="width: 4%" />
<col style="width: 4%" />
</colgroup>
<thead>
<tr class="header">
<th>1</th>
<th>2</th>
<th>3</th>
<th>4</th>
<th>5</th>
<th>6</th>
<th>7</th>
<th>8</th>
<th>9</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>BC-WA-OR</td>
<td>Coast W AK</td>
<td>Cook Inlet</td>
<td>Mid Yukon</td>
<td>N AK Pen</td>
<td>Russia</td>
<td>TBR Taku</td>
<td>Up Yukon</td>
<td>Other</td>
</tr>
</tbody>
</table>

## Change in bycatch values by season and year

The updated estimates from the Regional office differ from what was used
in the FEIS.  
The main differences were in the period 2001 and 2002 for the SE region
during the B season with substantially lower values in the revised
data.  
Table 3.7 from the FEIS was used to compare with the updated RO data.

Using the same data and the new model, the differences are notable
(presumably due to the lag effect of genetics)

## Model runs

Progression for comparison purposes is  
1. Results as presented in the FEIS  
2. Reference model, 2000-2007 (but run for years prior too, 1991-1999)
sia\_a.dat and sai\_b.dat 3. Reference plus, add in post 2007 years and
re-run (to 2012) sia\_a2.dat and sai\_b2.dat 4. Reference plus, but
absent geographic strata in B-season (since recent genetics not broken
out further) sia\_a2.dat, sia\_b3.dat 5. as in 4 but with 2011 data
included, sia\_a4.dat sia\_b4.dat 6. as in 5 but with earlier data
excluded, sia\_a5.dat sia\_b5.dat

Models 2 and 3 are identical (just project new) but differ from Model l)
due to the overlapping AEQ and genetic effects…

Models 3 and 4 only differ in the spatial strata used

SNPs from Guyon for 2008, 2010 are needed for A season, and for
B-season, 2007-2011 data stratified E and W of 170 degrees W
