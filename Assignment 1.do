clear all
cd "/Users/noviawidyachairani/Desktop/Graduate/Financial Econometrics/Assignment 1"
use Assignment_1_Data.dta

* 1. Graph each of these variables separately. What patterns do you observe over the period considered?
* Additional resource: https://www.youtube.com/watch?v=7WjUJR2-5us
scatter crudeoil crudeoil
scatter gold gold
scatter sp500 sp500
scatter bitcoin bitcoin

*-------------------

* Or we can use the twoway scatter as below
twoway (scatter crudeoil crudeoil) (lfit crudeoil crudeoil)
twoway (scatter gold gold) (lfit gold gold)
twoway (scatter sp500 sp500) (lfit sp500 sp500)
twoway (scatter bitcoin bitcoin) (lfit bitcoin bitcoin)


*************************************************************************************************************


* 2. Transforming the data set

* 2.a. Finding the log for each variables using log function for each variables
gen lcrudeoil = ln(crudeoil)
gen lgold = ln(gold)
gen lsp500 = ln(sp500)
gen lbitcoin = ln(bitcoin)

*-------------------

* 2.b. Finding the monthly growth for each variables
gen lcrudeoil_d = d.lcrudeoil
gen lgold_d = d.lgold
gen lsp500_d = d.lsp500
gen lbitcoin_d = d.lbitcoin

*-------------------

* 2.c. Making the scatter for all the growth of logged variables to see the relationship
scatter d.lcrudeoil d.lgold d.lsp500 d.lbitcoin
twoway (scatter d.lbitcoin d.lbitcoin) (lfit d.lbitcoin d.lbitcoin) (scatter d.lcrudeoil d.lcrudeoil) (lfit d.lcrudeoil d.lcrudeoil) (scatter d.lsp500 d.lsp500) (lfit d.lsp500 d.lsp500) (scatter d.lgold d.lgold) (lfit d.lgold d.lgold)


*************************************************************************************************************


* 3. Regression model

* 3.a. Obtain the multiple regression model
* Unrestricted regression model
reg bitcoin crudeoil sp500 gold
* Restricted regression model
reg bitcoin crudeoil
* Display the coefficients
display _b[crudeoil]
display _b[sp500]
display _b[gold]

*-------------------

* 3.b. RAMSEY RESET is using ovtest after the multiple regression
* Additional resource : https://www.youtube.com/watch?v=QWiGWYmtWc8
reg bitcoin crudeoil sp500 gold
estat ovtest

*-------------------

* 3.c. Heteroskedasticity
* Visual test
* Additional source: https://www.stata.com/features/overview/heteroskedastic-linear-regression/
scatter bitcoin crudeoil sp500 gold
reg bitcoin crudeoil sp500 gold
predict yhat, xb
predict u, residual
gen u_sq=u^2
scatter u_sq crudeoil
scatter u_sq sp500 
scatter u_sq gold
scatter u_sq yhat

* Visual heteroskedasticity commands after regression
rvpplot crudeoil, yline(0) /* a residual vs predictor plot*/
rvpplot sp500, yline(0)
rvpplot gold, yline(0)
rvfplot, yline(0)
scatter bitcoin crudeoil sp500 gold date

* BREUSH-PAGAN TEST (similar to white test) = Joint significance of any predictors on the squared residualwhich is our proxy/ stand-in estimator for the observation biopsy rate, observation by observtion error variance
* Additional resource : https://www.youtube.com/watch?v=OZyZaHVcE78
reg u_sq crudeoil sp500 gold
* Recall the originalregression
reg bitcoin crudeoil sp500 gold
hettest, rhs fstat
estat hettest, iid

* WHITE's TEST: involves regressing the squared residual/ the squares of all variables, as well as any cross products. We need to have cross product terms for every pair of variables in the model 
* Additional resource : https://www.youtube.com/watch?v=ONfwtKmidvI&t=93s
* Simple way (from the lecture)
gen crudeoil_sq = crudeoil^2
reg u_sq crudeoil crudeoil_sq
gen sp500_sq = sp500^2
reg u_sq sp500 sp500_sq
gen gold_sq = gold^2
reg u_sq gold gold_sq
reg bitcoin crudeoil sp500 gold
estat imtest, white
* Another way by crossing each variable
gen oilxsp = crudeoil * sp500 /* Cross crudeoil x sp500*/
reg u_sq crudeoil sp500 crudeoil_sq sp500_sq oilxsp
gen oilxgold = crudeoil * gold /* Cross crudeoil x gold*/
reg u_sq crudeoil gold crudeoil_sq gold_sq oilxgold
gen spxgold = sp500 * gold /* Cross sp500 x gold*/
reg u_sq sp500 gold sp500_sq gold_sq spxgold
* Recall the original regression
reg bitcoin crudeoil sp500 gold
estat imtest, white
* White's corrected standard errors
reg bitcoin crudeoil sp500 gold, robust /* Correct the standard errors for heteroskedasticity */
reg bitcoin crudeoil sp500 gold, vce(hc3)
* Generalised least squares
reg bitcoin crudeoil sp500 gold [aweight=1/crudeoil]
reg bitcoin crudeoil sp500 gold [aweight=1/sp500]
reg bitcoin crudeoil sp500 gold [aweight=1/gold]
* Rescaling dependent variable (use "gen lbitcoin = ln(bitcoin)" if lbitcoin not yet defined)
reg lbitcoin crudeoil sp500 gold  

*-------------------

* 3.d. Autocorrelation
* Source: https://www.youtube.com/watch?v=5WZF0o2we4I
clear all
cd "/Users/noviawidyachairani/Desktop/Graduate/Financial Econometrics/Assignment 1"
use Assignment_1_Data.dta

* Repeat the log
gen lcrudeoil = ln(crudeoil)
gen lgold = ln(gold)
gen lsp500 = ln(sp500)
gen lbitcoin = ln(bitcoin)

* GRAPHICAL ANALYSIS
reg lbitcoin lcrudeoil lsp500 lgold
predict uhat, residual
tsline uhat /* plot residual*/
* Making the above graph to look better
tsline uhat if e(sample)==1, yline(0)
scatter u l.u /* plot residuals against their first lag */
* The residual vs its own lag (today's residual is a function of yesterday's residual)
reg u l.u

* BREUSCH-GODFREY Test
* Additional resource: https://www.youtube.com/watch?v=O3QaWBT97t4
* Time series
gen date1 = q(2005q1)+_n-1
format date1 %tq
tsset date1
* To check the autocorrelation (here the R-squared is pretty = autocorrelation problem)
predict u, residual
reg u l.u l2.u bitcoin crudeoil sp500 gold
* Serial correlation of order 1
reg lbitcoin lcrudeoil lsp500 lgold
reg u lcrudeoil lsp500 lgold l.u
estat bgodfrey, lags(1) nomiss0
* Serial correlation of order 2
reg lbitcoin lcrudeoil lsp500 lgold
estat bgodfrey, lags(2) nomiss0
* Newey west SEs
newey lbitcoin lcrudeoil lsp500 lgold, lag(1)
* Serial correlation of order 2, continued
reg u lcrudeoil lsp500 lgold l1.u l2.u /* get (n-p)R^2 and compare it to chi2(p)*/
* Resolving autocorrelation - sometimes autocorrelation stems from mis-specified dynamics
reg lbitcoin lcrudeoil lsp500 lgold

*-------------------

* 3.e. Is the residual normally distributed?
hist u
hist u, normal
* JARQUE-BERA TEST (this is a user written command to install it enter 'ssc install jb') https://www.youtube.com/watch?v=4YpHJ63IRvU
ssc install jb
jb u
*SKEWNESS-KURTOSIS TEST (JB with a tweak for sample size)
sktest u

*-------------------

* 3.f. Reinterpreting the regression results, by correcting the standard error
reg bitcoin crudeoil sp500 gold, robust
reg lbitcoin lcrudeoil lsp500 lgold /*Mis-specification of functional form */

*-------------------

* 3.g. Checking the rebust model, whether it is misspecified or not, using RAMSEY Test
estat ovtest

*************************************************************************************************************


* Additional resources used to study:

* More about twoway scatter function: https://www.youtube.com/watch?v=7WjUJR2-5us

* Multiple regression & finding: Yt-hat & Ut-hat: https://www.youtube.com/watch?v=i4Lk1h252Yo

* RAMSEY Reset test: https://www.youtube.com/watch?v=QWiGWYmtWc8

* Heteroskedasticity
*.   Graphical analysis: https://www.stata.com/features/overview/heteroskedastic-linear-regression/
*.   Breusch-Pagan test: https://www.youtube.com/watch?v=OZyZaHVcE78
*.   White's test: https://www.youtube.com/watch?v=ONfwtKmidvI

* Autocorrelations
*.   Graphical analysis: -
*.   Breusch-Godfrey: https://www.youtube.com/watch?v=O3QaWBT97t4

* Residuals normally distributed
*.   Graphical analysis: -
*.   Jarque-Bera test: https://www.youtube.com/watch?v=dq-HNPVO_SI
                     * https://www.youtube.com/watch?v=Bp3DkCrsqjo
					 * https://www.youtube.com/watch?v=4YpHJ63IRvU
*.   Skewness-Kurtosis = Jarque-Bera test: https://www.youtube.com/watch?v=RvPhg45ZPTk






















