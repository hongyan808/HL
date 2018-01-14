/*
cd "C:\Users\zilong.zilong-PC.000\Dropbox\Dr_zhao4\data"

import excel "C:\Users\zilong.zilong-PC.000\Dropbox\Dr_zhao4\data\comb1.xls", sheet("Screening") firstrow clear


drop if CIK=="-"

duplicates tag CIK, gen(cc)

 duplicates drop CIK, force
*keep if cc==1


reshape long cp lc tl , i(CIK) j(year)

keep    CIK year CompanyName ExchangeTicker cp lc tl

sort CIK year
saveold temp1_out, replace


import excel "C:\Users\zilong.zilong-PC.000\Dropbox\Dr_zhao4\data\comb2.xls", sheet("Screening") firstrow clear


drop if CIK=="-"

duplicates tag CIK, gen(cc)

 duplicates drop CIK, force
*keep if cc==1


reshape long  ob ttp td , i(CIK) j(year)

keep   CIK year CompanyName ExchangeTicker  ob ttp td

sort CIK year
saveold temp2_out, replace


import excel "C:\Users\zilong.zilong-PC.000\Dropbox\Dr_zhao4\data\comb3.xls", sheet("Screening") firstrow clear


drop if CIK=="-"

duplicates tag CIK, gen(cc)

 duplicates drop CIK, force
*keep if cc==1


reshape long   bd srb sub cl , i(CIK) j(year)

keep  CIK year CompanyName ExchangeTicker   bd srb sub cl

sort CIK year
saveold temp3_out, replace

use temp1_out, clear

sort CIK year
merge CIK year using temp2_out

sort CIK year
cap drop _merge

merge CIK year using temp3_out

saveold debtstructure, replace
*/
***transfer into sas********
 
cd d:\comp 
use zhao4, clear
set more off

drop if sich>=4900 & sich <5000
drop if sich>=6000 & sich<7000
global xx  cp lc tl ob ttp td bd srb sub cl

foreach i of global xx {
	gen `i'_new=real(`i')
	drop `i'
	rename `i'_new `i'
	}

gen compdebt=dlc+dltt
drop if td==.
drop if at==.

gen cp_td=cp/td
gen lc_td=lc/td
gen tl_td=tl/td
gen od_td=(ob+ttp)/td
gen bd_td=bd/td
gen srb_td=srb/td
gen sub_td=sub/td
gen cl_td=cl/td

gen pub_td=srb_td+sub_td

gen cp_at=cp/at
gen lc_at=lc/at
gen tl_at=tl/at
gen od_at=(ob+ttp)/at
gen bd_at=bd/at
gen srb_at=srb/at
gen sub_at=sub/at
gen cl_at=cl/at

gen pub_at=srb_at+sub_at
gen pub=srb+sub
sum cp_td lc_td tl_td od_td bd_td srb_td sub_td cl_td pub_td

gen leverage=(dlc+dltt)/at
 

replace leverage=1 if leverage>1 & leverage !=.
replace leverage=0 if leverage<0



global xx cp_td lc_td tl_td od_td bd_td srb_td sub_td cl_td pub_td
foreach i of global xx {
  replace `i'=1 if `i'>1 & `i' !=.
  replace `i'=0 if `i'<0
  }


  sum cp_at lc_at tl_at od_at bd_at srb_at sub_at cl_at pub_at

global xx cp_at lc_at tl_at od_at bd_at srb_at sub_at cl_at pub_at
foreach i of global xx {
  replace `i'=1 if `i'>1 & `i' !=.
  replace `i'=0 if `i'<0
  }

 label variable bd_td "Bank Debt to Total debt"
  
  drop if year==2015
  
 table year, c(mean  pub_td mean bd_td mean od_td mean cp_td mean cl_td )  
 table year, c(mean lc_td  mean tl_td mean srb_td  mean sub_td )
 
 table year, c(mean  pub_at mean bd_at mean od_at mean cp_at mean cl_at )  
 table year, c(mean lc_at  mean tl_at mean srb_at  mean sub_at )

 
 

 
 
 gen gvkey_new=real(gvkey)
 drop gvkey 
 rename gvkey_new gvkey
 
  bysort cik: egen gvkeymodel=mode(gvkey)
  replace gvkey=gvkeymode if gvkey==.
  
  
 tsset gvkey fyear
 
global xx cp_at lc_at tl_at od_at bd_at srb_at sub_at cl_at pub_at
foreach i of global xx {
  gen d_`i'=d.`i'  
  } 
 
 
   global xx  cp_at lc_at tl_at od_at bd_at srb_at sub_at cl_at pub_at d_cp_at d_lc_at d_tl_at d_od_at d_bd_at d_srb_at d_sub_at d_cl_at d_pub_at
foreach x of global xx{
winsor `x', p(0.01) gen(`x'_w)
drop `x'
rename `x'_w `x'
}



 
 local x at
cap drop `x'_group
gen `x'_group=.
forvalues i=2006/2014 {
xtile `x'_group`i'=`x' if fyear==`i', nq(3)
replace `x'_group=`x'_group`i' if fyear==`i'
drop `x'_group`i'
}

preserve
bysort gvkey: egen at_avg=mean(at)
duplicates drop gvkey, force
xtile at_g=at_avg   , nq(3)
keep gvkey at_g
sort gvkey
 save temp,replace 

restore

sort gvkey
drop _merge
merge gvkey using temp

tsset gvkey fyear
gen dseq=d.seq  // is it seq or teq?
gen dre =d.re 
gen dequity=dseq-dre 
//capx
gen tot_payout=dvc+prstkc
gen inv=capx+aqc+ivch+fuseo-sppe-siv if scf==1 | scf==2 |scf==3
replace inv=capx+aqc+ivch-sppe-siv-ivaco if scf==7

egen ocf=rowtotal(ibc xidoc txdc esubc sppiv fopo fsrco) if scf==1 | scf==2 |scf==3

gen neg_recch=-recch
gen neg_invch=-invch
gen neg_apalch = -apalch 
gen neg_txach=  -txach
gen neg_aoloch= -aoloch

egen ocf1=rowtotal(oancf neg_recch neg_invch neg_apalch neg_txach neg_aoloch exre) if scf==7


replace ocf=ocf1 if scf==7


 table fyear at_group, c(mean bd_at  mean pub_at   )
 
 
 table fyear at_g , c(mean d_bd_at  mean d_pub_at   )
 
 gen leverage=(dlc+dltt)/at


replace leverage=1 if leverage>1 & leverage !=.
replace leverage=0 if leverage<0
 
  table fyear at_group, c(sum bd   sum  pub    )
  table fyear at_g  , c(sum bd   sum  pub  sum compdebt  )
 
 table fyear at_g  , c(mean compdebt  )
  table fyear at_g  , c(mean bd  )
   table fyear at_g  , c(mean pub  )
   table fyear at_g  , c(mean dequity  )
   table fyear at_g  , c(mean tot_payout)
   table fyear at_g  , c(mean capx)
   table fyear at_g  , c(mean inv)
    table fyear at_g  , c(mean aqc)
   table fyear at_g  , c(mean ib)
    table fyear at_g  , c(mean ocf)
	  table fyear at_g  , c(mean leverage)
	  
	  
	  
	  
	    table fyear at_group  , c(sum bd  )
   table fyear at_group  , c(sum pub  )
	  
	   table fyear at_group  , c(mean compdebt  )
  table fyear at_group  , c(mean bd  )
   table fyear at_group  , c(mean pub  )
   table fyear at_g  , c(mean dequity  )
   table fyear at_g  , c(mean tot_payout)
   table fyear at_g  , c(mean capx)
   table fyear at_g  , c(mean inv)
    table fyear at_g  , c(mean aqc)
   table fyear at_g  , c(mean ib)
    table fyear at_group  , c(mean ocf)
	  table fyear at_group  , c(mean leverage)
	  
	   table fyear at_group  , c(mean bd_at  )
   table fyear at_group  , c(mean pub_at  )
   
 /*
  gen cash_at=che/at
  gen dcash=d.cash_at
  
  gen MB=(at-ceq+prcc_f*csho)/at
  
  gen nwc=(wcap-che)/at
  
  gen capex=capx/at
  gen dcapex=d.capex
  
  gen leverage=td/at
  gen dleverage=d.leverage
  
  gen aqc_at=aqc/at
  gen daqc=d.aqc_at

   replace xint=0 if xint==.
   gen CF=(oibdp-xint-txt-dvc)/at

  replace xrd=0 if xrd==.
  gen rd_sale=xrd/sale
  
  gen drd=d.rd_sale

  gen sstk_at=sstk/at

  gen div_pay=(dvc>0 & dvc !=.)
 
  gen repo_at=prstkc/at
  
 gen dd1_at=dd1/at
gen dd2_at=dd2/at
gen dd3_at=dd3/at
gen dd4_at=dd4/at
gen dd5_at=dd5/at 

replace dd1_at=1 if dd1_at>1 & dd1_at!=.
replace dd1_at=0 if dd1_at<0

replace dd2_at=1 if dd2_at>1 & dd2_at!=.
replace dd2_at=0 if dd2_at<0

replace dd3_at=1 if dd3_at>1 & dd3_at!=.
replace dd3_at=0 if dd3_at<0


replace dd4_at=1 if dd4_at>1 & dd4_at!=.
replace dd4_at=0 if dd4_at<0


replace dd5_at=1 if dd5_at>1 & dd5_at!=.
replace dd5_at=0 if dd5_at<0


*replace leverage=1 if leverage>1 & leverage!=.
drop if leverage>1
replace leverage=0 if leverage<0


 
 gen tl_chg=d.tl_at
 
 gen tl_lag=l.tl
 gen notl=1 if tl_lag==0 & tl==0
 
  
local x tl_chg
cap drop `x'_group
gen `x'_group=.
forvalues i=2007/2014 {
xtile `x'_group`i'=`x' if year==`i' &  notl!=1 , nq(5)
replace `x'_group=`x'_group`i' if year==`i' &  notl!=1 
drop `x'_group`i'
}

local x at
cap drop `x'_group
gen `x'_group=.
forvalues i=2006/2014 {
xtile `x'_group`i'=`x' if year==`i' , nq(5)
replace `x'_group=`x'_group`i' if year==`i'
drop `x'_group`i'
}


replace tl_chg_group=11 if notl==1

gen ap_at=ap/at 
gen  rec_at=rectr/at

  global xx  cash_at dcash  at rd_sale drd    MB nwc capex  dcapex  aqc_at daqc CF   tl_chg   repo_at leverage dleverage   sstk_at rec_at ap_at
foreach x of global xx{
winsor `x', p(0.01) gen(`x'_w)
drop `x'
rename `x'_w `x'
}


tabstat  cash_at dcash  at rd_sale drd    MB nwc capex  dcapex  aqc_at daqc CF   tl_chg   repo_at  div_pay leverage dleverage ,  by(tl_chg_group)
tabstat  cash_at dcash  at rd_sale drd    MB nwc capex  dcapex  aqc_at daqc CF   tl_chg   repo_at  div_pay leverage dleverage if year==2007 ,  by(tl_chg_group)
tabstat  cash_at dcash  at rd_sale drd    MB  capex  dcapex  aqc_at daqc CF   tl_chg   repo_at  div_pay leverage dleverage if year==2008 | year==2009 ,  by(tl_chg_group)
tabstat  cash_at dcash  at rd_sale drd    MB nwc capex  dcapex  aqc_at daqc CF   tl_chg   repo_at  div_pay leverage dleverage if year==2009 ,  by(tl_chg_group)
tabstat  cash_at dcash  at rd_sale drd    MB nwc capex  dcapex  aqc_at daqc CF   tl_chg   repo_at  div_pay leverage dleverage if year==2010 ,  by(tl_chg_group)
tabstat  cash_at dcash  at rd_sale drd    MB nwc capex  dcapex  aqc_at daqc CF   tl_chg   repo_at  div_pay leverage dleverage if year==2011 ,  by(tl_chg_group)
tabstat  cash_at dcash  at rd_sale drd    MB nwc capex  dcapex  aqc_at daqc CF   tl_chg   repo_at  div_pay leverage dleverage if year==2012 | year==2013 ,  by(tl_chg_group)
tabstat  cash_at dcash  at rd_sale drd    MB nwc capex  dcapex  aqc_at daqc CF   tl_chg   repo_at  div_pay leverage dleverage if year==2013 ,  by(tl_chg_group)
tabstat  cash_at dcash  at rd_sale drd    MB nwc capex  dcapex  aqc_at daqc CF   tl_chg   repo_at  div_pay leverage dleverage if year==2014 ,  by(tl_chg_group)

tabstat cash_at dcash  at rd_sale drd    MB nwc capex  dcapex  aqc_at daqc CF   tl_chg   repo_at  div_pay leverage dleverage pub_at bd_at srb_at    sub_at  tl_at  lc_td  cl_at, by(year)


tabstat cash_at dcash  at rd_sale drd    MB nwc capex  dcapex  aqc_at daqc CF   tl_chg   repo_at  div_pay leverage dleverage pub_at bd_at srb_at    sub_at  tl_at  lc_at  cl_at if at_group==1, by(year)
tabstat cash_at dcash  at rd_sale drd    MB nwc capex  dcapex  aqc_at daqc CF   tl_chg   repo_at  div_pay leverage dleverage pub_at bd_at srb_at    sub_at  tl_at  lc_at  cl_at if at_group==2, by(year)
tabstat cash_at dcash  at rd_sale drd    MB nwc capex  dcapex  aqc_at daqc CF   tl_chg   repo_at  div_pay leverage dleverage pub_at bd_at srb_at    sub_at  tl_at  lc_at  cl_at if at_group==3, by(year)
tabstat cash_at dcash  at rd_sale drd    MB nwc capex  dcapex  aqc_at daqc CF   tl_chg   repo_at  div_pay leverage dleverage pub_at bd_at srb_at    sub_at  tl_at  lc_at  cl_at if at_group==4, by(year)
tabstat cash_at dcash  at rd_sale drd    MB nwc capex  dcapex  aqc_at daqc CF   tl_chg   repo_at  div_pay leverage dleverage pub_at bd_at srb_at    sub_at  tl_at  lc_at  cl_at if at_group==5, by(year)



tabstat  dd1_at dd2_at dd3_at dd4_at dd5_at, by(year)

 preserve
 keep if year>=2006
bysort gvkey: egen at_avg=mean(at)
duplicates drop gvkey, force
xtile at_avg_g=at_avg, nq(5)
keep gvkey at_avg_g
sort gvkey
save  at_avg,replace
restore

sort gvkey
cap drop _merge
merge gvkey using at_avg



tabstat cash_at dcash  at rd_sale drd    MB nwc capex  dcapex  aqc_at daqc CF   tl_chg   repo_at  div_pay leverage dleverage pub_at bd_at srb_at    sub_at  tl_at  lc_at  cl_at if at_avg_g ==1, by(year)
tabstat cash_at dcash  at rd_sale drd    MB nwc capex  dcapex  aqc_at daqc CF   tl_chg   repo_at  div_pay leverage dleverage pub_at bd_at srb_at    sub_at  tl_at  lc_at  cl_at if at_avg_g ==2, by(year)
tabstat cash_at dcash  at rd_sale drd    MB nwc capex  dcapex  aqc_at daqc CF   tl_chg   repo_at  div_pay leverage dleverage pub_at bd_at srb_at    sub_at  tl_at  lc_at  cl_at if at_avg_g ==3, by(year)
tabstat cash_at dcash  at rd_sale drd    MB nwc capex  dcapex  aqc_at daqc CF   tl_chg   repo_at  div_pay leverage dleverage pub_at bd_at srb_at    sub_at  tl_at  lc_at  cl_at if at_avg_g ==4, by(year)
tabstat cash_at dcash  at rd_sale drd    MB nwc capex  dcapex  aqc_at daqc CF   tl_chg   repo_at  div_pay leverage dleverage pub_at bd_at srb_at    sub_at  tl_at  lc_at  cl_at if at_avg_g ==5, by(year)



save temp1,replace


use temp1, clear
replace fyear=year if fyear==.

egen ocf=rowtotal(ibc xidoc txdc esubc sppiv fopo fsrco) if scf==1 | scf==2 |scf==3

gen neg_recch=-recch
gen neg_invch=-invch
gen neg_apalch = -apalch 
gen neg_txach=  -txach
gen neg_aoloch= -aoloch

egen ocf1=rowtotal(oancf neg_recch neg_invch neg_apalch neg_txach neg_aoloch exre) if scf==7


replace ocf=ocf1 if scf==7


replace sppe=0 if sppe==.
replace siv=0 if siv==.
replace ivaco=0 if ivaco==.

gen inv=capx+aqc+ivch+fuseo-sppe-siv if scf==1 | scf==2 |scf==3
replace inv=capx+aqc+ivch-sppe-siv-ivaco if scf==7


replace wcapc=0 if wcapc==.

replace recch=0 if recch==.
replace invch=0 if invch==.
replace apalch=0 if apalch==.
replace txach=0 if txach==.
replace aoloch=0 if aoloch==.
replace ivstch=0 if ivstch==.
replace fiao=0 if fiao==.

gen chg_nwc=wcapc+chech if scf==1
replace chg_nwc=chech-wcapc if scf==2 | scf==3
replace chg_nwc=chech-recch-invch-apalch-txach-aoloch-ivstch-fiao

tsset
gen payout=dvc+prstkc-l.dvc

gen payout_at=payout/at
gen dnwc_at=chg_nwc/at
gen inv_at=inv/at

gen ocf_at=ocf/at
gen ocf_shortfall=0 if ocf>0
replace ocf_shortfall=-ocf if ocf<0

gen ocf_short_at=ocf_shortfall/at

gen dlc_at=dlc/at

replace dvc=. if dvc==0
gen dvc_at=dvc/at

 tsset gvkey fyear
*gen change in debt
gen dtd=d.td 
gen dtd_lagat=dtd/at
 
gen capx_at=capx/at

cap drop dseq
gen dseq=d.seq  // is it seq or teq?
gen dre =d.re 

gen dequity=dseq-dre 
gen dequity_at=dequity/at

 

  global xx inv_at ocf_short_at  dnwc_at payout_at dlc_at dvc_at ocf_at dtd_lagat capx_at aqc_at ocf_at dequity_at
  
foreach x of global xx{
winsor `x', p(0.01) gen(`x'_w)
drop `x'
rename `x'_w `x'
}

tabstat inv_at  capx_at aqc_at ocf_short_at  dnwc_at payout_at dvc_at repo_at  dtd_lagat ocf_at dequity_at if dtd_lagat>0 & dtd_lagat!=.  , by(year)
tabstat inv_at  capx_at aqc_at ocf_short_at  dnwc_at payout_at dvc_at repo_at  dtd_lagat ocf_at dequity_at if dtd_lagat<0  , by(year) 
 

tabstat inv_at  ocf_short_at  dnwc_at payout_at   dtd_lagat ocf_at dequity_at if dtd_lagat>0 & dtd_lagat!=.  , by(year)
tabstat inv_at  ocf_short_at  dnwc_at payout_at   dtd_lagat ocf_at dequity_at if dtd_lagat<0  , by(year) 
 
 
tabstat inv_at  capx_at aqc_at ocf_short_at  dnwc_at payout_at dvc_at repo_at dtd_lagat if dtd_lagat>0.1 & dtd_lagat!=.  , by(year)
tabstat inv_at  capx_at aqc_at ocf_short_at  dnwc_at payout_at dvc_at repo_at dtd_lagat if dtd_lagat<-0.1  , by(year) 
 

tabstat inv_at ocf_short_at  dnwc_at payout_at if dtd_lagat>0 & dtd_lagat!=.  , by(year)
tabstat inv_at ocf_short_at  dnwc_at payout_at if dtd_lagat<0  , by(year) 
 
tabstat  rec_at ap_at cash_at inv_at ocf_short_at  dnwc_at payout_at cash_at  dlc_at repo_at leverage sstk_at if at_group==1, by(year)
 tabstat rec_at ap_at cash_at inv_at ocf_short_at  dnwc_at payout_at  repo_at leverage sstk_at if at_group==2, by(year)
 tabstat rec_at ap_at cash_at inv_at ocf_short_at  dnwc_at payout_at repo_at leverage sstk_at if at_group==3, by(year)
 tabstat rec_at ap_at cash_at  inv_at ocf_short_at  dnwc_at payout_at repo_at leverage sstk_at if at_group==4, by(year)
 tabstat rec_at ap_at cash_at inv_at ocf_short_at  dnwc_at payout_at repo_at leverage sstk_at if at_group==5, by(year)

 tabstat rec_at ap_at cash_at inv_at ocf_short_at ocf_at dnwc_at payout_at  repo_at dvc_at leverage dleverage sstk_at if at_avg_g==1, by(year)
 tabstat rec_at ap_at cash_at inv_at ocf_short_at ocf_at dnwc_at payout_at  repo_at dvc_at leverage dleverage sstk_at if at_avg_g==2, by(year)
 tabstat rec_at ap_at cash_at inv_at ocf_short_at ocf_at dnwc_at payout_at repo_at dvc_at leverage dleverage sstk_at if at_avg_g==3, by(year)
 tabstat rec_at ap_at cash_at inv_at ocf_short_at ocf_at dnwc_at payout_at repo_at dvc_at leverage dleverage sstk_at if at_avg_g==4, by(year)
 tabstat rec_at ap_at cash_at inv_at ocf_short_at ocf_at dnwc_at payout_at repo_at dvc_at leverage dleverage sstk_at if at_avg_g==5, by(year)

  tabstat rec_at ap_at cash_at inv_at ocf_short_at  dnwc_at payout_at  repo_at dvc_at leverage sstk_at if at_group==1, by(year)
 tabstat rec_at ap_at cash_at inv_at ocf_short_at  dnwc_at payout_at  repo_at dvc_at leverage sstk_at if at_group==2, by(year)
 tabstat rec_at ap_at cash_at inv_at ocf_short_at  dnwc_at payout_at repo_at dvc_at leverage sstk_at if at_group==3, by(year)
 tabstat rec_at ap_at cash_at inv_at ocf_short_at  dnwc_at payout_at repo_at dvc_at leverage sstk_at if at_group==4, by(year)
 tabstat rec_at ap_at cash_at inv_at ocf_short_at  dnwc_at payout_at repo_at dvc_at leverage sstk_at if at_group==5, by(year)
 
 
 tsset
 egen tot_lia=rowtotal( ap lco dlc txp   dltt txditc lo)
 gen dat=d.at
 gen dtot_lia=d.tot_lia
 gen dseq=d.seq
  
 
 reg dtot_lia dat
 reg dtot_lia dseq

 
  reg dtot_lia dat
 reg dseq dat
 
 
 
 
 
/*
gen ocf_shortfall=0 if ocf>0
replace ocf_shortfall=-ocf if ocf<0

gen dtl = 0
replace   dtl= d.tl if d.tl>0 &  d.tl!=.

gen ocf_duse1=ocf_shortfall/dtl
gen i_duse1=inv/dtl
gen payout_duse1=payout/dtl
gen nwc_duse1=chg_nwc/dtl

gen ocf_duse=ocf_duse1/(ocf_duse1+i_duse1+payout_duse1+nwc_duse1)
gen i_duse=i_duse1/(ocf_duse1+i_duse1+payout_duse1+nwc_duse1)
gen payout_duse=payout_duse1/(ocf_duse1+i_duse1+payout_duse1+nwc_duse1)
gen nwc_duse=nwc_duse1/(ocf_duse1+i_duse1+payout_duse1+nwc_duse1)

br ocf_duse i_duse payout_duse nwc_duse

gen use="OCF Shortfall" if ocf_duse>.5 & ocf_duse !=.
replace use="Invetment" if i_duse>.5 & i_duse !=.
replace use="payout" if payout_duse>.5 & payout_duse!=.
replace use="WC" if nwc_duse>.5 & nwc_duse !=.
replace use="mutiple" if ocf_duse<.5 & i_duse<.5 & payout_duse<.5 & nwc_duse<.5

keep if tl_chg>0.05 & tl_chg!=.

tab use
table fyear use 
 
tabstat  cash_at dcash  at rd_sale drd    MB nwc capex  dcapex  aqc_at daqc CF   tl_chg   repo_at  div_pay leverage dleverage if year==2012| year==2013, by(use)

tabstat  cash_at dcash  at rd_sale drd    MB nwc capex  dcapex  aqc_at daqc CF   tl_chg   repo_at  div_pay leverage dleverage if year==2008| year==2009, by(use)
