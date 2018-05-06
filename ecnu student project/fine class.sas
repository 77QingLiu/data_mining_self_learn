
* ---------------------------------------------------------- ;
* fine_class                                                 ;
* ---------------------------------------------------------- ;
* Performs fine classing (quantile into &nbins bins, then    ;
* counting y=1 occurences in the data set).  Determines      ;
* coefficients using Weight-of-Evidence (WOE).  THERE IS AN  ;
* OPPORTUNITY TO MODIFY THIS CODE SO THAT IT CAN TELL IF THE ;
* ACCEPT/REJECT COLUMNS WERE SUPPLIED BY TESTING IF          ;
* acceptvar=0, rejectvar=0                                   ;
* ---------------------------------------------------------- ;
* Arguments:                                                 ;
* ---------------------------------------------------------- ;
* intable    = the SAS data set to analyse                   ;
* classvar   = the variable to class (the x-var)             ;
* nbins      = the number of bins to class by                ;
* missingval = define a missing value e.g. { ., 9999, -1 }   ;
* goodvar    = the name of the GOOD variable                 ;
* badvar     = the name of the BAD variable                  ;
* acceptvar  = the name of the ACCEPT variable               ;
* rejectvar  = the name of the REJECT variable               ;
* acceptgbs  = 1 if bad rate of accepts/rejects is desired   ;
* weight_g   = variable name of the GOOD-variable weight     ;
* weight_b   = variable name of the BAD-variable weight      ;
* weight_a   = variable name of the ACCEPT-variable weight   ;
* weight_r   = variable name of the REJECT-variable weight   ;
* ivtable    = OPTIONAL, the name of a table to APPEND the   ;
*              IV (Information Value) metrics to             ;
* cleanup    = 1 if you want to delete temp-tables created   ;
* tol        = a small value to avoid boundary conditions    ;
*              where you have 100% good or 100% bad in a     ;
*              bin - make a small number, e.g. 0.5, 1.0      ;
* ---------------------------------------------------------- ;
* Example of Use:                                            ;
* ---------------------------------------------------------- ;
*%fine_class (
	intable=dc_test,
	classvar=predc,
	nbins=50,
	cleanup=0
);
* ---------------------------------------------------------- ;
%macro fine_class (
	intable=,
	classvar=,
	nbins=       20,
	missingval=  .,
	goodvar=     good,
	badvar=      bad,
	acceptvar=   0,
	rejectvar=   0,
	acceptgbs=   0,
	weight_g=    1,
	weight_b=    1,
	weight_a=    1,
	weight_r=    1,
	ivtable=     0,
	cleanup=     1,
	sascode=     1,
	tol=         0.001
);
	* these are local macro variables, so declare them so I  ;
	* do not accidentally overwrite a global variable here   ;
	%local nobs_per_quantile;
	%local myfmt;
	%local filename;
	%local nobs;
	%local max_&classvar;
	%local min_&classvar;
	%local range;
	%local iv_ar;
	%local iv_gb;
	* if &acceptvar^=0, need a temp table to calculate bads accepted, etc ;
	%if &acceptvar^=0 and &rejectvar^=0 %then %do;
		data _fine000;
			set &intable;
			%if &acceptgbs^=0 %then %do;
				_acceptgood=accept*good;
				_acceptbad= accept*bad;
				_rejectgood=reject*good;
				_rejectbad= reject*bad;
			%end;
			keep 
				&classvar &goodvar &badvar &acceptvar &rejectvar
				%if &acceptgbs^=0 %then %do;
					_acceptgood _acceptbad
					_rejectgood _rejectbad
				%end;
			;
		run;
		%let mytable=_fine000;
	%end;
	%else %do;
		%let mytable=&intable;
	%end;
	* for each x-var outcome, accumulate the good/bads ;
	proc summary data=&mytable missing;
		class &classvar;
		var &goodvar &badvar 
			%if &acceptvar^=0 and &rejectvar^=0 %then %do;
				&acceptvar &rejectvar
				%if &acceptgbs^=0 %then %do;
					_acceptgood _acceptbad
					_rejectgood _rejectbad
				%end;
			%end;
		;
		output out= _fine001(where=(_type_=1)) sum=;
	run;
	* separate missing and non-missing &classvar values, ;
	* e.g. if a missing value is coded as 99999 then     ;
	* it cannot be quantiled with non-missing values     ;
	data 
		_fine001miss
		_fine001nomiss
	;
		set _fine001;
		drop _type_ ;
		if &classvar=&missingval then output _fine001miss;
		else output _fine001nomiss;
	run;
	* get quantiles for the non-missing data;
	proc sql noprint;
		select count(*)/&nbins into :nobs_per_quantile from &intable;
	quit;
	%put nobs_per_quantile=&nobs_per_quantile;
	data _fine002;
		set _fine001nomiss;
		retain _rec 0;
		_rec+_freq_;
		_quantile=floor((_rec-1)/&nobs_per_quantile);
		drop _rec _freq_;
	run;
	* weight the non-missing data ;
	data _fine002;
		set _fine002;
		* weight good / bad / accept / reject outcomes ;
		* using the appropriate weight-variables       ;
		%if &weight_g^=1 %then %do;
			&goodvar=&goodvar*&weight_g;
			%if &acceptgbs^=0 %then %do;
				_acceptgood=_acceptgood*&weight_g;
				_rejectgood=_rejectgood*&weight_g;
			%end;
		%end;
		%if &weight_b^=1 %then %do;
			&badvar=&badvar*&weight_b;
			%if &acceptgbs^=0 %then %do;
				_acceptbad=_acceptbad*&weight_g;
				_acceptbad=_acceptbad*&weight_g;
			%end;
		%end;
		%if &weight_a^=1 %then %do;
			&acceptvar=&acceptvar*&weight_a;
			%if &acceptgbs^=0 %then %do;
				_acceptbad= _acceptbad*&weight_g;
				_acceptgood=_acceptgood*&weight_g;
			%end;
		%end;
		%if &weight_r^=1 %then %do;
			&rejectvar=&rejectvar*&weight_r;
			%if &acceptgbs^=0 %then %do;
				_rejectbad= _rejectbad*&weight_g;
				_rejectgood=_rejectgood*&weight_g;
			%end;
		%end;
		* change label of _quantile ;
		label _quantile='_quantile';
	run;
	* combine missing and non-missing values;
	data _fine003;
		set
			_fine002
			_fine001miss(in=m)
		;
		if m=1 then _quantile= -1;
	run;
	* aggregate by quantile, more decimals mean more accuracy  ;
	* when creating the [balfmt] format further down  ;
	proc sql;
		create table _fine004 as
		select
			_quantile,
			avg(&classvar)    as avg_&classvar format=11.8,
			min(&classvar)    as min_&classvar format=11.8,
			max(&classvar)    as max_&classvar format=11.8,
			%if &acceptvar^=0 and &rejectvar^=0 %then %do;
				sum(&acceptvar)   as n_accept,
				sum(&rejectvar)   as n_reject,
				%if &acceptgbs^=0 %then %do;
					sum(_acceptgood)   as n_accept_good,
					sum(_rejectgood)   as n_reject_good,
					sum(_acceptbad)    as n_accept_bad,
					sum(_rejectbad)    as n_reject_bad,
				%end;
			%end;
			sum(&goodvar)     as n_good,
			sum(&badvar)      as n_bad
		from
			_fine003
		group by
			_quantile
		;
	quit;
	* get some totals;
	proc sql noprint;
		select
			sum(n_good+n_bad)
		into
			:nobs
		from 
			_fine004
		;
	quit;
	%put DEBUG >> nobs=&nobs;
	* below are bucket ranges, I want to format the numbers nicely    ;
	* and automatically with the appropriate number of decimal points ;
	proc sql noprint;
		select 
			max(&classvar),
			min(&classvar),
			floor(max(&classvar)-min(&classvar))
		into 
			:max_&classvar,
			:min_&classvar,
			:range
		from &intable;
	quit;
	%if &range <= 2 %then %do;
		%let myfmt=6.4;
	%end;
	%else %if &range <= 10 %then %do;
		%let myfmt=best7.3;
	%end;
	%else %if &range <= 100 %then %do;
		%let myfmt=best8.2;
	%end;
	%else %if &range <= 1000 %then %do;
		%let myfmt=best8.1;
	%end;
	%else %do;
		%let myfmt=best8.0;
	%end;
	%put DEBUG >> min_&classvar=&&min_&classvar, max_&classvar=&&max_&classvar, range=&range, myfmt=&myfmt;
	* now that I have my format, I want to apply it to _fine004;
	data _fine004;
		set _fine004;
		* keep min_&classvar, max_&classvar with the ACCURATE formats, but  ;
		* apply &myfmt to create bins of &classvar with appropriate decimal ;
		* places;
		format min_fmt max_fmt &myfmt;
		if min_&classvar=. and max_&classvar=. then do;
			* special case, . causes format statements below to break down ;
			avg_&classvar= -9e99;
			min_&classvar= -9e99;
			max_&classvar= -9e99;
		end;
		min_fmt=min_&classvar;
		max_fmt=max_&classvar;
	run;
	* write out the format to file, then include it to actually run the format   ;
	* code, it is not elegant but it is effective ;
	%if &SYSSCP=WIN %then %do;
		* this is a Windows-OS;
		%let filename=D:\_TEMP.TXT;
	%end;
	%else %do;
		* assume UNIX;
		data _null_;
			filename=&SASWORKLOCATION || "_temp.txt";
			call symput('filename',filename);
		run;
		%put filename= [&filename];
	%end;
	data _null_;
		set _fine004 end=eof;
		file "&filename"; * write the [proc format] to file;
		if _n_=1 then do;
			put "proc format; value balfmt .='total'";
		end;
		if min_&classvar=-9e99 and max_&classvar=-9e99 then do;
			* special case, . causes format statements to break down;
			put  min_&classvar '-' max_&classvar "='missing'";
		end;
		else do;
			* most cases use this put statement;
			put  min_&classvar '-' max_&classvar "='" min_fmt ' to ' max_fmt "'";
		end;
		if eof then put "; 	run;";
	run;
	* find out what the total number of goods, bads are;
	proc sql noprint;
		select
			%if &acceptvar^=0 and &rejectvar^=0 %then %do;
				sum(n_accept),
				sum(n_reject),
			%end;
			%if &acceptgbs^=0 %then %do;
				sum(n_accept_good),
				sum(n_accept_bad),
				sum(n_reject_good),
				sum(n_reject_bad),
			%end;
			sum(n_good),
			sum(n_bad)
		into
			%if &acceptvar^=0 and &rejectvar^=0 %then %do;
				:naccepts,
				:nrejects,
			%end;
			%if &acceptgbs^=0 %then %do;
				:nacceptgoods,
				:nacceptbads,
				:nrejectgoods,
				:nrejectbads,
			%end;
			:ngoods,
			:nbads
		from
			_fine004
		;
	quit;
	%if &acceptvar^=0 and &rejectvar^=0 %then %do;
		%put DEBUG >> ngoods=&ngoods, nbads=&nbads, naccepts=&naccepts, nrejects=&nrejects;
	%end;
	%else %do;
		%put DEBUG >> ngoods=&ngoods, nbads=&nbads;
	%end;
	%if &acceptgbs^=0 %then %do;
		%put DEBUG >> nacceptgoods=&nacceptgoods, nacceptbads=&nacceptbads, nrejectgoods=&nrejectgoods, nacceptbads=&nacceptbads;
	%end;
	%include "&filename"; * read the [proc format] from file;
	* calculate further metrics by quantile;
	data _fine_quantiles;
		set _fine004;
		format avg_&classvar balfmt. ;
		retain cumul_total iv_ar iv_gb 0.0;
		rec=_n_;
		%if &acceptvar^=0 and &rejectvar^=0 %then %do;
			ar_total=n_accept+n_reject;
			* dn_accept, dn_reject add up to 100 over all rows;
			dn_accept=n_accept/&naccepts*100.0;
			dn_reject=n_reject/&nrejects*100.0;
			* the odds, a precursor to WOE;
			ar_odds=dn_accept/dn_reject;
			accept_rate=n_accept/ar_total*100.0;
			%if &acceptgbs^=0 %then %do;
				accept_good_rate=n_accept_good/n_accept*100.0;
				accept_bad_rate=n_accept_bad/n_accept*100.0;
				reject_good_rate=n_reject_good/n_reject*100.0;
				reject_bad_rate=n_reject_bad/n_reject*100.0;
			%end;
		%end;
		gb_total=n_good+n_bad;
		* dn_good, dn_bad add up to 100 over all rows;
		dn_good=n_good/&ngoods*100.0;
		dn_bad=n_bad/&nbads*100.0;
		* if there are 100% goods or 100% bads then   ;
		* the woe is undefined, so try to APPROXIMATE ;
		* by adding a small number (TOL) ;
		if dn_good<&TOL then dn_good=&TOL;
		if dn_bad <&TOL then dn_bad =&TOL;
		* the odds, a precursor to WOE;
		gb_odds=dn_good/dn_bad;
		bad_rate=n_bad/gb_total*100.0;
		row_total=gb_total/&nobs*100.0;
		cumul_total+row_total;
		%if &acceptgbs^=0 %then %do;
			dn_accept_good=n_accept_good/&nacceptgoods*100.0;
			dn_accept_bad =n_accept_bad /&nacceptbads*100.0;
			dn_reject_good=n_reject_good/&nrejectgoods*100.0;
			dn_reject_bad =n_reject_bad /&nrejectbads*100.0;
			accept_gb_odds=dn_accept_good/dn_accept_bad;
			reject_gb_odds=dn_reject_good/dn_reject_bad;
		%end;
		* woe, IV (ar), see Naeem Siddiqi p.81, ADDING logic that if   ;
		* there are 100% accepts or 100% rejects in a bin then instead ;
		* of having an undefined WOE, add &tol to dn_accept or         ;
		* dn_reject so that there is no division by zero issue when    ;
		* calculating the WOE ;
		%if &acceptvar^=0 and &rejectvar^=0 %then %do;
			if ar_odds>0 then do;
				* non-boundary condition, at least 1 accept and 1 reject;
				woe_ar=log(ar_odds);
			end;
			else if ar_odds=0 then do;
				* boundary condition, no accepts;
				woe_ar=log(&tol/dn_reject);
			end;
			else do;
				* boundary condition, no rejects;
				woe_ar=log(dn_accept/&tol);
			end;
			iv_ar+(dn_good-dn_bad)/100.0*woe_ar;
		%end;
		* woe, IV (gb), see Naeem Siddiqi p.81, ADDING logic that if   ;
		* there are 100% goods or 100% bads in a bin then instead of   ;
		* having an undefined WOE, add &tol to dn_bad or dn_good so    ;
		* that there is no division by zero issue when calculating the ;
		* WOE ;
		if gb_odds>0 then do;
			* non-boundary condition, at least 1 good and 1 bad;
			woe_gb=log(gb_odds);
		end;
		else if gb_odds=0 then do;
			* boundary condition, no goods;
			woe_gb=log(&tol/dn_bad);
		end;
		else do;
			* boundary condition, no bads;
			woe_gb=log(dn_good/&tol);
		end;
		iv_gb+(dn_good-dn_bad)/100.0*woe_gb;
	run;
	* sum the columns ;
	proc sql;
		create table _fine_total as
		select
			%if &acceptvar^=0 and &rejectvar^=0 %then %do;
				sum(ar_total) as ar_total,
				sum(n_reject) as n_reject,
				sum(n_accept) as n_accept,
				sum(n_accept)/&nobs*100.0 as accept_rate,
				sum(n_accept)/sum(n_reject) as ar_odds,
			%end;
			%if &acceptgbs^=0 %then %do;
				sum(n_accept_good)            as n_accept_good,
				sum(n_accept_bad )            as n_accept_bad,
				&nacceptgoods/&naccepts*100.0 as accept_good_rate,
				&nacceptbads/&naccepts*100.0  as accept_bad_rate,
				&nacceptgoods/&nacceptbads    as accept_gb_odds,
				sum(n_reject_good)            as n_reject_good,
				sum(n_reject_bad )            as n_reject_bad,
				&nrejectgoods/&nrejects*100.0 as reject_good_rate,
				&nrejectbads/&nrejects*100.0  as reject_bad_rate,
				&nrejectgoods/&nrejectbads    as reject_gb_odds,
			%end;
			sum(gb_total) as gb_total,
			sum(n_good) as n_good,
			sum(n_bad) as n_bad,
			sum(n_bad)/&nobs*100.0 as bad_rate,
			sum(n_good)/sum(n_bad) as gb_odds,
			100.0 as dn_bad,
			100.0 as dn_good,
			100.0 as row_total,
			max(cumul_total) as cumul_total,
			0.0 as woe_gb
			%if &acceptvar^=0 and &rejectvar^=0 %then %do;
				,0.0 as woe_ar			
			%end;
		from
			_fine_quantiles
		;
	quit;
	* some formats;
	proc format;
		value typs
			0='Total Sample'
			1='Marginal Classings'
		;
	run;
	* group quantile, total tables together;
	data _fine;
		set 
			_fine_quantiles
			_fine_total(in=t)
		;
		_type_=t;
		* create a subgroup variable, I will use to make negative  ;
		* woe values have red bars in the graphs, positive values  ;
		* have blue bars ;
		if woe_gb<0.0 then subgroup=1; else subgroup=2;
		label
			%if &acceptvar^=0 and &rejectvar^=0 %then %do;
				ar_total=     'APPS * TOTAL'
				woe_ar=       'WOE * AR'
				n_accept=     'NO. * ACCEPTS'
				n_reject=     'NO. * REJECTS'
				accept_rate=  'ACCEPT * RATE * %'
				ar_odds=      'AR * ODDS'
			%end;
			%if &acceptgbs^=0 %then %do;
				n_accept_good=  'ACCEPT *GOODS'
				n_accept_bad=   'ACCEPT *BADS'
				n_reject_good=  'REJECT *GOODS'
				n_reject_bad=   'REJECT *BADS'
				accept_good_rate='ACCEPT *GOOD *RATE %'
				reject_good_rate='REJECT *GOOD *RATE %'
				accept_bad_rate='ACCEPT *BAD *RATE %'
				reject_bad_rate='REJECT *BAD *RATE %'
				accept_gb_odds='ACCEPT *GB *ODDS'
				reject_gb_odds='REJECT *GB *ODDS'
			%end;
			gb_total=     'GOOD/BAD * TOTAL'
			row_total=    ' ROW  *TOTAL * %'
			cumul_total=  'CUMUL  *TOTAL * %'
			n_good=       ' NO. *GOODS'
			dn_good=      ' ROW   *GOODS * %'
			n_bad=        ' NO. * BADS '
			dn_bad=       ' ROW  *BADS* %'
			gb_odds=      'GB * ODDS'
			bad_rate=     ' BAD *RATE * %'
			woe_gb=       'WOE * GB'
			_type_=       'TABLE '
		;
	run;
	* get the IV metrics;
	proc sql noprint;
		select
			%if &acceptvar^=0 and &rejectvar^=0 %then %do;
				max(iv_ar) format=6.4,
			%end;
			max(iv_gb) format=6.4
		into
			%if &acceptvar^=0 and &rejectvar^=0 %then %do;
				:iv_ar,
			%end;
			:iv_gb
		from
			_fine_quantiles
		;
	quit;
	* print to screen;
	%if %eval(&nbins>15) %then %do;
		options pagesize=%eval(&nbins+10) linesize=max nocenter;
	%end;
	%else %do;
		options pagesize=30 linesize=max nocenter;
	%end;
	proc print data=_fine noobs label split='*' style(header)={font_size=1};
		title1 "           Fine Classing Report on (&classvar)      ";
		format 
			n_good n_bad gb_total 
			%if &acceptvar^=0 and &rejectvar^=0 %then %do;
				n_accept n_reject ar_total 
			%end;
			8. 
			%if &acceptvar^=0 and &rejectvar^=0 %then %do;
				accept_rate 
				woe_ar 
			%end;
			dn_good dn_bad gb_odds bad_rate row_total cumul_total woe_gb 6.2
			%if &acceptgbs^=0 %then %do;
				accept_gb_odds accept_good_rate accept_bad_rate 
				reject_gb_odds reject_good_rate reject_bad_rate
				6.2
			%end;
			_type_ typs.
		;
		by _type_;
		var
			avg_&classvar 
			%if &acceptvar^=0 and &rejectvar^=0 %then %do;
				ar_total n_accept n_reject accept_rate 
			%end;
			%if &acceptgbs^=0 %then %do;
				n_accept_good n_accept_bad accept_good_rate accept_bad_rate accept_gb_odds
				n_reject_good n_reject_bad reject_good_rate reject_bad_rate reject_gb_odds
			%end;
			gb_total n_good n_bad 
			gb_odds bad_rate dn_good dn_bad row_total cumul_total woe_gb 
			%if &acceptvar^=0 and &rejectvar^=0 %then %do;
				woe_ar
			%end;
		;
		footnote1  "Good Bad I.V. = &iv_gb";
		%if &acceptvar^=0 and &rejectvar^=0 %then %do;
			footnote2  "Accept Reject I.V. = &iv_ar";
		%end;
	run;
	title;
	title1;
	footnote1;
	footnote2;
	* create the bar chart of the WOEs;
	pattern1 c='red';
	pattern2 c='blue';
	footnote1 justify=right " %sysfunc(date(),worddate.) at %sysfunc(time(),time5.)" ;
	title1 "Good / Bad Classing for &classvar:";
	title2;
	title3;
*goptions dev=html;
	proc gchart data=_fine;
		hbar avg_&classvar / sumvar=woe_gb missing nolegend discrete clipref frame subgroup=subgroup;
		label woe_gb='WOE';
	run;
	* if supplied, output the IV metrics to the IV table;
	%if &ivtable^=0 %then %do;
		data _fine005;
			length var $ 50 ;
			var="&classvar";
			iv_gb=&iv_gb;
			%if &acceptvar^=0 and &rejectvar^=0 %then %do;
				iv_ar=&iv_ar;
			%end;
		run;
		proc append data=_fine005 base=&ivtable;
		run;
	%end;
	* if SAS code is desired to show WOE splits, then generate, and ensure ;
	* that WOE has at least 4 significant digits (keep precision);
	%if &sascode=1 %then %do;
		* revert back to SAS-missing values here;
		%if &missingval=. %then %do;
			data _fine_quantiles;
				set _fine_quantiles;
				if min_&classvar=-9e99 and max_&classvar=-9e99 then do;
					avg_&classvar=. ;
					min_&classvar=. ;
					max_&classvar=. ;
				end;
			run;
		%end;
		* variable must be NUMERIC, need a PUT() function in SQL statement;
		proc sql noprint;
			select count(*) into :nfineqtl from _fine_quantiles;
		quit;
		%put DEBUG >> nfineqtl=&nfineqtl;
		proc sql;
			select
				case 
					when rec=1 then 
						"if &classvar<=" || put(max_&classvar,best8.) 
							|| " then &classvar.woe=" || put(woe_gb,7.4) || ";" 
					when rec=&nfineqtl then
						"else &classvar.woe=" || put(woe_gb,7.4) || ";"  
					else
						"else if &classvar<=" || put(max_&classvar,best8.) 
							|| " then &classvar.woe=" || put(woe_gb,7.4) || ";" 
				end
			from
				_fine_quantiles
			;
		quit;
	%end;
	* clean up;
	%if &cleanup=1 %then %do;
		%if &acceptgbs^=0 %then %do;
			proc delete data=_fine000; run;
		%end;
		proc delete data=_fine001; run;
		proc delete data=_fine001miss; run;
		proc delete data=_fine001nomiss; run;
		proc delete data=_fine002; run;
		proc delete data=_fine003; run;
		proc delete data=_fine004; run;
		proc delete data=_fine005; run;
		proc delete data=_fine_quantiles; run;
		proc delete data=_fine_total; run;
	%end;
	title1;
	title2;
	title3;
	footnote1;
	footnote2;
	quit;
%mend fine_class;





* ---------------------------------------------------------- ;
* x_class                                                    ;
* ---------------------------------------------------------- ;
* Performs fine classing on DISCRETE variabbles (group by    ;
* &classvar counting bad=1, good=1 occurences).  Determines  ;
* coefficients using Weight-of-Evidence (WOE).  THERE IS AN  ;
* OPPORTUNITY TO MODIFY THIS CODE SO THAT IT CAN TELL IF THE ;
* ACCEPT/REJECT COLUMNS WERE SUPPLIED BY TESTING IF          ;
* acceptvar=0, rejectvar=0                                   ;
* ---------------------------------------------------------- ;
* Arguments:                                                 ;
* ---------------------------------------------------------- ;
* intable    = the SAS data set to analyse                   ;
* classvar   = the variable to class (the x-var)             ;
* descvar    = OPTIONAL, a description of the bin for each   ;
*              class, e.g. 1-2, 3-5, 5-10 which you would    ;
*              rather see in the output instead of 1, 2, ... ;
* goodvar    = the name of the GOOD variable                 ;
* badvar     = the name of the BAD variable                  ;
* acceptvar  = the name of the ACCEPT variable               ;
* rejectvar  = the name of the REJECT variable               ;
* acceptgbs  = 1 if bad rate of accepts/rejects is desired   ;
* weight_g   = variable name of the GOOD-variable weight     ;
* weight_b   = variable name of the BAD-variable weight      ;
* weight_a   = variable name of the ACCEPT-variable weight   ;
* weight_r   = variable name of the REJECT-variable weight   ;
* ivtable    = OPTIONAL, the name of a table to APPEND the   ;
*              IV (Information Value) metrics to             ;
* cleanup    = 1 if you want to delete temp-tables created   ;
* sascode    = 1 if you want to generate SAS code to assign  ;
*              WOEs to a new variable [classvar.woe]         ;
* tol        = a small value to avoid boundary conditions    ;
*              where you have 100% good or 100% bad in a     ;
*              bin - make a small number, e.g. 0.5, 1.0      ;
* ---------------------------------------------------------- ;
* Example of Use:                                            ;
* ---------------------------------------------------------- ;
*%x_class (
	intable=dc_test,
	classvar=predx,
	cleanup=0
);
* ---------------------------------------------------------- ;
%macro x_class (
	intable=,
	classvar=,
	descvar=   0,
	goodvar=   good,
	badvar=    bad,
	acceptvar= 0,
	rejectvar= 0,
	acceptgbs= 0,
	weight_g=  1,
	weight_b=  1,
	weight_a=  1,
	weight_r=  1,
	ivtable=   0,
	cleanup=   1,
	sascode=   0,
	tol=       0.001
);
	* these are local macro variables, so declare them so I  ;
	* do not accidentally overwrite a global variable here   ;
	%local nobs;
	%local iv_ar;
	%local iv_gb;
	* if &acceptvar^=0, need a temp table to calculate bads accepted, etc ;
	%if &acceptvar^=0 and &rejectvar^=0 %then %do;
		data _fine000;
			set &intable;
			%if &acceptgbs^=0 %then %do;
				_acceptgood=accept*good;
				_acceptbad= accept*bad;
				_rejectgood=reject*good;
				_rejectbad= reject*bad;
			%end;
			keep 
				&classvar &goodvar &badvar &acceptvar &rejectvar
				%if &acceptgbs^=0 %then %do;
					_acceptgood _acceptbad
					_rejectgood _rejectbad
				%end;
			;
		run;
		%let mytable=_fine000;
	%end;
	%else %do;
		%let mytable=&intable;
	%end;
	* for each x-var outcome, accumulate the good/bads ;
	proc summary data=&mytable missing;
		class &classvar;
		%if &descvar^=0 %then %do;
			id &descvar;
		%end;
		var &goodvar &badvar 
			%if &acceptvar^=0 and &rejectvar^=0 %then %do;
				&acceptvar &rejectvar
				%if &acceptgbs^=0 %then %do;
					_acceptgood _acceptbad
					_rejectgood _rejectbad
				%end;
			%end;
		;
		output out= _fine001(where=(_type_=1)) sum=;
	run;
	* weight the data ;
	data _fine001;
		set _fine001;
		* weight good / bad / accept / reject outcomes ;
		* using the appropriate weight-variables       ;
		%if &weight_g^=1 %then %do;
			&goodvar=&goodvar*&weight_g;
			%if &acceptgbs^=0 %then %do;
				_acceptgood=_acceptgood*&weight_g;
				_rejectgood=_rejectgood*&weight_g;
			%end;
		%end;
		%if &weight_b^=1 %then %do;
			&badvar=&badvar*&weight_b;
			%if &acceptgbs^=0 %then %do;
				_acceptbad=_acceptbad*&weight_g;
				_acceptbad=_acceptbad*&weight_g;
			%end;
		%end;
		%if &weight_a^=1 and &acceptvar^=0 and &rejectvar^=0 %then %do;
			&acceptvar=&acceptvar*&weight_a;
			%if &acceptgbs^=0 %then %do;
				_acceptbad= _acceptbad*&weight_g;
				_acceptgood=_acceptgood*&weight_g;
			%end;
		%end;
		%if &weight_r^=1 and &acceptvar^=0 and &rejectvar^=0 %then %do;
			&rejectvar=&rejectvar*&weight_r;
			%if &acceptgbs^=0 %then %do;
				_rejectbad= _rejectbad*&weight_g;
				_rejectgood=_rejectgood*&weight_g;
			%end;
		%end;
*******	* change label of _quantile ;
*******	label _quantile='_quantile';
	run;
	* aggregate by outcome ;
	proc sql;
		create table _fine002 as
		select
			%if &descvar^=0 %then %do;
			&descvar,
			%end;
			&classvar,
			%if &acceptvar^=0 and &rejectvar^=0 %then %do;
				sum(&acceptvar)   as n_accept,
				sum(&rejectvar)   as n_reject,
				%if &acceptgbs^=0 %then %do;
					sum(_acceptgood)   as n_accept_good,
					sum(_rejectgood)   as n_reject_good,
					sum(_acceptbad)    as n_accept_bad,
					sum(_rejectbad)    as n_reject_bad,
				%end;
			%end;
			sum(&goodvar)     as n_good,
			sum(&badvar)      as n_bad
		from
			_fine001
		group by
			%if &descvar^=0 %then %do;
			&descvar,
			%end;
			&classvar
		;
	quit;
	proc sort data= _fine002; by &classvar; run;
	* do this renaming here, unnecessary anywhere before here;
	%if &descvar=0 %then %do;
		%let descvar=&classvar;
	%end;
	* get the number of outcomes ;
	proc sql noprint;
		select
			sum(n_good+n_bad),
			%if &acceptvar^=0 and &rejectvar^=0 %then %do;
				sum(n_accept),
				sum(n_reject),
			%end;
			%if &acceptgbs^=0 %then %do;
				sum(n_accept_good),
				sum(n_accept_bad),
				sum(n_reject_good),
				sum(n_reject_bad),
			%end;
			sum(n_good),
			sum(n_bad)
		into
			:nobs,
			%if &acceptvar^=0 and &rejectvar^=0 %then %do;
				:naccepts,
				:nrejects,
			%end;
			%if &acceptgbs^=0 %then %do;
				:nacceptgoods,
				:nacceptbads,
				:nrejectgoods,
				:nrejectbads,
			%end;
			:ngoods,
			:nbads
		from 
			_fine002
		;
	quit;
	%if &acceptvar^=0 and &rejectvar^=0 %then %do;
		%put DEBUG >> nobs=&nobs, ngoods=&ngoods, nbads=&nbads, naccepts=&naccepts, nrejects=&nrejects;
	%end;
	%else %do;
		%put DEBUG >> nobs=&nobs, ngoods=&ngoods, nbads=&nbads;
	%end;
	%if &acceptgbs^=0 %then %do;
		%put DEBUG >> nacceptgoods=&nacceptgoods, nacceptbads=&nacceptbads, nrejectgoods=&nrejectgoods, nacceptbads=&nacceptbads;
	%end;
	* calculate further metrics by quantile;
	data _fine_quantiles;
		set _fine002;
		retain cumul_total iv_ar iv_gb 0.0;
		%if &acceptvar^=0 and &rejectvar^=0 %then %do;
			ar_total=n_accept+n_reject;
			* dn_accept, dn_reject add up to 100 over all rows;
			dn_accept=n_accept/&naccepts*100.0;
			dn_reject=n_reject/&nrejects*100.0;
			* the odds, a precursor to WOE;
			ar_odds=dn_accept/dn_reject;
			accept_rate=n_accept/ar_total*100.0;
			%if &acceptgbs^=0 %then %do;
				accept_good_rate=n_accept_good/n_accept*100.0;
				accept_bad_rate=n_accept_bad/n_accept*100.0;
				reject_good_rate=n_reject_good/n_reject*100.0;
				reject_bad_rate=n_reject_bad/n_reject*100.0;
			%end;
		%end;
		gb_total=n_good+n_bad;
		* dn_good, dn_bad add up to 100 over all rows;
		dn_good=n_good/&ngoods*100.0;
		dn_bad=n_bad/&nbads*100.0;
		* the odds, a precursor to WOE;
		gb_odds=dn_good/dn_bad;
		bad_rate=n_bad/gb_total*100.0;
		row_total=gb_total/&nobs*100.0;
		cumul_total+row_total;
		%if &acceptgbs^=0 %then %do;
			dn_accept_good=n_accept_good/&nacceptgoods*100.0;
			dn_accept_bad =n_accept_bad /&nacceptbads*100.0;
			dn_reject_good=n_reject_good/&nrejectgoods*100.0;
			dn_reject_bad =n_reject_bad /&nrejectbads*100.0;
			accept_gb_odds=dn_accept_good/dn_accept_bad;
			reject_gb_odds=dn_reject_good/dn_reject_bad;
		%end;
		* woe, IV (ar), see Naeem Siddiqi p.81, ADDING logic that if   ;
		* there are 100% accepts or 100% rejects in a bin then instead ;
		* of having an undefined WOE, add &tol to dn_accept or         ;
		* dn_reject so that there is no division by zero issue when    ;
		* calculating the WOE ;
		%if &acceptvar^=0 and &rejectvar^=0 %then %do;
			if ar_odds>0 then do;
				* non-boundary condition, at least 1 accept and 1 reject;
				woe_ar=log(ar_odds);
			end;
			else if ar_odds=0 then do;
				* boundary condition, no accepts;
				woe_ar=log(&tol/dn_reject);
			end;
			else do;
				* boundary condition, no rejects;
				woe_ar=log(dn_accept/&tol);
			end;
			iv_ar+(dn_accept-dn_reject)/100.0*woe_ar;
		%end;
		* woe, IV (gb), see Naeem Siddiqi p.81, ADDING logic that if   ;
		* there are 100% goods or 100% bads in a bin then instead of   ;
		* having an undefined WOE, add &tol to dn_bad or dn_good so    ;
		* that there is no division by zero issue when calculating the ;
		* WOE ;
		if gb_odds>0 then do;
			* non-boundary condition, at least 1 good and 1 bad;
			woe_gb=log(gb_odds);
		end;
		else if gb_odds=0 then do;
			* boundary condition, no goods;
			woe_gb=log(&tol/dn_bad);
		end;
		else do;
			* boundary condition, no bads;
			woe_gb=log(dn_good/&tol);
		end;
		iv_gb+(dn_good-dn_bad)/100.0*woe_gb;
	run;
	* sum the columns ;
	proc sql;
		create table _fine_total as 
		select
			%if &acceptvar^=0 and &rejectvar^=0 %then %do;
				sum(ar_total) as ar_total,
				sum(n_reject) as n_reject,
				sum(n_accept) as n_accept,
				&naccepts/&nobs*100.0 as accept_rate,
				1.00 as ar_odds,
			%end;
			%if &acceptgbs^=0 %then %do;
				sum(n_accept_good)            as n_accept_good,
				sum(n_accept_bad )            as n_accept_bad,
				&nacceptgoods/&naccepts*100.0 as accept_good_rate,
				&nacceptbads/&naccepts*100.0  as accept_bad_rate,
				&nacceptgoods/&nacceptbads    as accept_gb_odds,
				sum(n_reject_good)            as n_reject_good,
				sum(n_reject_bad )            as n_reject_bad,
				&nrejectgoods/&nrejects*100.0 as reject_good_rate,
				&nrejectbads/&nrejects*100.0  as reject_bad_rate,
				&nrejectgoods/&nrejectbads    as reject_gb_odds,
			%end;
			sum(gb_total) as gb_total,
			sum(n_good) as n_good,
			sum(n_bad) as n_bad,
			sum(n_good)/sum(n_bad) as gb_odds,
			sum(n_bad)/&nobs*100.0 as bad_rate,
			100.0 as dn_bad,
			100.0 as dn_good,
			100.0 as row_total,
			max(cumul_total) as cumul_total,
			0.0 as woe_gb
			%if &acceptvar^=0 and &rejectvar^=0 %then %do;
				,0.0 as woe_ar
			%end;
		from
			_fine_quantiles
		;
	quit;
	* some formats;
	proc format;
		value typs
			0='Total Sample'
			1='Marginal Classings'
		;
	run;
	* group quantile, total tables together;
	data _fine;
		set 
			_fine_quantiles
			_fine_total(in=t)
		;
		_type_=t;
		* create a subgroup variable, I will use to make negative  ;
		* woe values have red bars in the graphs, positive values  ;
		* have blue bars ;
		if woe_gb<0.0 then subgroup=1; else subgroup=2;
		label
			%if &acceptvar^=0 and &rejectvar^=0 %then %do;
				ar_total=     'APPS * TOTAL'
				woe_ar=       'WOE * AR'
				n_accept=     'NO. * ACCEPTS'
				n_reject=     'NO. * REJECTS'
				accept_rate=  'ACCEPT * RATE * %'
				ar_odds=      'AR * ODDS'
			%end;
			%if &acceptgbs^=0 %then %do;
				n_accept_good=  'ACCEPT *GOODS'
				n_accept_bad=   'ACCEPT *BADS'
				n_reject_good=  'REJECT *GOODS'
				n_reject_bad=   'REJECT *BADS'
				accept_good_rate='ACCEPT *GOOD *RATE %'
				reject_good_rate='REJECT *GOOD *RATE %'
				accept_bad_rate='ACCEPT *BAD *RATE %'
				reject_bad_rate='REJECT *BAD *RATE %'
				accept_gb_odds='ACCEPT *GB *ODDS'
				reject_gb_odds='REJECT *GB *ODDS'
			%end;
			gb_total=     'GOOD/BAD * TOTAL'
			row_total=    ' ROW  *TOTAL * %'
			cumul_total=  'CUMUL  *TOTAL * %'
			n_good=       ' NO. *GOODS'
			dn_good=      ' ROW   *GOODS * %'
			n_bad=        ' NO. * BADS '
			dn_bad=       ' ROW  *BADS* %'
			gb_odds=      'GB * ODDS'
			bad_rate=     ' BAD *RATE * %'
			woe_gb=       'WOE * GB'
			_type_=       'TABLE '
			&descvar=     "&classvar"
		;
	run;
	* get the IV metrics;
	proc sql noprint;
		select
			%if &acceptvar^=0 and &rejectvar^=0 %then %do;
				max(iv_ar) format=6.4,
			%end;
			max(iv_gb) format=6.4
		into
			%if &acceptvar^=0 and &rejectvar^=0 %then %do;
				:iv_ar,
			%end;
			:iv_gb
		from
			_fine_quantiles
		;
	quit;
	* print to screen;
	proc sql noprint;
		select count(*) into :nbins
		from _fine;
	quit;
	%put DEBUG >> nbins=&nbins;
	%if %eval(&nbins>15) %then %do;
		options pagesize=%eval(&nbins+10) linesize=max nocenter;
	%end;
	%else %do;
		options pagesize=30 linesize=max nocenter;
	%end;
	proc print data=_fine noobs label split='*' style(header)={font_size=1};
		title1 "           Fine Classing Report on (&classvar)      ";
		format 
			n_good n_bad gb_total 
			%if &acceptvar^=0 and &rejectvar^=0 %then %do;
				n_accept n_reject ar_total 
			%end;
			8. 
			dn_good dn_bad gb_odds bad_rate row_total cumul_total woe_gb 
			%if &acceptgbs^=0 %then %do;
				accept_gb_odds accept_good_rate accept_bad_rate 
				reject_gb_odds reject_good_rate reject_bad_rate
				6.2
			%end;
			%if &acceptvar^=0 and &rejectvar^=0 %then %do;
				accept_rate woe_ar 
			%end;
			6.2
			_type_ typs.
		;
		by _type_;
		var 
			&descvar 
			%if &acceptvar^=0 and &rejectvar^=0 %then %do;
				ar_total n_reject n_accept accept_rate 
			%end;
			%if &acceptgbs^=0 %then %do;
				n_accept_good n_accept_bad accept_good_rate accept_bad_rate accept_gb_odds
				n_reject_good n_reject_bad reject_good_rate reject_bad_rate reject_gb_odds
			%end;
			gb_total n_good n_bad 
			gb_odds bad_rate dn_good dn_bad row_total cumul_total woe_gb 
			%if &acceptvar^=0 and &rejectvar^=0 %then %do;
				woe_ar
			%end;
		;
		footnote1  "Good Bad I.V. = &iv_gb";
		%if &acceptvar^=0 and &rejectvar^=0 %then %do;
			footnote2  "Accept Reject I.V. = &iv_ar";
		%end;
	run;
	title;
	title1;
	footnote1;
	footnote2;
	* create the bar chart of the WOEs;
	pattern1 c='red';
	pattern2 c='blue';
	* create the bar chart of the WOEs;
	footnote1 justify=right " %sysfunc(date(),worddate.) at %sysfunc(time(),time5.)" ;
	title1 "Good / Bad Classing for &classvar:";
	title2;
	title3;
*goptions dev=html;
	proc gchart data=_fine;
		hbar &descvar / sumvar=woe_gb missing nolegend discrete clipref frame subgroup=subgroup;
		label woe_gb="WOE";
		footnote1  "Good Bad I.V. = &iv_gb";
		%if &acceptvar^=0 and &rejectvar^=0 %then %do;
			footnote2  "Accept Reject I.V. = &iv_ar";
		%end;
	run;
	* if supplied, output the IV metrics to the IV table;
	%if &ivtable^=0 %then %do;
		data _fine003;
			length var $ 50 ;
			var="&classvar";
			iv_gb=&iv_gb;
			%if &acceptvar^=0 and &rejectvar^=0 %then %do;
				iv_ar=&iv_ar;
			%end;
		run;
		proc append data=_fine003 base=&ivtable;
		run;
	%end;
	* if SAS code is desired to show WOE splits, then generate, and ensure ;
	* that WOE has at least 4 significant digits (keep precision);
	%if &sascode=1 %then %do;
		* check if &classvar is numeric or string;
		proc contents data=_fine_quantiles out=_fine005 noprint; run;
		proc sql noprint;
			select TYPE into :type from _fine005 where upcase(NAME)=upcase("&classvar");
		quit;
		%put DEBUG >> type=&type;
		%if &type=1 %then %do;
			* variable is NUMERIC, need a PUT() function in SQL statement;
			proc sql;
				select
					"if &classvar=" || put(&classvar,best8.) || " then &classvar.woe=" || put(woe_gb,7.4) || ";"
				from
					_fine_quantiles
				;
			quit;
		%end;
		%else %do;
			* variable is STRING, do not need a PUT() function in SQL statement;
			proc sql;
				select
					"if &classvar=%str(%')" || &classvar || "%str(%') then &classvar.woe=" || put(woe_gb,7.4) || ";"
				from
					_fine_quantiles
				;
			quit;
		%end;
	%end;
	* clean up;
	%if &cleanup=1 %then %do;
		%if &acceptgbs^=0 %then %do;
			proc delete data=_fine000; run;
		%end;
		proc delete data=_fine001; run;
		proc delete data=_fine002; run;
		%if &ivtable^=0 %then %do;
			proc delete data=_fine003; run;
		%end;
		proc delete data=_fine_quantiles; run;
		proc delete data=_fine_total; run;
		proc delete data=_fine005; run;
	%end;
	title1;
	title2;
	title3;
	footnote1;
	footnote2;
	quit;
%mend x_class;

