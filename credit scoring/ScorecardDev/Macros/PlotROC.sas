/*******
  Credit Risk Scorecards: Development and Implementation using SAS
  (c)  Mamdouh Refaat
********/


/*******************************************************/
/* Macro PlotROC  */
/*******************************************************/
%macro PlotROC(DSROC);
goptions reset=global gunit=pct border cback=white
         colors=(black blue green red)
         ftitle=swissb ftext=swiss htitle=6 htext=4;


symbol1 color=red
        interpol=join
;
 
  proc gplot data=&DSROC;
   plot Sensitivity*Specificity1 / haxis=0 to 1 by 0.1
                    vaxis=0 to 1 by 0.1
                    hminor=3
                    vminor=1
 
                      vref=0.2 0.4 0.6 0.8 1.0
                    lvref=2
                    cvref=blue
                    caxis=blue
                    ctext=red;
run;
quit;
 
	goptions reset=all;
%mend;
