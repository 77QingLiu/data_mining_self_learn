libname sample xlsx "FRAUD_TRAIN_SAMP1.xlsx";
data FRAUD_TRAIN_SAMP1;
    set sample.sheet1;
run;

libname sample xlsx "FRAUD_TRAIN_SAMP2.xlsx";
data FRAUD_TRAIN_SAMP2;
    set sample.sheet1;
run;

libname sample xlsx "FRAUD_VALDT_SAMP1.xlsx";
data FRAUD_VALDT_SAMP1;
    set sample.sheet1;
run;


*以下模型训练;
%macro rftrain(indat,p);
    proc contents noprint data=&indat. out=train_data_name;
    run;

    data train_data_name;
        set train_data_name;
        where type=1 and upcase(compress(name)) not in ( 'CSR_ID','TARGET')  ;
        keep name;
    run;

    %do i=1 %to &p.;

    *每次随机筛选50个变量;
    proc surveyselect data=train_data_name out=var_01 sampsize=50;quit;
    proc sql ;
        select distinct name into:var separated by ' '    from var_01;
    quit;

    *每次随机筛选20%的观测;
    data step01;
        set &indat.;
        x=ranuni(0);
        if x<=0.2;
    run;

    *构建决策树, criterion=entropy指定信息熵作为决策树分割的依据，可以选择信息增益、Gini系数等其他指标;
    Proc split data=step01 outleaf=leaf 
        outimportance =importance outtree=tree outmatrix=matrix outseq=seq 
        criterion     =entropy
        assess        =impurity 
        maxbranch     =3
        maxdepth      =5
        exhaustive    =100
        leafsize      =30
        splitsize     =30
        subtree       =assessment;
        code file     ="split\sas_rule&i..txt";
        describe file ="split\rulefinal&i..txt";
        input  &var./ level=interval;
        target target /level=binary;
        run; 
    %end;
%mend;

* fraud_train_samp是建模数据，训练1000棵决策树;
%rftrain(fraud_train_samp1,1);

* 代码清单2-2 模型打分代码;
%macro rfscore(indat,p,outdat);
    %do i=1 %to &p.;
        data score_&i.;
            set &indat.;
            %include "split\sas_rule&i..txt";
            p_&i.=p_target1;
            keep csr_id p_&i.;
        run;

        proc sort data=score_&i.;
            by csr_id;
        run;
    %end;

    proc sort data=&indat.(keep=csr_id target) out=tmp1;
        by csr_id;
    run;

    data &outdat.;
        merge tmp1 %do i=1 %to &p.;score_&i. %end;;
        by csr_id;
        pr=sum(of p_1-p_&p.)/&p.;
        keep csr_id target pr;
    run;
%mend;

*对验证数据打分，结果输出到表score_valdt;
%rfscore(fraud_valdt_samp1,1,score_valdt);


* 代码清单2-3 模型评估代码;
%macro Fit(in,out,grp_cnt,pred_var,act_var);        
    data work.tt1;        
        set &in;        
    run;        

    data _null_;        
        set work.tt1 nobs=obs ;        
        call symput("Base",obs/&grp_cnt);        
        stop;        
    run;        

    proc sort data=work.tt1;        
        by  descending &pred_var;        
    run;        

    data work.tt1;        
        N=_N_;        
        set work.tt1;        
        format Grp2 4.0;        
        Grp2=INT((N-1)/&base);        
    run;        

    proc means data=work.tt1 nway noprint;        
        class  Grp2 ;        
        output out=&out mean(&pred_var &act_var)=pred_evt actual_evt;        
    run;        

%mend;    

*fit_valdt就是表2-5前五列;
%Fit(score_valdt,fit_valdt,20,pr,target);
