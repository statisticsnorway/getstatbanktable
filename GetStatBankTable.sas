%macro convert(varname);
 &varname = kcvt(&varname,'utf-8',"&encoding.");
* &varname = tranwrd(&varname,'Ã€','ä');
%mend;

%macro GetStatbankTable(
 table=03013,
 time=5,
 max_cells=500000,
 language=no,
 outlib=work,
 out=statbank,
 metaoutlib=work,
 metaout=meta,
 delete_zeros=0,
 delete_missing=0,
 transpose=1,
 formatvars=1,
 fmtlib=work,
 metaonly=0,
 delete_temp=1,
 sleep=60,
 url=https://data.ssb.no/api/v0,
 encoding=&sysencoding.
 );
/* Sas macro that extracts a whole table from the Statbank (https://www.ssb.no/statbank) based on table number

Written by Kristian Lønø, Statistics Norway, June 2018

Parameters:
 table          Table name in the Statistical bank 
 time           Number of the last periods to extract, All gives all periods
 max_cells      Maximum number of cells that can be extracted in one query. If more than this, the query is divided into several queries. Usually set by the Statbank
 language       Language code. no=Norwegian, en=English
 outlib         Sas library for output table
 out            Sas dataset name for output table
 metaoutlib     Sas library for meta data output
 metaout        Sas dataset name for meta data output
 delete_zeros   Delete rows where all values are 0?
                  1 = Yes
                  0 = No
 delete_missing Delete rows where all values are missing (.)?
                  1 = Yes
                  0 = No
 transpose      Transposed output table. 
                  0 = no transpose (one column for each combination of ContenstsCode and Time)
                  1 = transpose Contents code and time (ContenstsCode and Time in separate variables)
                  2 = transpose ContentsCode (One variable for ContentsCode and one column for each Time)
 formatvars     Format the classification variables?
                  1 = Yes. Formats are created and the codes are used in the output table
                  0 = No. Use the texts as they are in the columns
 fmtlib         Sas libname for formats
 metaonly       Choose if only the metadata is to be extracted
                  1 Extract only metadata
 delete_temp    Delete datasets used temporary during execution?
                  1 = Yes, except the metadata dataset Meta
                  2 = Yes, all 
                  0 = No, keep all. Useful for debugging
 sleep          Wait time in seconds for execution after 30 rounds. We are not allowed to extract more than 30 tables a minute from the Statbank
 url            Url to the Statbank
 encoding       Encoding
*/

%local table_exist no_of_periods first_period last_period max_period selected_periods variables_passed_limit rounds no_of_cells;

%let language = %lowcase(&language);
data _null_;
 if "&language" not in("no","en","sv","fi") then
  call symputx('language','no');
run;

* Language text catalogue;
proc sql;
create table language_texts
(text_name     char(32)  label='Name of text entry',
 text_language char(2) label='Language code',
 text          char(200) label='Text'
);            
insert into language_texts
values('dupl_title','no',"Samme tekst brukt flere ganger for tabell &table.") 
values('dupl_label_text','no','Kode') 
values('dupl_code_text','no','Tekst') 
values('timevar','no','Tid') 
values('valuevar','no','Verdi') 
values('contentscodevar','no','Statistikkvariabel') 
values('contentscodelabel','no','Statistikkvariabel') 
values('warn_table_not_numeric','no',"WARNING: Tabellnummer &table. er ikke et tall. Velg et tabellnummer med kun tall og prøv igjen.")
values('warn_table_exist','no',"WARNING: Tabell &table. finnes ikke. Velg et annet tabellnummer og prøv igjen.")
values('note_periods','no',%nrstr("NOTE: Antall perioder i tabell &table. er &no_of_periods, første periode er &first_period og siste periode er &last_period.."))
values('note_sel_periods','no',%nrstr("NOTE: Valgte perioder er ""&selected_periods.""."))
values('note_no_of_passed','no',%nrstr(,"NOTE: Antall variable passert maksimum grense for celler er &variables_passed_limit."))
values('note_no_of_rounds','no',%nrstr(,"NOTE: Antall runder nødvendig for å unngå å overskride maksimalt antall celler er &rounds."))
values('note_no_of_cells','no',%nrstr(,"NOTE: Antall celler totalt i uttrekket blir &no_of_cells."))
values('dupl_title','en',"Duplicate texts used for table &table.") 
values('dupl_label_text','en','Code') 
values('dupl_code_text','en','Text') 
values('timevar','en','Time') 
values('valuevar','en','Value') 
values('contentscodevar','en','ContentsCode') 
values('contentscodelabel','en','Contents') 
values('warn_table_not_numeric',"en","WARNING: Table number &table. is not numeric. Choose a numeric value and try again.")
values('warn_table_exist',"en","WARNING: Table &table. does not exist. Choose another table number and try again.")
values('note_periods',"en",%nrstr("NOTE: Number of periods in table &table. is &no_of_periods, first period is &first_period and last period is &last_period.."))
values('note_sel_periods',"en",%nrstr("NOTE: Selected periods are ""&selected_periods.""."))
values('note_no_of_passed',"en",%nrstr("NOTE: No. of variables passed maximum limit of cells is &variables_passed_limit."))
values('note_no_of_rounds',"en",%nrstr("NOTE: No of rounds needed to not exceed maximum cells is &rounds."))
values('note_no_of_cells',"en",%nrstr("NOTE: Total number of cells to extract is &no_of_cells."))
values('dupl_title','sv',"Duplicate texts used for table &table.") 
values('dupl_label_text','sv','Code') 
values('dupl_code_text','sv','Text') 
values('timevar','sv','Tid') 
values('valuevar','sv','Värde') 
values('contentscodevar','sv','ContentsCode') 
values('contentscodelabel','sv','Contents') 
values('warn_table_not_numeric',"sv","WARNING: Table number &table. is not numeric. Choose a numeric value and try again.")
values('warn_table_exist',"sv","WARNING: Table &table. does not exist. Choose another table number and try again.")
values('note_periods',"sv",%nrstr("NOTE: Number of periods in table &table. is &no_of_periods, first period is &first_period and last period is &last_period.."))
values('note_sel_periods',"sv",%nrstr("NOTE: Selected periods are ""&selected_periods.""."))
values('note_no_of_passed',"sv",%nrstr("NOTE: No. of variables passed maximum limit of cells is &variables_passed_limit."))
values('note_no_of_rounds',"sv",%nrstr("NOTE: No of rounds needed to not exceed maximum cells is &rounds."))
values('note_no_of_cells',"fi",%nrstr("NOTE: Total number of cells to extract is &no_of_cells."))
values('dupl_title','fi',"Duplicate texts used for table &table.") 
values('dupl_label_text','fi','Code') 
values('dupl_code_text','fi','Text') 
values('timevar','fi','Vuosi') 
values('valuevar','fi','Arvo') 
values('contentscodevar','fi','Tiedot') 
values('contentscodelabel','fi','Tiedot') 
values('warn_table_not_numeric',"fi","WARNING: Table number &table. is not numeric. Choose a numeric value and try again.")
values('warn_table_exist',"fi","WARNING: Table &table. does not exist. Choose another table number and try again.")
values('note_periods',"fi",%nrstr("NOTE: Number of periods in table &table. is &no_of_periods, first period is &first_period and last period is &last_period.."))
values('note_sel_periods',"fi",%nrstr("NOTE: Selected periods are ""&selected_periods.""."))
values('note_no_of_passed',"fi",%nrstr("NOTE: No. of variables passed maximum limit of cells is &variables_passed_limit."))
values('note_no_of_rounds',"fi",%nrstr("NOTE: No of rounds needed to not exceed maximum cells is &rounds."))
values('note_no_of_cells',"fi",%nrstr("NOTE: Total number of cells to extract is &no_of_cells."))
;
quit;

data _null_;
 set language_texts;
 where upcase(text_language) = upcase("&language");
 call symputx(text_name,strip(text),'L');
run;

* Find which country the extract is made for by search within the url;
data _null_;
 select ;
  when (find(upcase("&url."),'SSB.NO') > 0) 
   do;
    call symputx("url_language","NO",'L');
    call symputx("url_complete","&url./&language./table/&table.",'L');
   end;
  when (find(upcase("&url."),'SCB.SE') > 0) 
   do;
    call symputx("url_language","SV",'L');
    call symputx("url_complete","&url./&language./ssd/START/&table.",'L');;
   end;
  when (find(upcase("&url."),'STAT.FI') > 0) 
   do;
    call symputx("url_language","FI",'L');
    call symputx("url_complete","&url./&language./StatFin/&table.",'L');;
   end;
  otherwise 
   do;
    call symputx("url_language","XX",'L');
    call symputx("url_complete","&url./&language./table/&table.",'L');
   end;
 end;
run;
%put &=url_language &=url_complete;

* Check if table no is numeric (only for Norwegian);
data _null_;
 table = "%bquote(&table)";
 if notdigit(table) = 0 or upcase("&url_language") NE "NO" then
  call symputx('num_table',1,'L');
 else
  call symputx('num_table',0,'L');
run;

%if "&num_table" = "1" %then
 %do;  /* Start table number is numeric or not from Norwegian Statbank*/

* If all periods are chosen, the time parameter is set to zero to avoid evaluation problems;
* If the time parameter is invalid it is set to 5 last periods;
data _null_;
 if upcase("&time.") = "ALL" or upcase("&time.") = "ALLE" then
  call symputx('time','0','L');
run;
data _null_;
 time = "&time";
 if time + 0 < 0 then 
  call symputx('time','5','L');
run;

* Import the metadata for the table;
filename sbmeta url "&url_complete." encoding="&encoding.";

* Check if the table exists;
%let table_exist=0;
data _null_;
 if fexist("sbmeta") then
  call symputx("table_exist",1);
run;

%if "&table_exist" = "1" %then
 %do; /* Start table exists */

libname sbmeta json fileref=sbmeta ;

proc sql;
   create table metadata as
   select t1.*,
          t2.*,
          t3.*,
          t4.*
      from sbmeta.root as t1 inner join sbmeta.variables as t2
       on (t1.ordinal_root = t2.ordinal_root) inner join sbmeta.variables_values as t3
       on (t2.ordinal_variables = t3.ordinal_variables)inner join sbmeta.variables_valuetexts as t4
       on (t2.ordinal_variables = t4.ordinal_variables)
      order by ordinal_root, ordinal_variables, ordinal_values;
quit;

* If no class variables then elimination variable has to be added;
data metadata;
 set metadata;
 if elimination = . then
  elimination = .;
run;

proc transpose data=metadata out=meta1 (rename=(col1=value)) ;
 by ordinal_variables title code text elimination time;
 var values:;
run;

proc transpose data=metadata out=meta2 (rename=(col1=valuetext) drop=_name_) ;
 by ordinal_variables ;
 var valuetext:;
run;

data meta_t;
 merge meta1 meta2;
 if value ne '';
 code_no = compress(_name_,,'DK')+0;
 %convert(code);
 %convert(text);
 %convert(valuetext);
 %convert(title);
* code = translate(code,'OAoa','ÖÄöä');
 fmtname = translate(cats("$",code,"_&language."),'OAoa','ÖÄöä');
 * The finnish contents has finnish name also for the english version;
 if upcase(code) = 'TIEDOT' then
  fmtname = "$Contentscode_&language.";
 start = value;
 label = valuetext;
 hlo = 'S ';
 drop _name_;
run;

proc sort data=meta_t;
 by time ordinal_variables;
run;

proc sql noprint;
 select count(*), min(value), max(value), max(code_no) into :no_of_periods trimmed, :first_period trimmed, :last_period trimmed, :max_period trimmed
 from meta_t
 where time = 1
 ;
quit;

%put &note_periods;

* Choose selected time periods;
%if "&time." ne "0" %then
 %do;
  data meta_t;
   set meta_t;
   where not(code_no <= &max_period. - &time. and time = 1);
 %end;

* Selected periods;
proc sql noprint;
 select value into :selected_periods separated by '"",""'
 from meta_t
 where time = 1
 order by value
 ;
quit;
%put &note_sel_periods.;

* Number of cells;
proc sql ;
 create table no_of_cells as
 select ordinal_variables, code, count(*) as no_of_values
 from meta_t
 group by ordinal_variables, code
 order by ordinal_variables
 ;
quit;

data no_of_cells;
 set no_of_cells;
 retain no_of_cells ;
 if _n_ = 1 then 
  no_of_cells = no_of_values;
 else
  no_of_cells = no_of_values * no_of_cells ;
 if no_of_cells >= &max_cells then
  do;
   passed_max = 1;
   passed_no +1;
  end;
run;

proc sql ;
 create table no_of_cells as
 select *, sum(passed_max) as variables_passed_limit
 from no_of_cells
 order by ordinal_variables
 ;
quit;

* Put total number of cells to extract into a macro variable for later use in a note to the log;
proc sql noprint;
 select max(no_of_cells) format=15. into :no_of_cells trimmed 
 from no_of_cells
 ;
quit;

proc sql;
   create table &metaoutlib..&metaout. as
   select t1.*,
          t2.passed_max, t2.passed_no, t2.variables_passed_limit
      from meta_t as t1 inner join no_of_cells as t2
       on (t1.ordinal_variables = t2.ordinal_variables)
      order by t1.time, t1.ordinal_variables, code, code_no;
quit;

proc format cntlin=&metaoutlib..&metaout. (where=(time ne 1))  lib=&fmtlib.;
run;

* Put title of table into macro variable for usage as dataset label;
data _null_;
 set &metaoutlib..&metaout. (obs=1);
 call symputx('TableLabel',title);
run;
 
* Create formats for variables;
%if "&formatvars." = "1" %then
 %do;
  data formats_recode;
   length start $999;
   set &metaoutlib..&metaout. ;
   where time ne 1 and upcase(code) not in ('CONTENTSCODE','TIEDOT');
   fmtname = cats(fmtname,'R');
   hlo = '';
   lab_h = label;
   label = start;
   start = catx(' ',start,upcase(lab_h));
   output;
   start = catx(' ',upcase(lab_h));
   output;
   keep fmtname start label hlo ;
  run;

  proc sort data=formats_recode;
   by fmtname start label;
  run;

  data formats_recode format_duplicates;
   set formats_recode ;
   by fmtname start;
   if first.start and last.start then
    output formats_recode;
   else
    if hlo ne 'O' then
     do;
      output format_duplicates;
      call symputx('format_duplicates','1','L');
     end;
   if last.fmtname then
    do;
     start = '';
     label = 'ZZZZZZZZ';
     hlo = 'O';
     output formats_recode;
    end;
  run;

  proc format cntlin=formats_recode;
  run;

  %if "&format_duplicates" = "1" %then
   %do;
    proc sql ;
     title "&dupl_title. %bquote(&TableLabel.).";
     select label label="&dupl_label_text.", start label="&dupl_code_text."
     from format_duplicates
     ;
    quit;
   %end;
 %end;

* Find out if we have to do more than 1 extract from Statbank (necessary if the limit of cells is exceeded);
%let variables_passed_limit=0;
%let rounds=0;

proc sql noprint;
 select max(variables_passed_limit) into :variables_passed_limit trimmed 
 from no_of_cells
 where variables_passed_limit > 0
 ;
quit;
%if &variables_passed_limit <1 %then
 %do;
  %let variables_passed_limit=0;
 %end;
%put &note_no_of_passed.;
%put &note_no_of_cells.;

%if &variables_passed_limit. > 0 %then
 %do;
  %do i = 1 %to &variables_passed_limit.;
   proc sql;
    create table passed_&i. as
    select code as code_&i.,
           value as value_&i.
       from &metaoutlib..&metaout. 
       where passed_no=&i
       order by code_&i., value_&i.;
   quit;
  %end;

* Find the selection criterias for each query;
  proc sql;
   create table split_combi as
   select distinct
    %do i = 1 %to &variables_passed_limit.;
     t&i..code_&i.,
     t&i..value_&i.,
    %end;
     count(*) as count_combinations
     from
    %do i = 1 %to &variables_passed_limit.;
      passed_&i. as t&i.
      %if &i < &variables_passed_limit. %then
       %do;
        , 
       %end;
    %end;
      order by 
    %do i = 1 %to &variables_passed_limit.;
     t&i..code_&i.,
     t&i..value_&i.,
    %end;
    count_combinations
    ;
  quit;
  proc sql noprint;
   select count_combinations into :rounds trimmed 
   from split_combi
   ;
  quit;
%if &rounds <1 %then
 %do;
  %let rounds=0;
 %end;
%put &note_no_of_rounds.;
%end;

* When the metaonly is requested the query from Statbank is not executed;
%if "&metaonly" NE "1" %then
 %do;

filename json temp;

* general json extract script generated;
data json;
 set &metaoutlib..&metaout. end=last;
 by notsorted ordinal_variables;
 length jsoncode $9999;
 if _n_ = 1 then
  do;
    jsoncode = '{"query": [';
    output;
  end;
 if first.ordinal_variables then
  do;
  * all but the last (time) shall end with a comma. For time we use the actual selected values;
   code = kcvt(code,"&encoding.",'utf-8');
   codename = code;

   jsoncode = cat('{"code": "',strip(code),'",');
   output;
   jsoncode = '"selection": {';
   output;
   if time = 1 then
    do;
     jsoncode = '"filter": "item",';
     output;
     jsoncode = cat('"values": [',"""&selected_periods.""",']}}');
    end;
   else 
    do;
     jsoncode = '"filter": "all",';
     output;
     jsoncode = cat('"values": ["*"]}},');
    end;
   if passed_max = 1 then
    var_split = code;
   output;
   codename = '';
   var_split = '';
  end;
  if last then
   do;
    jsoncode = '],"response": {"format": "csv"}}';
    output;
   end;
 keep jsoncode ordinal_variables var_split codename time ;
run;

* Classification variables;
 proc sql ;
  create table class_vars as
  select distinct ordinal_variables as varorder, code as varname length=35, quote(strip(text)) as varlabel length=999, 
         cats(': $char',max(length(strip(value)||' '||strip(valuetext))),'.') as varformat, 
         max(length(strip(value))) as valuelength
  from &metaoutlib..&metaout.
  where not(time = 1 or upcase(code) in ('CONTENTSCODE','TIEDOT'))
  group by varname
  order by varorder, varname
  ;
 quit;

* Class variables inserted into macro variables for later use;
  data _null_;
   call symputx('class_var_no',0,'L');
  run;

  data _null_;
   set class_vars;
   call symputx('class_var'||strip(_n_),varname,'L');
   call symputx('class_var_no',_n_,'L');
  run;

* combinations of ContentsCode and Time;
  proc sql ;
   create table content_and_time as
   select sum(t1.ordinal_variables,t1.code_no) as varorder, t1.value as code_1, t2.code as code_2, t1.value as value_1, t2.value as value_2,
          catx('_',t1.value,t2.value) as varname length=35,
          quote(strip(t1.valuetext)||' '||strip(t2.valuetext)) as varlabel length=999,
          ' : 8.' as varformat
   from &metaoutlib..&metaout. (where=(upcase(code) in ('CONTENTSCODE','TIEDOT'))) as t1, &metaoutlib..&metaout. (where=(time=1)) as t2
   order by varorder, varname
   ;
  quit;

* For extracts that is divided because the number of cells will be more than the maximum allowed;
%if &rounds > 0 %then
  %do;
   %do i = 1 %to &rounds; /* Start extrating from statbank */
    data json_&i;
     set json;
     file json;
     if _N_ = 1 then
      set split_combi (firstobs=&i obs=&i);
      %do j= 1 %to &variables_passed_limit.; 
       code_&j = kcvt(code_&j,"&encoding.",'utf-8');

       if codename = code_&j then
        do;
         if find(jsoncode,'"filter"') > 0 then
          jsoncode = '"filter": "item",';
         if find(jsoncode,'"values"') > 0 then
          do;
           jsoncode = cat('"values": ["',strip(value_&j),'"]}}');
           %if &j < &variables_passed_limit. %then
           %do;
             jsoncode = cats(jsoncode,',');
           %end;
          end;
        end;
      %end;
     output;
     put jsoncode;
    run;  
    filename csv_&i temp;
    proc http in=json out=csv_&i. url="&url_complete." method="post";
    run; 

    * If more than 30 requests in a minute Statbank will refuse more queries for a while. Hence we add some wait time;
    data _null_;
      if mod(&i,30) = 0 then
       sleep = sleep(&sleep,1);;
    run;
    proc sql ;
     create table content_and_time_extr as
     select *
     from content_and_time
     where 
      %if &variables_passed_limit. > 1 %then
        %do j= 1 %to &variables_passed_limit.; 
         %do k= 1 %to &variables_passed_limit.;
          %if &j ne &k %then
           %do;
           (value_1 in (select value_&j from split_combi (firstobs=&i obs=&i)) and
            value_2 in (select value_&k from split_combi (firstobs=&i obs=&i))) 
           %if &j < &variables_passed_limit. or (&j = &variables_passed_limit. and &k < %eval(&variables_passed_limit.-1)) %then
            %do;
             or
            %end;
           %end;
         %end;
        %end;
       %else
        %do;
         (value_1 in (select value_1 from split_combi (firstobs=&i obs=&i)) or
          value_2 in (select value_1 from split_combi (firstobs=&i obs=&i)))
        %end;
     order by varorder, varname
     ;
    quit;

    filename inputprg temp;
    data inputvars;
     set class_vars content_and_time_extr end=last;
     file inputprg;
     if _n_ = 1 then
      do;
       put "data &outlib..&out._&i.;" ;
       put " infile csv_&i. truncover dsd dlm=',' firstobs=2;" ;
       put " input";
      end;
     varname = cats('"',varname,'"n');
     put @6 varname $35. @45 varformat $12.;
     if last then
      do;
       put @6 ";";
      end;
    run;

    data _null_;
     set inputvars end=last;
     file inputprg mod;
     if _n_ = 1 then
      do;
       put " label " ;
      end;
     put @6 varname $35. @45 " = " @48 varlabel $999.;
     if last then
      do;
       put @6 ";" ;
       put "run;" ;
      end;
    run;

    %include inputprg/source2;

   proc sort data=&outlib..&out._&i.;
    by
    %do c=1 %to &class_var_no.;
     &&class_var&c
    %end;
    ;
   run;
   %end; /* End extracting from statbank */

   * Merge extracts together. The do-loop is to make sure the order of files are correct,by using the : notification the order would be wrong when there are more than 9 iterations;
    data &outlib..&out. (label="%bquote(&TableLabel.)");
     merge 
      %do i = 1 %to &rounds;
       &outlib..&out._&i
      %end;
      ;
    by
    %do i=1 %to &class_var_no.;
     &&class_var&i
    %end;
    ;
    run;
  %end; 
%else
 %do; /* Number of cells are below maximum so only one query to the statbank is needed */
  data _null_;
   set json;
   file json;
   put jsoncode;
  run;
  filename csv_1 temp encoding="&encoding.";
  proc http in=json out=csv_1 url="&url_complete." method="post";
  run; 

 filename inputprg temp;
  data inputvars;
   set class_vars content_and_time end=last;
   file inputprg;
   if _n_ = 1 then
    do;
     put "data &outlib..&out. (label=""%bquote(&TableLabel.)"");" ;
     put " infile csv_1 truncover dsd dlm=',' firstobs=2;" ;
     put " input";
    * if there are no class variables the extract start with a dummy variable because it is then a separator (comma) in the beginning;
    %if &class_var_no. = 0 %then
     %do;
      put @6 'Dummy' @42 '$1.';
     %end;

    end;
    varname = cats('"',varname,'"n');
    put @6 varname $35. @45 varformat $12.;
   if last then
    do;
     put @6 ";";
     %if &class_var_no. = 0 %then
      %do;
       put @6 "drop Dummy;" ;
      %end;
    end;
  run;

  data _null_;
   set inputvars end=last;
   file inputprg mod;
   if _n_ = 1 then
    do;
     put " label " ;
    end;
   put @6 varname $35. @45 " = " @48 varlabel $999.;
   if last then
    do;
     put @6 ";" ;
     put "run;" ;
    end;
  run;

  %include inputprg/source2;


 %end; /* End of code for one query to the statbank */

* Change text in class variables with codes;
%if "&formatvars." = "1" %then
 %do;
  data &outlib..&out. (label="%bquote(&TableLabel.)");
   set &outlib..&out.;
  %if &class_var_no. > 0 %then
   %do i = 1 %to &class_var_no.;
    &&class_var&i.._1 = put(upcase(&&class_var&i),%sysfunc(translate($&&class_var&i.._&language.R.,'OAoa','ÖÄöä')));
    if &&class_var&i.._1 NE 'ZZZZZZZZ' then
     &&class_var&i = &&class_var&i.._1;
*    format &&class_var&i.. $&&class_var&i.._&language..;
    drop &&class_var&i.._1;
   %end;
  run;
 %end;

* Transpose output table (two different possibilities);
%if "&transpose" = "1" or "&transpose" = "2" %then
 %do;
  * Find label for time variable;
  proc sql noprint;
   select distinct text into :timelabel trimmed 
   from &metaoutlib..&metaout.
   where upcase(code) IN('TID','VUOSI')
   ;
  quit;

  proc transpose data=&outlib..&out. out=transp1  ;
   by notsorted _character_;
   var _numeric_;
  run;
  data &outlib..&out.;
   set transp1;
   &contentscodevar. = substr(_name_,1,length(_name_)-findc(reverse(strip(_name_)),'_'));
   &timevar. = reverse(scan(reverse(strip(_name_)),1,'_'));
   format &contentscodevar. $contentscode_&language..;
   "&valuevar."n = col1;
   label "&valuevar."n = "&valuevar.";
   label "&contentscodevar."n = "&contentscodelabel.".;
   label "&timevar."n = "&timelabel.";
   drop _name_ _label_ col1;
  run;
  
  %if "&transpose." = "2" %then
   %do; 
    proc transpose data=&outlib..&out. out=&outlib..&out. (label="%bquote(&TableLabel.)" drop=_name_ _label_) ;
     by notsorted 
     %if &class_var_no. > 0 %then
      %do i = 1 %to &class_var_no.;
       &&class_var&i. 
      %end;
      &contentscodevar. ;
     id &timevar.;
     var _numeric_;
    run;
  %end; /* End transpose = 2 */

 %end;

  * Calculate lengths for character variables; 
  %if &class_var_no. > 0 or "&transpose." = "1" or "&transpose." = "2" %then
   %do;
    proc sql noprint;
     select 
     %if &class_var_no. > 0 %then
      %do i = 1 %to &class_var_no.;
       max(length(&&class_var&i.)), 
      %end;
     %if "&transpose." = "1" %then
      %do;
       max(length(&contentscodevar.)), 
       max(length(&timevar.)), 
      %end;
     %if "&transpose." = "2" %then
      %do;
       max(length(&contentscodevar.)), 
      %end;
      count(1)
     into
     %if &class_var_no. > 0 %then
      %do i = 1 %to &class_var_no.;
       :&&class_var&i.._max trimmed, 
      %end;
     %if "&transpose." = "1" %then
      %do;
       :contentscode_max trimmed, 
       :tid_max trimmed, 
      %end;
     %if "&transpose." = "2" %then
      %do;
       :contentscode_max trimmed, 
      %end;
       :dummy trimmed
       from &outlib..&out.;
      quit;
   %end; /* End calculate lengths for character variables */

  data &outlib..&out. (label="%bquote(&TableLabel.)");
  %if &class_var_no. > 0 or "&transpose." = "1" or "&transpose." = "2" %then
   %do;
    length
     %if &class_var_no. > 0 %then
      %do i = 1 %to &class_var_no.;
       &&class_var&i. $%superq(&&class_var&i.._max) 
      %end;
     %if "&transpose." = "1" %then
      %do;
       &contentscodevar. $&contentscode_max.  
       &timevar.  $&tid_max.
      %end;
     %if "&transpose." = "2" %then
      %do;
       &contentscodevar. $&contentscode_max.  
      %end;
    ;
   %end;
   set &outlib..&out.;
  * Rows where numeric values are all zeros and/or all missing will be deleted;
   %if "&delete_zeros." = "1" %then 
    %do;
     if max(of _numeric_) = 0 and min(of _numeric_) = 0 then 
      delete;
    %end;
   %if "&delete_missing." = "1" %then 
    %do;
     if max(of _numeric_) = . and min(of _numeric_) = . then 
      delete;
    %end;
    /* The formats are added here because Sas vil combine rows with same formatted values during transpose if they have the same text and following each other in the data set, which we don't want*/
    %if &formatvars = 1 and &class_var_no. > 0 %then
     %do i = 1 %to &class_var_no.;
      format &&class_var&i.. %sysfunc(translate($&&class_var&i.._&language..,'OAoa','ÖÄöä'));
     %end;
  run;

  %if &class_var_no. > 0 %then
   %do;
    proc sort data=&outlib..&out. ;
     by 
     %do i = 1 %to &class_var_no.;
      &&class_var&i
     %end;
     ;
    run;
   %end;
%end;

%if "&delete_temp" >= "1" %then
 %do;
  proc datasets lib=work nolist;
  %if &variables_passed_limit. > 0 %then
   %do;
    delete passed_: split_combi content_and_time_extr; 
   %end;
   delete metadata meta1 meta2 meta_t language_texts no_of_cells class_vars ;
   %if "&metaonly" NE "1" %then
    %do;
     delete inputvars content_and_time json:;
    %end;
   %if "&formatvars." = "1" %then
    %do;
     delete formats_recode format_duplicates;
    %end;
   %if ("&transpose." = "1" or  "&transpose." = "2") and "&metaonly" NE "1" %then
    %do;
     delete transp1;
    %end;
  quit;
   %if "&metaonly" NE "1" %then
    %do;
     proc datasets lib=&outlib. nolist;
      delete &out._:  ;
     quit;
    %end;

%end;
%if "&delete_temp" >= "2" %then
 %do;
  proc datasets lib=work nolist;
   delete &metaoutlib..&metaout.;
  quit;
%end;

libname sbmeta clear;

%end; /* End table exists */
 %else
  %do;
   %put &warn_table_exist.;
   %if "&delete_temp" >= "1" %then
    %do;
     proc datasets lib=work nolist;
      delete language_texts ;
     quit;
    %end;
  %end;

%end; /* End table number is numeric */
 %else
  %do;
   %put &warn_table_not_numeric.;

   %if "&delete_temp" >= "1" %then
    %do;
     proc datasets lib=work nolist;
      delete language_texts ;
     quit;
    %end;
  %end;

%mend;

