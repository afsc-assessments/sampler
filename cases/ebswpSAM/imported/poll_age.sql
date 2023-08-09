set colsep ",";
set pagesize 0;
set trimspool on;
set TERMOUT off;
set linesize 1000;
set headsep off;
spool 'poll_age22.csv';
select 
YEAR, HAUL_OFFLOAD, HAUL_OFFLOAD_DATE,NMFS_AREA,GEAR,FMP_SUBAREA,FMP_GEAR,LENGTH,weight,age,SEX
from norpac.debriefed_age_flat_mv where species=201 and fmp_subarea='BS' and year=2022;
spool off;
