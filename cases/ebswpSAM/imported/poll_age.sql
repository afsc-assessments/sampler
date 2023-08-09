set colsep ",";
set pagesize 0;
set trimspool on;
set headsep off;
spool 'c:\users\jim.ianelli\testage.csv';
select 
YEAR, HAUL_OFFLOAD, HAUL_OFFLOAD_DATE,NMFS_AREA,GEAR,FMP_SUBAREA,FMP_GEAR,LENGTH,weight,age,SEX
from norpac.debriefed_age_flat_mv where species=201;
spool off;