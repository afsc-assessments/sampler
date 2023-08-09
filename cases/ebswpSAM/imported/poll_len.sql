set colsep ,;
set pagesize 0;
set trimspool on;
set headsep off;
spool 'c:\users\jim.ianelli\poll_len.csv';
select 
YEAR, HAUL_OFFLOAD, HAUL_OFFLOAD_DATE,NMFS_AREA,GEAR,FMP_SUBAREA,FMP_GEAR,LENGTH,FREQUENCY,SEX
from norpac.debriefed_length_mv where species=201;