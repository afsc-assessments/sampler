set colsep ,;
set pagesize 0;
set linesize 1000;
set trimspool on;
set TERMOUT off;
set headsep off;
spool 'poll_len.csv';
select 
YEAR, HAUL_OFFLOAD, HAUL_OFFLOAD_DATE,NMFS_AREA,GEAR,FMP_SUBAREA,FMP_GEAR,LENGTH,FREQUENCY,SEX
from norpac.debriefed_length_mv where species=201 and 
FMP_SUBAREA='BS' and YEAR=2022;
spool off