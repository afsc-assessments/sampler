/* age script for revised aug 98 */
spool atka_age.dat
set pagesize 0;
set linesize 65;
set termout off;
set feedback off;

select h.vessel_type        ||'_'||h.cruise||'_'||h.vessel||'_'||h.haul||' '||
  h.nmfs_area               ||' '||
  nvl(h.bottom_depth,-9)    ||' '||
  to_char(x.haul_date,'mm') ||' '||
  to_char(x.haul_date,'dd') ||' '||
  to_char(x.haul_date,'yy') ||' '||
  h.latitude                ||' '||
  h.longitude               ||' '||
  NVL(x.age,-9)             ||' '||
  DECODE(x.sex,'M',1,'F',2,
								 'U',3,null)||' '||
  x.species                 ||' '||
  NVL(x.weight,-9)          ||' '||
  x.length                  ||' '||
  x.specimen_number         ||' '||
  x.sampling_system
from 
	norpac.domestic_haul h, 
	norpac.domestic_age x
where
/*join between domestic_age and domestic_hauls*/
  h.haul_join=x.haul_join and
/*user specifications*/
  x.species = 204 and
  h.nmfs_area between 500 and 544 
;
spool off;
