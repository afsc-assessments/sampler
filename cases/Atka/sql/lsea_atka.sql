/* length script last revised December  2000 */

spool length.atka
set pagesize 0;
set linesize 50;
set termout off;
set feedback off;

select h.vessel_type       ||'_'||h.cruise||'_'||h.vessel||'_'||h.haul||' '
||h.nmfs_area              ||' '
||NVL(h.bottom_depth,-9)   ||' '
||to_char(l.haul_date,'mm')||' '
||to_char(l.haul_date,'dd')||' '
||to_char(l.haul_date,'yy')||' '
||h.latitude               ||' '
||h.longitude              ||' '
||l.length                 ||' '
||l.frequency              ||' '
||DECODE(l.sex,'M',1,'F',2,'U',3,null)||' '
|| l.species               ||' '
||'-9'
from norpac.domestic_length l, 
		norpac.domestic_haul h
where
  h.haul_join=l.haul_join and
  l.species =204 and
	h.nmfs_area between 500 and 544 ;
	spool off;

