/* length script last revised December  2000 */

spool length.atka
set pagesize 0;
set linesize 75;
set termout off;
set feedback off;


select o.vessel_type_code ||'_'||h.cruise||'_'||h.vessel||'_'||h.haul||' '
||h.generic_area              ||' '
||NVL(h.bottom_depth,-9)   ||' '
||to_char(l.DT,'mm')||' '
||to_char(l.DT,'dd')||' '
||to_char(l.DT,'yy')||' '
||h.latitude               ||' '
||h.longitude              ||' '
||l.size_group                 ||' '
||l.frequency              ||' '
||DECODE(l.sex,'M',1,'F',2,'U',3,null)||' '
|| l.species               ||' '
||'-9'
from norpac.foreign_length l, 
		norpac.foreign_haul h,
norpac.foreign_fishing_operation o
where
(l.haul_join = h.haul_join ) and
(o.cruise=h.cruise) and 
(o.vessel=h.vessel) and  
 h.haul_join=l.haul_join and
  l.species =204 and
	h.generic_area between 500 and 544 ;

union

select dh.vessel_type       ||'_'||dh.cruise||'_'||dh.vessel||'_'||dh.haul||' '
||dh.nmfs_area              ||' '
||NVL(dh.bottom_depth,-9)   ||' '
||to_char(dl.haul_date,'mm')||' '
||to_char(dl.haul_date,'dd')||' '
||to_char(dl.haul_date,'yy')||' '
||dh.latitude               ||' '
||dh.longitude              ||' '
||dl.length                 ||' '
||dl.frequency              ||' '
||DECODE(dl.sex,'M',1,'F',2,'U',3,null)||' '
|| dl.species               ||' '
||'-9'
from norpac.domestic_length dl, 
		norpac.domestic_haul dh
where
  dh.haul_join=dl.haul_join and
  dl.species =204 and
	dh.nmfs_area between 500 and 544 ;
	spool off;

