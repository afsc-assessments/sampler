spool len_pcod1.dat
set pagesize 0;
set linesize 85;
set termout off;
set feedback off;

select h.gear_type       ||' '
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
  ||' '
  || h.vessel||'_'||h.cruise||'_'||h.haul
from norpac.domestic_length l, 
		norpac.domestic_haul h
where
  h.haul_join=l.haul_join and
  l.species =202 and
	h.nmfs_area between 600 and 699 
	;

select p.gear_type||' '
||p.nmfs_area_code||' '
||'-9'||' '
||to_char(x.haul_date,'mm')||' '
||to_char(x.haul_date,'dd')||' '
||to_char(x.haul_date,'yy')||' '
||'-9'||' '
||'-9'||' '
|| x.length||' '
||x.frequency||' '
||DECODE(x.sex,'M',1,'F',2,'U',3,null)||' '
|| x.species||' '
|| '-9' ||' '
|| p.vessel||'_'||p.cruise||'_'||delivery
from norpac.domestic_port p, norpac.domestic_length x
where
      /*join between domestic_length and domestic_port_sample*/ 
      p.port_join=x.port_join and
/*user specifications*/
x.species =202 and
p.nmfs_area_code between 600 and 699 ;
spool off;
