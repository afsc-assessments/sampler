/* age script for revised aug 98 */
spool age_pcod1.dat
set pagesize 0;
set linesize 65;
set termout off;
set feedback off;

select h.gear_type        ||' '||
  h.nmfs_area               ||' '||
  h.bottom_depth            ||' '||
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
  x.species =202 and
  h.nmfs_area between 600 and 699 

select 
gear_type||' '||
p.nmfs_area_code||' '||
'-9'||' '||
to_char(x.haul_date,'mm')||' '||
to_char(x.haul_date,'dd')||' '||
to_char(x.haul_date,'yy')||' '||
'-9'||' '||
'-9'||' '||
NVL(x.age,-9)||' '||
DECODE(x.sex,'M',1,'F',2,'U',3,null)||' '||
x.species||' '||
NVL(x.weight,-9)||' '||
x.length||' '||
x.specimen_number||' '||
x.sampling_system
from norpac.domestic_port p, norpac.domestic_age x
where
p.port_join=x.port_join and
x.species =202 and
p.nmfs_area_code between 600 and 699 ;
spool off;
