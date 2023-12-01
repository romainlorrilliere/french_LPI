SELECT dblink_connect('dbname=stoc_eps');


CREATE SERVER stoc_eps
        FOREIGN DATA WRAPPER postgres_fdw
        OPTIONS (dbname 'stoc_eps');
		
SELECT * FROM dblink('dbname=stoc_eps','SELECT pk_point FROM point LIMIT 100'); 


