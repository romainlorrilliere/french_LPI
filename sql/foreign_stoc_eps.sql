 CREATE SERVER stoc_eps_fw FOREIGN DATA WRAPPER postgres_fdw OPTIONS (host 'localhost', dbname 'stoc_eps', port '5432');;
 
 CREATE USER MAPPING FOR postgres SERVER stoc_eps_fw OPTIONS (user 'postgres', password 'postgres');
 
 CREATE SCHEMA stoc_eps_fw;
 
 IMPORT FOREIGN SCHEMA public FROM SERVER stoc_eps_fw INTO stoc_eps_fw;
 
 IMPORT FOREIGN SCHEMA public LIMIT TO (point_aqua) FROM SERVER stoc_eps_fw INTO stoc_eps_fw;
 
 
 IMPORT FOREIGN SCHEMA <foreign_schema> 
    LIMIT TO (<new_table1>, <new_table2>)
    FROM SERVER <foreign_server>
    INTO <local_schema>;