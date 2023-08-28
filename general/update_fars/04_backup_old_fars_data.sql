-- This is run on the main SSPF database
CREATE TABLE static.fars_processed_2015_2019_backup (LIKE static.fars_processed INCLUDING ALL);

INSERT INTO static.fars_processed_2015_2019_backup
    SELECT *
    FROM static.fars_processed
;