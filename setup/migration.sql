START TRANSACTION;
INSERT INTO approved_reports (username, filename, record_count, rating, merge_version, notes, date_submitted) SELECT * FROM reports;
UPDATE approved_reports SET hash='0', game='tes5';
COMMIT;