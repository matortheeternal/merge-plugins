UPDATE approved_reports
SET merge_version = SUBSTRING(merge_version, 1, 3)
WHERE merge_version LIKE ('2.%');