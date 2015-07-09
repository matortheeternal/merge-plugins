CREATE TABLE users 
(
ip VARCHAR(30) NOT NULL,
username VARCHAR(30),
auth VARCHAR(32),
firstSeen TIMESTAMP, 
lastSeen TIMESTAMP,
timesSeen INT,
download BIGINT,
upload BIGINT,
timesRun INT,
mergesBuilt INT,
pluginsChecked INT,
pluginsMerged INT,
reportsSubmitted INT
);

CREATE TABLE blacklist
(
ip VARCHAR(30) NOT NULL,
username VARCHAR(30),
created TIMESTAMP,
expires TIMESTAMP
);

CREATE TABLE approved_reports
(
game VARCHAR(20),
username VARCHAR(20),
filename VARCHAR(64),
hash VARCHAR(16),
record_count int,
rating int,
merge_version VARCHAR(10),
notes VARCHAR(255),
date_submitted timestamp
);

CREATE TABLE unapproved_reports
(
game VARCHAR(20),
username VARCHAR(20),
filename VARCHAR(64),
hash VARCHAR(16),
record_count int,
rating int,
merge_version VARCHAR(10),
notes VARCHAR(255),
date_submitted timestamp
);


CREATE TABLE tes5dictionary
(
filename VARCHAR(64),
hash VARCHAR(16),
average_rating float,
num_ratings int,
notes VARCHAR(4096)
);


CREATE TABLE tes4dictionary
(
filename VARCHAR(64),
hash VARCHAR(16),
average_rating float,
num_ratings int,
notes VARCHAR(4096)
);


CREATE TABLE fnvdictionary
(
filename VARCHAR(64),
hash VARCHAR(16),
average_rating float,
num_ratings int,
notes VARCHAR(4096)
);


CREATE TABLE fo3dictionary
(
filename VARCHAR(64),
hash VARCHAR(16),
average_rating float,
num_ratings int,
notes VARCHAR(4096)
);