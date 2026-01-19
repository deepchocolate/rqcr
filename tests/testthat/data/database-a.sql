-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

CREATE TABLE tableA (
  ID INT NOT NULL,
  Var VARCHAR,
  PRIMARY KEY(ID)
);

INSERT INTO tableA VALUES (1,'A');
