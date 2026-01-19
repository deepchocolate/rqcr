-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

CREATE TABLE tableB (
  ID INT NOT NULL,
  Var VARCHAR,
  PRIMARY KEY(ID)
);

INSERT INTO tableB VALUES (1,'B');
