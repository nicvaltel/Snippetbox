CREATE EXTENSION citext;
CREATE EXTENSION pgcrypto;

CREATE TABLE snippets (
id BIGSERIAL PRIMARY KEY NOT NULL,
title VARCHAR(100) NOT NULL,
content TEXT NOT NULL,
created timestamp with time zone NOT NULL,
expires timestamp with time zone NOT NULL
);

CREATE INDEX idx_snippets_created ON snippets(created);

