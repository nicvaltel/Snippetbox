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



-- CREATE USER web;
-- GRANT SELECT, INSERT, UPDATE, DELETE ON snippetbox_db.public.snippets  TO web;
-- -- Important: Make sure to swap 'pass' with a password of your own choosing.
-- ALTER USER web WITH PASSWORD 'pass';

-- psql -h localhost -p 6666 -U web snippetbox_db

-- INSERT INTO snippets (title, content, created, expires) VALUES (
-- 'An old silent pond',
-- 'An old silent pond...\nA frog jumps into the pond,\nsplash! Silence again.\n\n– Matsuo Bashō',
-- (now() at time zone 'utc'),
-- (SELECT CURRENT_DATE + INTERVAL '365 day')

-- );
-- INSERT INTO snippets (title, content, created, expires) VALUES (
-- 'Over the wintry forest',
-- 'Over the wintry\nforest, winds howl in rage\nwith no leaves to blow.\n\n– Natsume Soseki',
-- (now() at time zone 'utc'),
-- (SELECT (now() at time zone 'utc') + INTERVAL '365 day')
-- );

-- INSERT INTO snippets (title, content, created, expires) VALUES (
-- 'First autumn morning',
-- 'First autumn morning\nthe mirror I stare into\nshows my father''s face.\n\n– Murakami Kijo',
-- (now() at time zone 'utc'),
-- (SELECT (now() at time zone 'utc') + INTERVAL '7 day')
-- );