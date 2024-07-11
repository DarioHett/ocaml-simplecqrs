CREATE table if not exists events
(
    id     SERIAL PRIMARY KEY,
    agg_id BIGINT NOT NULL,
    version   BIGINT NOT NULL,
    data      JSONB  NOT NULL,
    UNIQUE (agg_id, version)
);

-- INSERT INTO events(agg_id, version, data) 
-- VALUES (1, 2, '{"test": 123}');

-- notify a, '';
-- SELECT pg_notify('a', 'new');
