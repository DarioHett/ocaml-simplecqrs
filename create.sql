CREATE table if not exists events
(
    id     SERIAL PRIMARY KEY,
    agg_id VARCHAR NOT NULL,
    version   BIGINT NOT NULL,
    data      JSONB  NOT NULL,
    UNIQUE (agg_id, version)
);
