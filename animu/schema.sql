CREATE TABLE episodes(
    aid INTEGER,
    title TEXT,
    airdate TEXT,
    epno INTEGER,
    epid INTEGER,
    UNIQUE(aid, epid)
);
