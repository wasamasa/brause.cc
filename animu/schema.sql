CREATE TABLE IF NOT EXISTS episodes(
    aid INTEGER,
    title TEXT,
    airdate TEXT,
    epno INTEGER,
    epid INTEGER,
    UNIQUE(aid, epid)
);

CREATE TABLE IF NOT EXISTS releases(
    aid INTEGER,
    atitle TEXT,
    gid INTEGER,
    gtitle TEXT,
    eprange TEXT,
    lastup TEXT,
    lastep INTEGER,
    completed BOOLEAN,
    UNIQUE(aid, gid, lastep)
);
