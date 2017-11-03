

CREATE TABLE table_upstream
  ( isbn BIGINT PRIMARY KEY
  , lc TEXT
  , title TEXT
  , auths TEXT[]
  , publisher TEXT
  , edition Int
  , publish_date DATE
  , img_uuid UUID
  , tags TEXT[]
  , abstract TEXT
  )
;

CREATE TABLE table_image
  ( uuid UUID PRIMARY KEY
  , img BYTEA
  , mime TEXT
  )
;
