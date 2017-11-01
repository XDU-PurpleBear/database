-- The initialiaztion file for SQL database,
-- Copyright (C) 2017 Purple Bear Team 
-- Maintainer: Johann Lee me@qinka.pro
-- sql/initialization.sql

--
-- config database
--

-- create the database
CREATE DATABASE purple
  WITH OWNER = postgres
       TABLESPACE = pg_default
       CONNECTION LIMIT = -1
;
COMMENT ON DATABASE purple
  IS 'The database for library manage syst'

-- change database
\c purple

-- operation loggings
CREATE SCHEMA opt_log;
GRANT ALL -- right to pg
  ON SCHEMA opt_log
  TO postgres
; 
COMMENT ON SCHEMA opt_log
  IS 'The schema for record all of operation.'
;


--
-- create tables
--

-- create table for account (readers and librarians)
CREATE TABLE table_account
  ( key_uuid          UUID          PRIMARY KEY
  , key_password      VARCHAR(512)
  , key_user_name     VARCHAR(64)   UNIQUE
  , key_first_name    VARCHAR(64)
  , key_last_name     VARCHAR(64)
  , key_birthday      DATE
  , key_register_date DATE
  , key_balance       NUMERIC(10,2)
  , key_sex           BOOLEAN
  , key_tel           BIGINT        UNIQUE NOT NULL CHECK(key_tel > 10000000000 AND key_tel < 20000000000)
  , key_right         REAL
  , key_log           UUID
  )
;
COMMENT ON TABLE table_account
  IS 'The infos for user (both readers and librarians)'
;

-- create the table for book (kind)
CREATE TABLE table_book_kind
  ( key_isbn         VARCHAR(20) PRIMARY KEY
  , key_lc           VARCHAR(20)
  , key_name         TEXT
  , key_auth         TEXT[]
  , key_publisher    VARCHAR(64)
  , key_edition      INTEGER
  , key_publish_date DATE
  , key_imgs         UUID
  , key_tags         TEXT[]
  , key_abstract     TEXT
  , key_stu_id       VARCHAR(16)
  )
;
COMMENT ON TABLE table_book_kind
  IS 'The book(kind)'
;

-- create the table of book (instances)
CREATE TABLE table_book_instance
  ( key_uuid   UUID PRIMARY KEY
  , key_isbn   VARCHAR(20) -- without the limitation of foreign key
  , key_status VARCHAR(8)  -- aubr
                           -- a for available
                           -- b for borrow
                           -- u for unavailable
                           -- r for reserve
                           -- when availiable: a---
                           -- when borrow: --b-
  , key_opt_id UUID NULL  -- without the limitation of foreign key
  )
;
COMMENT ON TABLE table_book_instance
  IS 'The book(instance)'
;

-- create the table of operation
CREATE TABLE table_book_operation
  ( key_uuid UUID PRIMARY KEY
  , key_return_date DATE[]
  , key_status VARCHAR(8) -- same to book instance's
  )
;
COMMENT ON TABLE table_book_operation
  IS 'The operations for book'
;

CREATE TABLE table_order_list
  ( key_uuid UUID PRIMARY KEY
  , key_user UUID NOT NULL
  , key_timestamp DATE NOT NULL
  , key_book_opt  UUID[]
  , key_status VARCHAR[6]
  )
;
COMMENT ON TABLE table_book_operation
  IS 'Order list'
;

CREATE TABLE table_image
  ( key_uuid UUID PRIMARY KEY
  , key_mime TEXT NOT NULL
  , key_data BYTEA
  )
;
COMMENT ON TABLE table image
  IS 'images'
;

CREATE TABLE table_location
  ( key_begin TEXT NOT NULL
  , key_end TEXT NOT NULL
  , key_location TEXT PRIMARY KEY
  )
;