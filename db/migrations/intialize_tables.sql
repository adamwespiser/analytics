drop table if exists events;
create table events (
  id serial primary key,
  session_tracking_id text,
  category text,
  label text,
  modtime timestamp DEFAULT current_timestamp
);


drop table if exists page_view;
create table page_view (
  id serial primary key,
  session_tracking_id text,
  url_filepath text,
  modtime timestamp DEFAULT current_timestamp
);


-- TODO set up dependecies and track session ids
-- for now, we'll just grab a session_tracking_id,
-- and store it
drop table if exists user_session;
create table user_session (
  session_tracking_id text,
  modtime timestamp DEFAULT current_timestamp
);
