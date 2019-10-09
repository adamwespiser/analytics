drop table if exists events;
create table events (
  id serial primary key,
  session_tracking_id integer,
  category text,
  label text,
  modtime timestamp DEFAULT current_timestamp
);


drop table if exists page_view;
create table page_view (
  id serial primary key,
  session_tracking_id integer,
  url_filepath text,
  modtime timestamp DEFAULT current_timestamp
);


drop table if exists user_session;
create table user_session (
  id serial primary key,
  modtime timestamp DEFAULT current_timestamp
);
