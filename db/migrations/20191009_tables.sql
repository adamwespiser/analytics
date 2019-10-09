drop table if exists events;
create table events (
  id serial primary key,
  user_session_id uuid,
  category text,
  label text,
  modtime timestamp DEFAULT current_timestamp
);


drop table if exists page_view;
create table page_view (
  id serial primary key,
  user_session_id uuid,
  url_filepath text,
  modtime timestamp DEFAULT current_timestamp
);


drop table if exists user_session;
create table user_session (
  id uuid primary key DEFAULT md5(random()::text || clock_timestamp()::text)::uuid,
  modtime timestamp DEFAULT current_timestamp
);
