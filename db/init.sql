begin;

create table system_parameters (
  version varchar default 'init'
);

insert into system_parameters(version) values ('init');


create function get_version() returns varchar as
$get_version$
  select version from system_parameters limit 1;
$get_version$
language sql;

create sequence seq_upload_ids;

create table uploads (
    upload_id       bigint      default nextval('seq_upload_ids')
  , name            varchar     not null
  , file_name       varchar     not null 
  , mime_type       varchar     not null
  , disposition     varchar     not null
  , creation_date   timestamptz default current_timestamp
  , expiration_date timestamptz default null
  );

alter table uploads add primary key (upload_id);
alter table uploads add check (disposition in ('x-default', 'inline', 'attachement'));

create unique index idx_uploads_name on uploads(name);


create function add_upload (
  p_file_name   varchar
, p_mime_type   varchar
, p_disposition varchar
) returns varchar as
$add_upload$
  insert into uploads(
    name
  , file_name 
  , mime_type
  , disposition
  )
  values (
    md5(random()::text) || (case
      when length(p_file_name) = 0 or p_file_name like '.%'
      then p_file_name
      else '_'||p_file_name
    end) -- TODO fix collisions
  , p_file_name
  , p_mime_type
  , p_disposition
  )
  returning name;
$add_upload$
language sql;


create type t_upload as (
  file_name       varchar    
, mime_type       varchar    
, disposition     varchar    
, creation_date   timestamptz);

create function get_upload (p_name varchar) returns setof t_upload as
$get_upload$
  select
    file_name       
  , mime_type       
  , disposition     
  , creation_date   
  from uploads
  where name = p_name
    and (expiration_date is null or expiration_date > current_timestamp);
$get_upload$
language sql;

commit;

