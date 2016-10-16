begin;

drop sequence if exists seq_upload_ids cascade;

drop table if exists system_parameters cascade;
drop table if exists uploads cascade;

drop function if exists add_upload(varchar) cascade;
drop function if exists get_upload (varchar) cascade;
drop function if exists get_version() cascade;

drop function if exists add_upload (varchar, varchar,  varchar);
drop type if exists t_upload;
commit;

