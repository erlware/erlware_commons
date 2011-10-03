%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-
{application, erlware_commons,
 [{description, "Additional standard library for Erlang"},
  {vsn, "0.5.0"},
  {modules, [
             ec_lists,
             ec_plists,
             ec_file,
             ec_string,
             ec_semver,
             ec_talk
            ]},
  {registered, []},
  {applications, [kernel, stdlib]}]}.
