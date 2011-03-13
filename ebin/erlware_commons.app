%% -*- mode: Erlang; fill-column: 75; comment-column: 50; -*-
{application, erlware_commons,
 [{description, "Additional standard library for Erlang"},
  {vsn, "0.2.0"},
  {modules, [
	     ec_lists,
	     ec_string,
	     ec_semver
	    ]},
  {registered, []},
  {applications, [kernel, stdlib]}]}.
