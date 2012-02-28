%%% -*- mode:erlang -*-
{application, moodbox,
 [
  % A quick description of the application.
  {description, "MoodBox Application"},

  % The version of the applicaton
  {vsn, "0.1.0"},

  % All modules used by the application.
  {modules,
   [
      functions_test,moodbox,moodbox_auth,moodbox_context,moodbox_country,moodbox_cpp_codegen,moodbox_database,moodbox_datetime,moodbox_erlsom2erl,moodbox_exception,moodbox_facade,moodbox_logger,moodbox_model,moodbox_model_definition,moodbox_model_test_definition,moodbox_multipart,moodbox_repository,moodbox_sequence,moodbox_server,moodbox_string,moodbox_sup,moodbox_validation,moodbox_xml_generator,moodbox_xml_parser,mt
   ]},

  % All of the registered names the application uses. This can be ignored.
  {registered, []},

  % Applications that are to be started prior to this one. This can be ignored
  % leave it alone unless you understand it well and let the .rel files in
  % your release handle this.
  {applications,
   [
    kernel,
    stdlib
   ]},

  % OTP application loader will load, but not start, included apps. Again
  % this can be ignored as well.  To load but not start an application it
  % is easier to include it in the .rel file followed by the atom 'none'
  {included_applications, []},

  % configuration parameters similar to those in the config file specified
  % on the command line. can be fetched with gas:get_env
  {env, [
         {port, 8080}
        ]},

  % The Module and Args used to start this application.
  {mod, {moodbox, []}}
 ]
}.

