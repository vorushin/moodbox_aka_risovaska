-module(moodbox_version).

-export([check_version/2]).

check_version(undefined = _VersionTag, _Version) ->
    undefined;
check_version(_VersionTag, undefined = _Version) ->
    undefined;
check_version(desktop_client = _VersionTag, "1.4.0.0" = _Version) ->
    supported;
check_version(desktop_client = _VersionTag, "1.4.1.0" = _Version) ->
    supported;
check_version(desktop_client = _VersionTag, "1.4.3.0" = _Version) ->
    supported;
check_version(desktop_client = _VersionTag, "1.4.5.0" = _Version) ->
    supported;
check_version(desktop_client = _VersionTag, "1.5.0.0" = _Version) ->
    supported;
check_version(_VersionTag, _Version) ->
    not_supported.
