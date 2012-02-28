del /q priv\generated_cpp\*
erl -eval "code:add_pathz(\"../ebin\"),make:all(),moodbox_cpp_codegen:test(),init:stop()"