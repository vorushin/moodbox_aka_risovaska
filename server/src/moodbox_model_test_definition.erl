-module(moodbox_model_test_definition).

-export([get_definition/0]).

-include("moodbox_model.hrl").


get_definition() ->
    #model{
		name = 'Test',
		types_info = [
			      ?Struct(envelope, [?FieldOptional(header, header), 
						 ?Field(body, #union{types = [getAuthTicket, getProfile]})] ),
			      
			      ?Struct(header, [?Field(authTicket, binary)] ),
			      
			      ?Function(getAuthTicket, [?Field(login, string), ?Field(password, string)], binary),
			      
			      ?Function(getProfile,    [?Field(ticket, binary)], profile),
			      
			      ?Struct(profile, [?Field(firstname, string), ?Field(lastname, string), ?Field(contry, string), ?Field(birthday, datetime), ?Field(sex, sex)]),
			      
			      ?Enum(sex, [{male, 1}, {female, 2}, {undefined, 3}]),
			      
			      ?Struct(simpletypes, [?Field(atom, atom), ?Field(i, int), ?Field(f, float),
						    ?Field(bin, binary), ?Field(b, bool), ?Field(s, string), 
						    ?Field(d, datetime), ?Field(e, e), ?Field(t, b), 
						    ?FieldDefault(c, string, "empty"),
						    ?FieldOptional(o, string)]),
			      
			      ?Struct(unionAndList, [?Field(u,  #union{types = [b, int]}), 
						     ?Field(u2, #union{types = [b, int, string]}), 
						     ?Field(u3, #union{types = [b, float, string]}), 
						     ?Field(u4, #union{types = [int, #list{type = float}]}),
						     ?Field(u5, #union{types = [int, #list{type = string}]}),
						     ?Field(l,  #list{type = string}),
						     ?Field(l2, #list{type = string}),
						     ?Field(l3, #list{type = bool}),
						     ?Field(l4, #list{type = #list{type = int}}),
						     ?Field(lu, #list{type = #union{types = [b, int, string, datetime]}}),
						     ?Field(b, b)] ),
			      
			      ?Struct(all, [?Field(u,  #union{types = [b, int]}),
					    ?Field(u2, #union{types = [b, int, string]}),
					    ?Field(u3, #union{types = [b, float, string]}),
					    ?Field(u4, #union{types = [int, #list{type = float}]}),
					    ?Field(u5, #union{types = [int, #list{type = string}]}),
					    ?Field(l,  #list{type = string}),
					    ?Field(l2, #list{type = string}),
					    ?Field(l3, #list{type = bool}),
					    ?Field(lu, #list{type = #union{types = [b, int, string, datetime]}}),
					    ?Field(atom, atom), ?Field(i, int), ?Field(f, float),
					    ?Field(bin, binary), ?Field(b, bool), ?Field(s, string),
					    ?Field(d, datetime), ?Field(e, e), ?Field(t, b),
					    ?FieldDefault(c, string, "empty"),
					    ?FieldOptional(o, string)] ),
			      
			      ?Struct(a, [?Field(lu, #list{type = #union{types = [b, int, string]}}),
					  ?Field(u, #union{types = [b, int, #list{type = int}]}),
					  ?Field(b, b), ?Field(l, #list{type = string}),
					  ?Field(d, float), ?Field(e, e)] ),
			 
			      ?Enum(e, [{atom1, 0}, {atom2, 1}, {atom3, 2}]),
			      
			      ?Struct(b, [?FieldDefault(c, string, "empty")] )
			     ]
	       }.
