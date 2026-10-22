-module(m_wikiconcept_keyword_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

keyword_category_test() ->
    Modules = [m_category, m_rsc, z_db, z_language],
    lists:foreach(fun(M) -> meck:new(M, [no_link]) end, Modules),
    try
        meck:expect(z_language, editable_language_codes, fun(_) -> [en] end),
        meck:expect(m_category, is_a, fun(Category, keyword, _) -> Category =:= 10 end),
        meck:expect(m_rsc, rid, fun(_, _) -> undefined end),
        Concept = #{
            <<"id">> => 1,
            <<"display_name">> => <<"Science">>,
            <<"wikidata">> => <<"https://www.wikidata.org/entity/Q336">>
        },
        meck:expect(z_db, qmap_props_row, fun(_, _, _) -> {ok, Concept} end),
        meck:expect(z_db, q, fun(_, [42, 1], _) -> 1 end),
        meck:expect(m_rsc, insert, fun(#{<<"category_id">> := 10}, _) -> {ok, 42} end),
        ?assertEqual({error, eacces}, m_wikiconcept:insert_keyword(<<"Q336">>, 99, context)),
        ?assertEqual(0, meck:num_calls(z_db, qmap_props_row, '_')),
        ?assertEqual({ok, 42}, m_wikiconcept:insert_keyword(<<"Q336">>, 10, context)),

        % Resource ACL failures must not connect a keyword.
        meck:expect(m_rsc, insert, fun(_, _) -> {error, eacces} end),
        ?assertEqual({error, eacces}, m_wikiconcept:insert_keyword(<<"Q336">>, 10, context)),
        ?assertEqual(1, meck:num_calls(z_db, q, '_')),

        % Repeated submission reuses the existing connection.
        meck:expect(z_db, qmap_props_row, fun(_, _, _) ->
            {ok, Concept#{<<"keyword_id">> => 42}}
        end),
        ?assertEqual({ok, 42}, m_wikiconcept:insert_keyword(<<"Q336">>, 10, context)),
        ?assertEqual(1, meck:num_calls(z_db, q, '_'))
    after
        lists:foreach(fun meck:unload/1, Modules)
    end.

keyword_language_test() ->
    Modules = [m_category, m_rsc, z_db, z_language],
    lists:foreach(fun(M) -> meck:new(M, [no_link]) end, Modules),
    try
        meck:expect(m_category, is_a, fun(10, keyword, _) -> true end),
        meck:expect(m_rsc, rid, fun(_, _) -> undefined end),
        meck:expect(z_db, q, fun(_, [42, 1], _) -> 1 end),
        meck:expect(z_language, editable_language_codes, fun(_) -> [nl, de] end),
        meck:expect(z_language, default_language, fun(_) -> nl end),
        Base = #{
            <<"id">> => 1,
            <<"display_name">> => <<"Science">>,
            <<"wikidata">> => <<"https://www.wikidata.org/entity/Q336">>,
            <<"summary">> => #trans{tr = [{en, <<"Study of nature">>}, {nl, <<>>}]}
        },
        Cases = [
            {Base, [{nl, <<"Science">>}]},
            {Base#{<<"title">> => #trans{tr = [{en, <<"Science">>}, {fr, <<"Sciences">>}]}},
                [{nl, <<"Science">>}]},
            {Base#{<<"title">> => #trans{tr = [{en, <<"Science">>}, {nl, <<"  ">>}]}},
                [{nl, <<"Science">>}]},
            {Base#{<<"title">> => #trans{tr = [{en, <<"Science">>}, {de, <<"Wissenschaft">>}]}},
                [{de, <<"Wissenschaft">>}]},
            {Base#{<<"title">> => #trans{tr = [{fr, <<"Sciences">>}]}},
                [{nl, <<"Science">>}]},
            {Base#{<<"display_name">> => <<>>, <<"title">> => #trans{tr = []}},
                [{nl, <<"Q336">>}]}
        ],
        lists:foreach(fun({Concept, Expected}) ->
            meck:expect(z_db, qmap_props_row, fun(_, _, _) -> {ok, Concept} end),
            meck:expect(m_rsc, insert, fun(Props, _) ->
                ?assertEqual(#trans{tr = Expected}, maps:get(<<"title">>, Props)),
                ?assertEqual([Iso || {Iso, _} <- Expected], maps:get(<<"language">>, Props)),
                ?assertEqual(#trans{tr = [{nl, <<"Study of nature">>}]}, maps:get(<<"summary">>, Props)),
                {ok, 42}
            end),
            ?assertEqual({ok, 42}, m_wikiconcept:insert_keyword(<<"Q336">>, 10, context))
        end, Cases)
    after
        lists:foreach(fun meck:unload/1, Modules)
    end.
