%% Run only against a disposable PostgreSQL database, with WIKICONCEPT_TEST_PORT set.
-module(m_wikiconcept_install_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

install_test_() ->
    case os:getenv("WIKICONCEPT_TEST_PORT") of
        false -> [];
        Port -> {timeout, 60, fun() -> check_install(list_to_integer(Port)) end}
    end.

check_install(Port) ->
    {ok, Conn} = epgsql:connect("127.0.0.1", "postgres", "", [
        {database, "postgres"}, {port, Port}
    ]),
    Schema = "wikiconcept_test_" ++ integer_to_list(erlang:unique_integer([positive])),
    try
        sql(Conn, "create schema " ++ Schema),
        sql(Conn, "set search_path to " ++ Schema ++ ", public"),
        sql(Conn, "create table rsc (id integer primary key)"),
        mock_db(),
        fresh_and_repeat(Conn),
        search_totals(Conn),
        legacy_and_repeat(Conn),
        invalid_legacy(Conn)
    after
        meck:unload(z_db),
        meck:unload(z_pivot_rsc),
        sql(Conn, "set search_path to public"),
        sql(Conn, "drop schema " ++ Schema ++ " cascade"),
        epgsql:close(Conn)
    end.

mock_db() ->
    meck:new(z_db, [no_link]),
    meck:expect(z_db, q, fun(Query, Conn) -> sql(Conn, Query) end),
    meck:expect(z_db, q1, fun(Query, Conn) -> scalar(Conn, Query) end),
    meck:expect(z_db, q1, fun(Query, Args, Conn) ->
        {ok, _, [{Value}]} = epgsql:equery(Conn, iolist_to_binary(Query), Args),
        Value
    end),
    meck:expect(z_db, qmap_props, fun(Query, Args, Conn) ->
        {ok, Cols, Rows} = epgsql:equery(Conn, iolist_to_binary(Query), Args),
        {ok, [maps:from_list(lists:zip([C#column.name || C <- Cols], tuple_to_list(R))) || R <- Rows]}
    end),
    meck:expect(z_db, flush, fun(_) -> ok end),
    meck:expect(z_db, table_exists, fun(Table, Conn) ->
        scalar(Conn, "select to_regclass('" ++ atom_to_list(Table) ++ "') is not null")
    end),
    meck:expect(z_db, column, fun(wikiconcept, props_json, Conn) ->
        Type = scalar(Conn, "select data_type from information_schema.columns "
            "where table_schema = current_schema() and table_name = 'wikiconcept' "
            "and column_name = 'props_json'"),
        {ok, #column_def{name = props_json, type = Type}}
    end),
    meck:expect(z_db, transaction, fun(F, Conn) ->
        sql(Conn, "begin"),
        try F(Conn) of
            Result -> sql(Conn, "commit"), Result
        catch Class:Reason:Stack ->
            sql(Conn, "rollback"),
            erlang:raise(Class, Reason, Stack)
        end
    end),
    meck:new(z_pivot_rsc, [no_link]),
    meck:expect(z_pivot_rsc, insert_task, fun(m_wikiconcept, load, <<>>, _) -> {ok, 1} end).

fresh_and_repeat(Conn) ->
    ok = m_wikiconcept:install(Conn),
    ?assertEqual(<<"jsonb">>, column_type(Conn)),
    ok = m_wikiconcept:install_data(Conn),
    ?assertEqual(1, meck:num_calls(z_pivot_rsc, insert_task, '_')),
    sql(Conn, "insert into rsc values (42)"),
    sql(Conn, "insert into wikiconcept (openalex, wikidata, wikidata_id, keyword_id, props_json) "
        "values ('C1', 'Q1', 'Q1', 42, '{\"title\":\"Café\"}'), "
        "('C2', 'Q2', 'Q2', null, null)"),
    sql(Conn, "insert into wikiconcept_name (id, name) values (1, 'cafe')"),
    Before = snapshot(Conn),
    Oid = scalar(Conn, "select 'wikiconcept'::regclass::oid"),
    ok = m_wikiconcept:install(Conn),
    ?assertEqual(Oid, scalar(Conn, "select 'wikiconcept'::regclass::oid")),
    ?assertEqual(Before, snapshot(Conn)),
    ok = m_wikiconcept:install_data(Conn),
    ?assertEqual(1, meck:num_calls(z_pivot_rsc, insert_task, '_')),
    % Foreign keys on a fresh installation must retain their deletion behavior.
    sql(Conn, "delete from rsc where id = 42"),
    ?assertEqual(null, scalar(Conn, "select keyword_id from wikiconcept where id = 1")),
    sql(Conn, "insert into rsc values (42)"),
    sql(Conn, "update wikiconcept set keyword_id = 42 where id = 1").

%% The pager needs the full count even for a page past the end of a filtered result.
search_totals(Conn) ->
    sql(Conn, "insert into wikiconcept_name (id, name) values (1, 'cafe science')"),
    lists:foreach(fun({Text, Total}) ->
        ?assertEqual(Total, m_wikiconcept:count(Text, Conn)),
        {ok, Rows} = m_wikiconcept:find(Text, 1, 50, Conn),
        ?assertEqual(Total, length(Rows))
    end, [{<<>>, 2}, {<<"cafe">>, 1}, {<<"cafe science">>, 1},
          {<<"https://openalex.org/missing">>, 0}, {<<"missing">>, 0}]),
    {ok, []} = m_wikiconcept:find(<<"cafe">>, 301, 50, Conn),
    #search_result{total = 1, is_total_estimated = false, result = []} =
        mod_wikiconcept:observe_search_query(#search_query{
            name = <<"wikiconcept">>, offsetlimit = {301, 50},
            args = #{ <<"q">> => [#{ <<"term">> => <<"text">>, <<"value">> => <<"cafe">> }] }
        }, Conn),
    ok.

legacy_and_repeat(Conn) ->
    Before = snapshot(Conn),
    sql(Conn, "alter table wikiconcept alter column props_json type bytea "
        "using convert_to(props_json::text, 'UTF8')"),
    ok = m_wikiconcept:install(Conn),
    ?assertEqual(<<"jsonb">>, column_type(Conn)),
    ?assertEqual(Before, snapshot(Conn)),
    ok = m_wikiconcept:install(Conn),
    ?assertEqual(Before, snapshot(Conn)),
    sql(Conn, "delete from wikiconcept where id = 1"),
    ?assertEqual(0, scalar(Conn, "select count(*) from wikiconcept_name")).

invalid_legacy(Conn) ->
    sql(Conn, "alter table wikiconcept alter column props_json type bytea "
        "using convert_to(props_json::text, 'UTF8')"),
    sql(Conn, "update wikiconcept set props_json = convert_to('invalid JSON', 'UTF8')"),
    ?assertException(error, {sql_error, _}, m_wikiconcept:install(Conn)),
    ?assertEqual(<<"bytea">>, column_type(Conn)),
    ?assertEqual(<<"invalid JSON">>, scalar(Conn,
        "select convert_from(props_json, 'UTF8') from wikiconcept")),
    ?assertEqual(1, scalar(Conn, "select count(*) from wikiconcept")).

column_type(Conn) ->
    {ok, #column_def{type = Type}} = z_db:column(wikiconcept, props_json, Conn),
    Type.

snapshot(Conn) ->
    {sql(Conn, "select * from wikiconcept order by id"),
     sql(Conn, "select * from wikiconcept_name order by id, name")}.

scalar(Conn, Query) ->
    [{Value}] = sql(Conn, Query),
    Value.

sql(Conn, Query) ->
    case epgsql:equery(Conn, unicode:characters_to_binary(Query), []) of
        {ok, [], []} -> [];
        {ok, _Columns, Rows} -> Rows;
        {ok, Count} -> Count;
        {error, Error} -> erlang:error({sql_error, Error})
    end.
