%% @author Driebit BV
%% @copyright 2024 Driebit BV
%% @doc Model for managing Wikipedia concepts and using them as keywords. For more
%% information check https://docs.openalex.org/api-entities/concepts and
%% https://docs.openalex.org/api-entities/concepts/concept-object
%% @end

%% Copyright 2024 Driebit BV
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(m_wikiconcept).

-export([
    m_get/3,
    m_post/3,

    get_concept/2,

    insert_keyword/2,

    find/2,
    find/4,
    find_all/3,

    collect_all/1,

    fetch_all/1,
    fetch_all/2,
    fetch/2,
    openalex_autocomplete/3,
    openalex_search/3,

    load/1,
    update_task/1,
    install/1,
    install_data/1
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(OPENALEX_CONCEPTS_URL, "https://api.openalex.org/concepts").
-define(OPENALEX_AUTOCOMPLETE_URL, "https://api.openalex.org/autocomplete/concepts").

-define(is_digit(C), (C >= $0 andalso C =< $9)).


m_get([ <<"id">>, ID | Rest ], _Msg, Context) ->
    case get_concept(ID, Context) of
        {ok, Concept} ->
            {ok, {Concept, Rest}};
        {error, _} = Error ->
            Error
    end;
m_get([ <<"find">>, Text | Rest ], _Msg, Context) ->
    case get_concept(Text, Context) of
        {ok, Concept} ->
            {ok, {Concept, Rest}};
        {error, _} = Error ->
            Error
    end.


m_post([ <<"insert">>, <<"keyword">> ], #{ payload := #{ <<"wikidata">> := WikiDataId } }, Context) ->
    insert_keyword(WikiDataId, Context).


%% @doc Create a keyword for the given wikidata concept. The keyword is created in either the
%% system content group or the default content group.
-spec insert_keyword(ConceptID, Context) -> {ok, m_rsc:resource_id()} | {error, Reason} when
    ConceptID :: binary(),
    Context :: z:context(),
    Reason :: term().
insert_keyword(ConceptID, Context) ->
    case get_concept(ConceptID, Context) of
        {ok, #{
            <<"keyword_id">> := Id
        }} when is_integer(Id) ->
            {ok, Id};
        {ok, #{
            <<"id">> := CId,
            <<"title">> := TitleAll,
            <<"wikidata">> := WikiDataURI
        } = Concept} ->
            Title = filter_lang(TitleAll, Context),
            R = case m_rsc:rid(WikiDataURI, Context) of
                undefined ->
                    Rsc = #{
                        <<"category_id">> => keyword,
                        <<"is_published">> => true,
                        <<"uri">> => WikiDataURI,
                        <<"content_group_id">> => system_content_group,
                        <<"language">> => langs(Title),
                        <<"title">> => Title,
                        <<"summary">> => filter_lang(maps:get(<<"summary">>, Concept, undefined), Context)
                    },
                    case m_rsc:insert(Rsc, Context) of
                        {ok, _} = Ok ->
                            Ok;
                        {error, eacces} ->
                            Rsc1 = Rsc#{
                                <<"content_group_id">> => default_content_group
                            },
                            m_rsc:insert(Rsc1, Context);
                        {error, _} = E ->
                            E
                    end;
                KId ->
                    {ok, KId}
            end,
            case R of
                {ok, KwId} ->
                    z_db:q("
                        update wikiconcept
                        set keyword_id = $1
                        where id = $2
                          and keyword_id is null
                        ",
                        [ KwId, CId ],
                        Context),
                    {ok, KwId};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

langs(#trans{ tr = Tr }) ->
    [ Iso || {Iso, _} <- Tr ].

filter_lang(#trans{ tr = Tr }, Context) ->
    Editable = z_language:editable_language_codes(Context),
    Tr1 = lists:filter(
        fun({Iso, _}) ->
            lists:member(Iso, Editable)
        end,
        Tr),
    Tr2 = case Tr1 of
        [] ->
            En = z_convert:to_binary(proplists:get_value(en, Tr)),
            [ {z_context:language(Context), En} ];
        _ ->
            Tr1
    end,
    #trans{ tr = Tr2 }.


%% @doc Get a concept by URL or code.
get_concept(<<"https://", _/binary>> = URL, Context) ->
    z_db:qmap_props_row("
        select *
        from wikiconcept
        where wikidata = $1
           or openalex = $1
           or wikipedia = $1",
        [ URL ],
        Context);
get_concept(<<"Q", _/binary>> = ID, Context) ->
    z_db:qmap_props_row("
        select *
        from wikiconcept
        where wikidata_id = $1",
        [ ID ],
        Context);
get_concept(<<"C", _/binary>> = ID, Context) ->
    URL = <<"https://openalex.org/", ID/binary>>,
    get_concept(URL, Context);
get_concept(<<"q", ID/binary>>, Context) ->
    get_concept(<<"Q", ID/binary>>, Context);
get_concept(<<"c", ID/binary>>, Context) ->
    get_concept(<<"C", ID/binary>>, Context);
get_concept(_, _Context) ->
    {error, enoent}.

%% @doc Find the concepts matching the text.
-spec find(Text, Context) -> {ok, Concepts} | {error, Reason} when
    Text :: binary() | string(),
    Context :: z:context(),
    Concepts :: list(map()),
    Reason :: term().
find(Text, Context) ->
    find(Text, 1, 20, Context).

%% @doc Find the concepts matching the words in the text.
-spec find(Text, Offset, Limit, Context) -> {ok, Concepts} | {error, Reason} when
    Text :: binary() | string(),
    Offset :: pos_integer(),
    Limit :: pos_integer(),
    Context :: z:context(),
    Concepts :: list(map()),
    Reason :: term().
find(Text, Offset, Limit, Context) ->
    Text1 = z_string:trim(unicode:characters_to_binary(Text)),
    find_1(Text1, Offset, Limit, Context).

find_1(<<"https://", _/binary>> = URL, Offset, Limit, Context) ->
    z_db:qmap_props("
        select *
        from wikiconcept
        where wikidata = $1
           or openalex = $1
           or wikipedia = $1
        order by wikidata_id
        offset $2 limit $3",
        [ URL, Offset-1, Limit ],
        Context);
find_1(Text, Offset, Limit, Context) ->
    case words(z_string:normalize(Text)) of
        [] ->
            find_all(Offset, Limit, Context);
        % [ <<_/utf8>> ] ->
        %     {ok, []};
        [ <<"q", C1, C2, _/binary>> ] when ?is_digit(C1), ?is_digit(C2) ->
            z_db:qmap_props("
                select *
                from wikiconcept
                where wikidata_id = $1
                order by wikidata_id
                offset $2 limit $3",
                [ Text, Offset-1, Limit ],
                Context);
        [ <<"c", C1, C2, _/binary>> ] when ?is_digit(C1), ?is_digit(C2) ->
            URL = <<"https://openalex.org/", Text/binary>>,
            z_db:qmap_props("
                select *
                from wikiconcept
                where openalex = $1
                order by openalex
                offset $2 limit $3",
                [ URL, Offset-1, Limit ],
                Context);
        Words ->
            Where = lists:map(
                fun(Nr) ->
                    [ " n.name like concat('%', $", integer_to_binary(Nr+3), "::text, '%') " ]
                end,
                lists:seq(1, length(Words))),
            Where1 = lists:join(" AND ", Where),
            SQL = iolist_to_binary([
                "select c.*, s.score
                 from wikiconcept c
                    join (
                         select n.id, max(public.similarity(n.name, $1::text)) AS score
                         from wikiconcept_name n
                         where ", Where1, "
                         group by n.id
                         order by score desc
                         offset $2 limit $3
                    ) s
                    on c.id = s.id"
            ]),
            z_db:qmap_props(SQL, [ Text, Offset-1, Limit | Words ], Context)
    end.

%% @doc Find all concepts, ordered by display_name.
find_all(Offset, Limit, Context) ->
    z_db:qmap_props("
        select *
        from wikiconcept
        order by display_name asc
        offset $1
        limit $2",
        [ Offset-1, Limit ], Context).

%% @doc Split a text in words
words(Text) ->
    binary:split(Text, [ <<" ">>, <<":">>, <<",">>, <<".">>, <<";">> ], [ global, trim_all ]).

%% @doc Fetch the list of all wikipedia concepts. This is a quite big
%% JSON with all known concepts and all their translations.
-spec collect_all(Context) -> {ok, Concepts} | {error, Reason} when
    Context :: z:context(),
    Concepts :: map(),
    Reason :: term().
collect_all(Context) ->
    collect_all_1({next, <<"*">>}, [], Context).

collect_all_1(done, Acc, _Context) ->
    {ok, lists:flatten(lists:reverse(Acc))};
collect_all_1({next, _} = Cursor, Acc, Context) ->
    case fetch_all(Cursor, Context) of
        {ok, {Concepts, NextCursor}} ->
            collect_all_1(NextCursor, [Concepts|Acc], Context);
        {error, _} = Error ->
            Error
    end.

%% @doc Fetch the list of all wikipedia concepts. Return the first
%% 200 concepts and a cursor to fetch the next 200.
-spec fetch_all(Context) -> {ok, {Concepts, Cursor}} | {error, Reason} when
    Context :: z:context(),
    Concepts :: list( map() ),
    Cursor :: done | {next, binary()},
    Reason :: term().
fetch_all(Context) ->
    fetch_all({next, <<"*">>}, Context).

fetch_all({next, Cursor}, Context) ->
    Args = [
        {<<"per-page">>, <<"200">>},
        {<<"cursor">>, Cursor}
    ],
    case z_fetch:fetch_json(get, ?OPENALEX_CONCEPTS_URL, Args, [], Context) of
        {ok, #{
            <<"meta">> := #{
                <<"next_cursor">> := NextCursor
            },
            <<"results">> := Results
        }} when is_binary(NextCursor), NextCursor =/= <<>> ->
            {ok, {Results, {next, NextCursor}}};
        {ok, #{
            <<"results">> := Results
        }} ->
            {ok, {Results, done}};
        {error, _} = Error ->
            Error
    end;
fetch_all(done, _Context) ->
    {ok, {[], done}}.

fetch(<<"https://openalex.org/", Id/binary>>, Context) ->
    Url = iolist_to_binary([
        ?OPENALEX_CONCEPTS_URL,
        $/, Id
    ]),
    z_fetch:fetch_json(Url, [], Context);
fetch(<<"https://www.wikidata.org/wiki/", _/binary>> = Q, Context) ->
    Args = [
        {<<"filter">>, <<"id:", Q/binary>>}
    ],
    z_fetch:fetch_json(get, ?OPENALEX_CONCEPTS_URL, Args, [], Context).

openalex_autocomplete(Q, Page, Context) ->
    Args = [
        {<<"q">>, unicode:characters_to_binary(Q)},
        {<<"page">>, integer_to_binary(Page)}
    ],
    z_fetch:fetch_json(get, ?OPENALEX_AUTOCOMPLETE_URL, Args, [], Context).

openalex_search(Q, Page, Context) ->
    Args = [
        {<<"search">>, unicode:characters_to_binary(Q)},
        {<<"page">>, integer_to_binary(Page)}
    ],
    z_fetch:fetch_json(get, ?OPENALEX_CONCEPTS_URL, Args, [], Context).


%% @doc Ensure that all wiki concepts are loaded and updated.
%% @todo Only fetch the concepts that have been updated.
update_task(Context) ->
    load(Context).

%% @doc Fetch all wiki concepts and store them into our database table for
%% easy searching and connection to keyword resources.
load(Context) ->
    load(fetch_all(Context), Context).

load({ok, {Cs, done}}, Context) ->
    load_1(Cs, Context);
load({ok, {Cs, Cursor}}, Context) ->
    z_db:transaction(
        fun(Ctx) ->
            load_1(Cs, Ctx)
        end,
        Context),
    ?LOG_INFO(#{
        in => zotonic_mod_wikiconcept,
        text => <<"Updated wiki concepts">>,
        result => ok,
        count => length(Cs)
    }),
    load(fetch_all(Cursor, Context), Context);
load({error, _} = Error, _Context) ->
    Error.

load_1([], _Context) ->
    ok;
load_1([C|Cs], Context) ->
    #{
        <<"ids">> := #{
            <<"openalex">> := OpenAlex,
            <<"wikidata">> := WikiData
        } = Ids,
        <<"level">> := Level,
        <<"ancestors">> := Ancestors,
        <<"display_name">> := DisplayName,
        <<"image_thumbnail_url">> := ThumbUrl,
        <<"image_url">> := ImageUrl,
        <<"international">> := #{
            <<"description">> := IntlDescription,
            <<"display_name">> := IntlDisplayName
        },
        <<"updated_date">> := Updated
    } = C,
    Mag = maps:get(<<"mag">>, Ids, undefined),
    WikiPedia = maps:get(<<"wikipedia">>, Ids, undefined),
    _UmlsCui = maps:get(<<"umls_cui">>, Ids, undefined),
    Title = case to_trans(IntlDisplayName) of
        undefined ->
            #trans{ tr = [ {en, DisplayName} ] };
        IntTitle ->
            IntTitle
    end,
    Summary = to_trans(IntlDescription),
    Summary1 = ensure_trans(Summary, Title),
    AncestorWikiIds = lists:map(
        fun(#{ <<"wikidata">> := AW }) ->
            wikidata_id(AW)
        end,
        Ancestors),
    Props = #{
        <<"level">> => Level,
        <<"openalex">> => OpenAlex,
        <<"wikidata">> => WikiData,
        <<"wikipedia">> => WikiPedia,
        <<"mag">> => Mag,
        <<"wikidata_id">> => wikidata_id(WikiData),
        <<"wikidata_ancestor_ids">> => AncestorWikiIds,
        <<"display_name">> => DisplayName,
        <<"image_thumbnail_url">> => ThumbUrl,
        <<"image_url">> => ImageUrl,
        <<"props_json">> => #{
            <<"title">> => Title,
            <<"summary">> => Summary1
        },
        <<"modified">> => z_datetime:to_datetime(Updated)
    },
    {ok, CId} = case z_db:q1("
        select id
        from wikiconcept
        where wikidata = $1",
        [ WikiData ],
        Context)
    of
        undefined ->
            {ok, _} = z_db:insert(wikiconcept, Props, Context);
        Id ->
            {ok, _} = z_db:update(wikiconcept, Id, Props, Context),
            {ok, Id}
    end,
    % Add the name index for searching. Index all display names, the generic display
    % name and the slug of the wikipedia url.
    Titles = [ z_string:normalize(T) || T <- texts(Title) ],
    Ts = [ z_string:normalize(DisplayName) | Titles ],
    Ts1 = case WikiPedia of
        undefined -> Ts;
        <<>> -> Ts;
        _ ->
            WikiName = lists:last(binary:split(WikiPedia, <<"/">>, [global])),
            WikiName1 = z_string:normalize(z_url:url_decode(WikiName)),
            [ WikiName1 | Ts ]
    end,
    Ts2 = lists:usort(Ts1),
    Ns = z_db:q("select name from wikiconcept_name where id = $1", [ CId ], Context),
    Ns1 = [ N || {N} <- Ns ],
    NewNs = Ts2 -- Ns1,
    DelNs = Ns1 -- Ts2,
    lists:foreach(
        fun(T) ->
            z_db:q("
                insert into wikiconcept_name (id, name)
                values ($1, $2)",
                [ CId, T ],
                Context)
        end, NewNs),
    lists:foreach(
        fun(T) ->
            z_db:q("
                delete from wikiconcept_name
                where id = $1
                  and name = $2",
                [ CId, T ],
                Context)
        end, DelNs),
    load_1(Cs, Context).

to_trans(Langs) when is_map(Langs) ->
    Tr = maps:fold(
        fun(Lang, Text, Acc) ->
            case z_language:to_language_atom(Lang) of
                {ok, Code} ->
                    [ {Code, Text} | Acc ];
                {error, _} ->
                    Acc
            end
        end,
        [],
        Langs),
    #trans{ tr = Tr };
to_trans(undefined) ->
    undefined.

wikidata_id(<<"https://www.wikidata.org/wiki/", ID/binary>>) ->
    ID.

texts(undefined) ->
    [];
texts(#trans{ tr = Tr }) ->
    [ T || {_, T} <- Tr ].

ensure_trans(undefined, _) ->
    undefined;
ensure_trans(#trans{ tr = S }, #trans{ tr = T }) ->
    SIso = [ Iso || {Iso, _} <- S ],
    TIso = [ Iso || {Iso, _} <- T ],
    S1 = [ {Iso, <<>>} || Iso <- TIso -- SIso ],
    #trans{ tr = S ++ S1 }.

%% @doc Install the datamodel to receive all concepts.
install(Context) ->
    case z_db:table_exists(wikiconcept, Context) of
        true ->
            ok;
        false ->
            [] = z_db:q("create extension if not exists pg_trgm with schema public", Context),
            [] = z_db:q("
                create table wikiconcept (
                    id serial not null,
                    level integer not null default 0,

                    openalex character varying(128) not null,
                    wikidata character varying(128) not null,
                    wikipedia character varying(200),
                    mag character varying(64),

                    wikidata_id character varying(16) not null,
                    wikidata_ancestor_ids character varying(16)[],

                    display_name character varying(128),
                    image_thumbnail_url character varying(1000),
                    image_url character varying(1000),

                    props_json bytea,

                    keyword_id integer,

                    modified timestamp with time zone NOT NULL DEFAULT now(),

                    constraint wikiconcept_pkey primary key (id),
                    constraint wikiconcept_openalex_key unique (openalex),
                    constraint wikiconcept_wikidata_key unique (wikidata),
                    constraint wikiconcept_wikidata_id_key unique (wikidata_id)
                )", Context),

            [] = z_db:q("
                create table wikiconcept_name (
                    id serial not null,
                    name character varying(1000)
                )", Context),

            [] = z_db:q("
                alter table wikiconcept_name add constraint fk_wikiconcept_name_id foreign key (id)
                references wikiconcept (id)
                on update cascade on delete cascade", Context),

            [] = z_db:q("
                alter table wikiconcept add constraint fk_wikiconcept_keyword_id foreign key (keyword_id)
                references rsc (id)
                on update cascade on delete set null", Context),
            [] = z_db:q("create index fki_wikiconcept_keyword_id ON wikiconcept (keyword_id)", Context),

            [] = z_db:q("create index wikiconcept_mag_key on wikiconcept (mag)", Context),
            [] = z_db:q("create index wikiconcept_wikipedia_key on wikiconcept (wikipedia)", Context),

            [] = z_db:q("
                create index wikiconcept_name_name ON wikiconcept_name
                USING GIN (name public.gin_trgm_ops)", Context),

            z_db:flush(Context),
            ok
    end.

%% @doc Ensure that the wikiconcepts are loaded into the local table. They are downloaded from OpenAlex
%% using their public API.
install_data(Context) ->
    case z_db:q("select count(*) from wikiconcept", Context) of
        0 ->
            z_pivot_rsc:insert_task(?MODULE, update_task, <<>>, Context);
        _ ->
            ok
    end.

