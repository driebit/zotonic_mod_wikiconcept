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
-moduledoc(#{
    zotonic_keywords => [
        "reference", "frontend_developer", "backend_developer", "model",
        "search_and_discovery", "api_and_integration", "semantic_web"
    ]
}).
-moduledoc("
Look up imported Wikidata/OpenAlex concepts, search their names and connect
them to keyword resources. Concept rows are binary-keyed maps, with properties
from `props_json` merged into the result. Imported text is not guaranteed to
be safe HTML; escape it when displayed.

## Read paths

| Path under `m.wikiconcept` | Result |
| --- | --- |
| `id[identifier]` | One concept by Wikidata Q-code, OpenAlex C-code or stored HTTPS URL. |
| `find[identifier]` | Alias for `id`; this is not a free-text search. |
| `find_keyword[identifier]` | Best matching connected keyword resource ID. |
| `connected[resource_id]` | List of concepts connected to a resource. |
| `list_all` | Concepts ordered by local numeric ID, using request arguments `page` and `pagelen` (defaults 1 and 50). |
| `total` | Number of imported concepts. |

For example:

```django
{% for concept in m.wikiconcept.connected[id] %}
    {{ concept.display_name|escape }}
{% endfor %}
```

Identifiers accept Q/C prefixes in either case, or an exact stored Wikidata,
OpenAlex or Wikipedia HTTPS URL. A local numeric concept ID is not accepted
by `get_concept/2`. Successful model reads return `{ok, {Value, Rest}}`;
unknown identifiers return `{error, enoent}`. Unsupported model paths have
no catch-all clause. Read paths do not check resource visibility or module
permissions, including when returning connected keyword IDs.

`find_keyword/2` first uses the concept's own keyword, then the connected
ancestor with the greatest level, then a connected descendant with the
smallest level. It returns `{ok, ResourceId}` or `{error, enoent}`.

## Write paths

POST to these model paths with a binary-keyed payload map:

| Path | Payload | Behavior |
| --- | --- | --- |
| `insert/keyword` | `wikidata` | Return or create a keyword for the concept, returning `{ok, ResourceId}`. |
| `connect` | `wikidata`, `id` | Connect the concept to the keyword resource, replacing a previous connection. |
| `disconnect` | `wikidata`, `id` | Disconnect the concept from the keyword resource. |

For example, the HTTP model endpoint for connecting is
`/api/model/wikiconcept/post/connect`, with a JSON body such as
`{\"wikidata\":\"Q420\",\"id\":1234}`. The transport's authentication and topic
ACLs apply independently of the model checks.

Connect/disconnect require `z_acl:rsc_editable/2` and a resource in the keyword
category; failures return `eacces` or `enoent`. Their direct Erlang helpers do
not repeat these ACL checks. Keyword insertion returns an existing connection
or matching resource URI when available; otherwise it calls `m_rsc:insert/2`,
trying the system content group and then the default group on `eacces`.
Reusing a matching resource URI requires an editable keyword resource.
`insert_keyword/3` accepts a category within the keyword subtree;
`insert_keyword/2` defaults to `keyword`. Resource insertion checks the selected
category and content group permissions. An already connected keyword is returned unchanged.

For direct Erlang calls with a selected keyword subcategory:

```erlang
m_wikiconcept:insert_keyword(<<\"Q420\">>, CategoryId, Context).
```

The model POST path uses the default `keyword` category; category selection is
available through `insert_keyword/3` and the admin concept dialog.

New keywords are published and use the concept's translated `title` and `summary`.
Each field retains nonempty translations matching the site's editable languages.
If none match, a nonempty English translation is copied to
`z_language:default_language/1`, independently of the current request language.
For titles, missing English text falls back to `display_name`, then the supplied
concept identifier. The resource's `language` property follows the resulting title
translations. A summary without a matching or English translation is omitted.

## Local search and remote import

```erlang
m_wikiconcept:find(<<\"biology\">>, 1, 20, Context).
m_wikiconcept:get_concept(<<\"Q420\">>, Context).
```

`find/2` defaults to offset 1 and limit 20. `find/4` uses a one-based offset,
normalizes text and matches words against local names, using PostgreSQL
trigram similarity for scoring. Empty search text lists concepts by display
name. Results are `{ok, [ConceptMap]}` or an error. `count/2` returns the exact
number of matching concepts using the same criteria, without pagination.

`find_descendant/2` defaults to offset 1 and limit 1000; `/4` accepts explicit
pagination. It finds descendants one level below the first matching concept.
`list_all_concepts/3` instead accepts a one-based page number and page size.

`fetch_all/1,2` fetch OpenAlex pages and return
`{ok, {Concepts, done | {next, Cursor}}}` or an error. `load/1` downloads and
stores the complete catalogue. Remote `fetch/2`, `openalex_search/3` and
`openalex_autocomplete/3` return provider response data through `z_fetch`;
they are separate from the local model lookup paths.

Installation and migration preserve existing rows and keyword links.
`install_data/1` queues an import only for an empty catalogue; use the admin
Reimport action to refresh populated tables. Imports update records by their
Wikidata URL and reconcile search names while retaining keyword connections.
").

-export([
    m_get/3,
    m_post/3,

    connect_keyword/3,
    disconnect_keyword/3,
    list_connected_concepts/2,
    list_all_concepts/3,
    count_all_concepts/1,

    get_concept/2,

    insert_keyword/2,
    insert_keyword/3,
    find_keyword/2,
    rsc_merge/3,

    find/2,
    find/4,
    count/2,
    find_all/3,

    find_descendant/2,
    find_descendant/4,

    collect_all/1,

    fetch_all/1,
    fetch_all/2,
    fetch/2,
    openalex_autocomplete/3,
    openalex_search/3,

    load/1,
    load/2,
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
    end;
m_get([ <<"find_keyword">>, ConceptID | Rest ], _Msg, Context) ->
    case find_keyword(ConceptID, Context) of
        {ok, RscId} ->
            {ok, {RscId, Rest}};
        {error, _} = Error ->
            Error
    end;
m_get([ <<"connected">>, RscId | Rest ], _Msg, Context) ->
    {ok, Concepts} = list_connected_concepts(RscId, Context),
    {ok, {Concepts, Rest}};
m_get([ <<"list_all">> | Rest ], _Msg, Context) ->
    PageSize = z_convert:to_integer(z_context:get_q(<<"pagelen">>, Context, <<"50">>)),
    PageNum = z_convert:to_integer(z_context:get_q(<<"page">>, Context, <<"1">>)),
    {ok, Concepts} = list_all_concepts(PageNum, PageSize, Context),
    {ok, {Concepts, Rest}};
m_get([ <<"total">> | Rest ], _Msg, Context) ->
    {ok, {count_all_concepts(Context), Rest}}.

m_post([ <<"insert">>, <<"keyword">> ], #{ payload := #{ <<"wikidata">> := WikidataID } }, Context) ->
    insert_keyword(WikidataID, Context);
m_post([ <<"connect">> ],
        #{
            payload := #{
                <<"wikidata">> := WikidataID,
                <<"id">> := Id
            }
        },
        Context) ->
    case z_acl:rsc_editable(Id, Context) of
        true ->
            case m_rsc:is_a(Id, keyword, Context) of
                true ->
                    connect_keyword(WikidataID, Id, Context);
                false ->
                    {error, enoent}
            end;
        false ->
            {error, eacces}
    end;
m_post([ <<"disconnect">> ],
        #{
            payload := #{
                <<"wikidata">> := WikidataID,
                <<"id">> := Id
            }
        },
        Context) ->
    case z_acl:rsc_editable(Id, Context) of
        true ->
            case m_rsc:is_a(Id, keyword, Context) of
                true ->
                    disconnect_keyword(WikidataID, Id, Context);
                false ->
                    {error, enoent}
            end;
        false ->
            {error, eacces}
    end.


%% @doc Connect a keyword to a wikidata concept.
%% Any earlier connection is overwritten.
-spec connect_keyword(WikidataID, Id, Context) -> ok | {error, Reason} when
    WikidataID :: binary() | string(),
    Id :: m_rsc:resource() | undefined,
    Context :: z:context(),
    Reason :: enoent.
connect_keyword(WikidataID, Id, Context) ->
    WikidataID1 = unicode:characters_to_binary(WikidataID),
    Id1 = m_rsc:rid(Id, Context),
    PrevId = z_db:q1("
        select keyword_id
        from wikiconcept
        where wikidata_id = $1",
        [ WikidataID1 ],
        Context),
    case m_rsc:rid(Id, Context) of
        PrevId ->
            ok;
        Id1 ->
            case z_db:q("
                update wikiconcept
                set keyword_id = $1
                where wikidata_id = $2",
                [ Id1, WikidataID1 ],
                Context)
            of
                1 ->
                    publish_update(PrevId, Context),
                    publish_update(Id1, Context),
                    ok;
                0 ->
                    {error, enoent}
            end
    end.

%% @doc Disconnect a keyword from a wikidata concept.
-spec disconnect_keyword(WikidataID, Id, Context) -> ok | {error, Reason} when
    WikidataID :: binary() | string(),
    Id :: m_rsc:resource() | undefined,
    Context :: z:context(),
    Reason :: enoent.
disconnect_keyword(WikidataID, Id, Context) ->
    WikidataID1 = unicode:characters_to_binary(WikidataID),
    case m_rsc:rid(Id, Context) of
        undefined ->
            {error, enoent};
        Id1 ->
            case z_db:q("
                update wikiconcept
                set keyword_id = null
                where keyword_id = $1
                  and wikidata_id = $2",
                [ Id1, WikidataID1 ],
                Context)
            of
                1 ->
                    publish_update(Id1, Context),
                    ok;
                0 ->
                    {error, enoent}
            end
    end.

publish_update(undefined, _Context) ->
    ok;
publish_update(Id, Context) when is_integer(Id) ->
    z_mqtt:publish(
        [ <<"model">>, <<"rsc">>, <<"event">>, Id, <<"wikiconcept">> ],
        #{},
        Context).

%% @doc List all concepts connected to a keyword.
-spec list_connected_concepts(RscId, Context) -> {ok, Concepts} | {error, Reason} when
    RscId :: m_rsc:resource(),
    Context :: z:context(),
    Concepts :: list( map() ),
    Reason :: term().
list_connected_concepts(RscId, Context) ->
    z_db:qmap_props("
        select *
        from wikiconcept
        where keyword_id = $1",
        [ m_rsc:rid(RscId, Context) ],
        Context).

list_all_concepts(PageNum, PageSize, Context) ->
    Offset = PageSize * (PageNum - 1),
    z_db:qmap_props("
        select *
        from wikiconcept
        order by id asc
        offset $1
        limit $2",
        [ Offset, PageSize ],
        Context
    ).

count_all_concepts(Context) ->
    z_db:q1("SELECT count(*) FROM wikiconcept", Context).

%% @doc Create a keyword for the given wikidata concept. The keyword is created in either the
%% system content group or the default content group.
-spec insert_keyword(ConceptID, Context) -> {ok, m_rsc:resource_id()} | {error, Reason} when
    ConceptID :: binary(),
    Context :: z:context(),
    Reason :: term().
insert_keyword(ConceptID, Context) ->
    insert_keyword(ConceptID, keyword, Context).

%% @doc Create a keyword in the selected keyword category or one of its subcategories.
%% Resource insertion enforces the category and content group ACLs.
-spec insert_keyword(binary(), m_category:category(), z:context()) ->
    {ok, m_rsc:resource_id()} | {error, term()}.
insert_keyword(ConceptID, Category, Context) ->
    case m_category:is_a(Category, keyword, Context) of
        true -> insert_keyword_1(ConceptID, Category, Context);
        false -> {error, eacces}
    end.

insert_keyword_1(ConceptID, Category, Context) ->
    case get_concept(ConceptID, Context) of
        {ok, #{
            <<"keyword_id">> := Id
        }} when is_integer(Id) ->
            {ok, Id};
        {ok, #{
            <<"id">> := CId,
            <<"display_name">> := DisplayName,
            <<"wikidata">> := WikiDataURI
        } = Concept} ->
            Title = keyword_title(maps:get(<<"title">>, Concept, undefined), DisplayName, ConceptID, Context),
            R = case m_rsc:rid(WikiDataURI, Context) of
                undefined ->
                    Rsc = #{
                        <<"category_id">> => Category,
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
                    case m_rsc:is_a(KId, keyword, Context) andalso z_acl:rsc_editable(KId, Context) of
                        true -> {ok, KId};
                        false -> {error, eacces}
                    end
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

%% Always provide a nonempty English fallback before selecting the site languages.
keyword_title(Title, DisplayName, ConceptID, Context) ->
    Tr = case Title of
        #trans{tr = Translations} -> Translations;
        Text when is_binary(Text) -> [{en, Text}];
        _ -> []
    end,
    English = case nonempty_text(proplists:get_value(en, Tr)) of
        true -> proplists:get_value(en, Tr);
        false ->
            case nonempty_text(DisplayName) of
                true -> DisplayName;
                false -> ConceptID
            end
    end,
    filter_lang(#trans{tr = lists:keystore(en, 1, Tr, {en, English})}, Context).

filter_lang(undefined, _Context) ->
    undefined;
filter_lang(#trans{ tr = Tr }, Context) ->
    Editable = z_language:editable_language_codes(Context),
    Filled = [{Iso, Text} || {Iso, Text} <- Tr, nonempty_text(Text)],
    case [{Iso, Text} || {Iso, Text} <- Filled, lists:member(Iso, Editable)] of
        [] ->
            case proplists:get_value(en, Filled) of
                undefined -> undefined;
                English -> #trans{tr = [{z_language:default_language(Context), English}]}
            end;
        Matching ->
            #trans{tr = Matching}
    end;
filter_lang(Text, Context) when is_binary(Text) ->
    filter_lang(#trans{tr = [{en, Text}]}, Context).

nonempty_text(Text) when is_binary(Text) ->
    z_string:trim(Text) =/= <<>>;
nonempty_text(_) ->
    false.


%% @doc Handle the merge of two keywords. Merge all connected concepts.
-spec rsc_merge(WinnerId, LoserId, Context) -> ok when
    WinnerId :: m_rsc:resource_id(),
    LoserId :: m_rsc:resource_id(),
    Context :: z:context().
rsc_merge(WinnerId, LoserId, Context) ->
    case z_db:q("
        update wikiconcept
        set keyword_id = $1
        where keyword_id = $2",
        [ WinnerId, LoserId ],
        Context)
    of
        0 ->
            ok;
        Count when is_integer(Count) ->
            publish_update(WinnerId, Context)
    end.

%% @doc Find the keyword best matching to the given concept.
%% First tries to find a keyword in the ascendants of a concept,
%% then tries to find it in the descendents.
-spec find_keyword(ConceptID, Context) -> {ok, KeywordRscId} | {error, Reason} when
    ConceptID :: binary() | string(),
    Context :: z:context(),
    KeywordRscId :: m_rsc:resource_id(),
    Reason :: term().
find_keyword(ConceptID, Context) ->
    case get_concept(ConceptID, Context) of
        {ok, #{
            <<"keyword_id">> := RscId
        }} when is_integer(RscId) ->
            {ok, RscId};
        {ok, #{
            <<"wikidata_id">> := WikidataID,
            <<"wikidata_ancestor_ids">> := Ancestors
        }} ->
            case find_nearest_ancestor(Ancestors, Context) of
                {ok, RscId} ->
                    {ok, RscId};
                {error, enoent} ->
                    find_nearest_descendent(WikidataID, Context)
            end;
        {error, _} = Error ->
            Error
    end.

find_nearest_ancestor(undefined, _Context) ->
    {error, enoent};
find_nearest_ancestor([], _Context) ->
    {error, enoent};
find_nearest_ancestor(Ancestors, Context) ->
    case z_db:q1("
        select keyword_id
        from wikiconcept
        where keyword_id is not null
          and wikidata_id = any($1::character varying[])
        order by level desc
        limit 1",
        [ Ancestors ],
        Context)
    of
        undefined ->
            {error, enoent};
        RscId ->
            {ok, RscId}
    end.

find_nearest_descendent(WikidataID, Context) ->
    case z_db:q1("
        select keyword_id
        from wikiconcept
        where keyword_id is not null
          and wikidata_ancestor_ids && $1::character varying[]
        order by level asc
        limit 1",
        [ [ WikidataID ] ],
        Context)
    of
        undefined ->
            {error, enoent};
        RscId ->
            {ok, RscId}
    end.


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
get_concept(ID, Context) when is_list(ID) ->
    get_concept(unicode:characters_to_binary(ID, utf8), Context);
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

%% @doc Find all concepts with a level directly below the given concept and which
%% have the given concept as an ascendant.
-spec find_descendant(Concept, Context) -> {ok, Concepts} | {error, Reason} when
    Concept :: binary() | string(),
    Context :: z:context(),
    Concepts :: list(map()),
    Reason :: term().
find_descendant(Concept, Context) ->
    find_descendant(Concept, 1, 1000, Context).

%% @doc Find concepts with a level directly below the given concept and which
%% have the given concept as an ascendant.
-spec find_descendant(Concept, Offset, Limit, Context) -> {ok, Concepts} | {error, Reason} when
    Concept :: binary() | string(),
    Offset :: pos_integer(),
    Limit :: pos_integer(),
    Context :: z:context(),
    Concepts :: list(map()),
    Reason :: term().
find_descendant(Concept, Offset, Limit, Context) ->
    Concept1 = z_string:trim(unicode:characters_to_binary(Concept)),
    case find_1(Concept1, 1, 1, Context) of
        {ok, [C]} ->
            #{
                <<"level">> := Level,
                <<"wikidata_id">> := WikidataID
            } = C,
            z_db:qmap_props("
                select *
                from wikiconcept
                where wikidata_ancestor_ids && $1
                  and level = $2
                order by display_name, wikidata_id
                offset $3 limit $4",
                [ [ WikidataID ], Level+1, Offset-1, Limit ],
                Context);
        {ok, []} ->
            {ok, []};
        {error, _} = Error ->
            Error
    end.

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

%% @doc Count concepts using the same criteria as find/4, without pagination.
-spec count(Text, Context) -> non_neg_integer() when
    Text :: binary() | string(),
    Context :: z:context().
count(Text, Context) ->
    Text1 = z_string:trim(unicode:characters_to_binary(Text)),
    {_Select, From, Args, _Order} = find_query(Text1),
    z_db:q1(["select count(*) ", From], Args, Context).

find_1(Text, Offset, Limit, Context) ->
    {Select, From, Args, Order} = find_query(Text),
    N = length(Args),
    SQL = iolist_to_binary([
        "select ", Select, " ", From, " order by ", Order,
        " offset $", integer_to_binary(N+1), " limit $", integer_to_binary(N+2)
    ]),
    z_db:qmap_props(SQL, Args ++ [Offset-1, Limit], Context).

find_query(<<"https://", _/binary>> = URL) ->
    {"c.*", "from wikiconcept c where c.wikidata = $1 or c.openalex = $1 or c.wikipedia = $1",
     [URL], "c.wikidata_id"};
find_query(Text) ->
    case words(z_string:normalize(Text)) of
        [] ->
            {"c.*", "from wikiconcept c", [], "c.display_name, c.id"};
        [ <<"q", C1, C2, _/binary>> ] when ?is_digit(C1), ?is_digit(C2) ->
            {"c.*", "from wikiconcept c where c.wikidata_id = $1", [Text], "c.wikidata_id"};
        [ <<"c", C1, C2, _/binary>> ] when ?is_digit(C1), ?is_digit(C2) ->
            {"c.*", "from wikiconcept c where c.openalex = $1",
             [<<"https://openalex.org/", Text/binary>>], "c.openalex"};
        Words ->
            Where = [
                ["n.name like concat('%', $", integer_to_binary(N+1), "::text, '%')"]
                || N <- lists:seq(1, length(Words))
            ],
            From = iolist_to_binary([
                "from wikiconcept c join (
                    select n.id, max(public.similarity(n.name, $1::text)) as score
                    from wikiconcept_name n where ", lists:join(" and ", Where), "
                    group by n.id
                ) s on c.id = s.id"
            ]),
            {"c.*, s.score", From, [Text | Words], "s.score desc, c.id"}
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

%% @doc Fetch all wiki concepts and store them into our database table for
%% easy searching and connection to keyword resources.
load(Context) ->
    load(fetch_all(Context), Context).

load({ok, {Cs, done}}, Context) ->
    load_1(Cs, Context);
load({ok, {Cs, Cursor}}, Context) ->
    load_1(Cs, Context),
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

%% @doc Create missing tables and migrate legacy properties without losing concept links.
-spec install(Context) -> ok when
    Context :: z:context().
install(Context) ->
    ok = z_db:transaction(fun install_schema/1, Context),
    z_db:flush(Context),
    ok.

install_schema(Context) ->
    % pg_trgm is shared by all sites in the database; never drop it on reinstall.
    [] = z_db:q("create extension if not exists pg_trgm with schema public", Context),
    case z_db:table_exists(wikiconcept, Context) of
        true -> migrate_props_json(Context);
        false -> create_concepts(Context)
    end,
    case z_db:table_exists(wikiconcept_name, Context) of
        true -> ok;
        false -> create_names(Context)
    end,
    [] = z_db:q("create index if not exists fki_wikiconcept_keyword_id on wikiconcept (keyword_id)", Context),
    [] = z_db:q("create index if not exists wikiconcept_mag_key on wikiconcept (mag)", Context),
    [] = z_db:q("create index if not exists wikiconcept_wikipedia_key on wikiconcept (wikipedia)", Context),
    [] = z_db:q("
        create index if not exists wikiconcept_name_name on wikiconcept_name
        using gin (name public.gin_trgm_ops)", Context),
    ok.

migrate_props_json(Context) ->
    case z_db:column(wikiconcept, props_json, Context) of
        {ok, #column_def{ type = <<"jsonb">> }} ->
            ok;
        {ok, #column_def{ type = <<"bytea">> }} ->
            % The old schema stored UTF-8 JSON in bytea via DB_PROPS_JSON.
            % Invalid data aborts the transaction instead of discarding it.
            [] = z_db:q("
                alter table wikiconcept alter column props_json type jsonb
                using convert_from(props_json, 'UTF8')::jsonb", Context),
            ok
    end.

create_concepts(Context) ->
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

            props_json jsonb,

            keyword_id integer,

            modified timestamp with time zone NOT NULL DEFAULT now(),

            constraint wikiconcept_pkey primary key (id),
            constraint wikiconcept_openalex_key unique (openalex),
            constraint wikiconcept_wikidata_key unique (wikidata),
            constraint wikiconcept_wikidata_id_key unique (wikidata_id),
            constraint fk_wikiconcept_keyword_id foreign key (keyword_id)
                references rsc (id) on update cascade on delete set null
        )", Context),
    ok.

create_names(Context) ->
    [] = z_db:q("
        create table wikiconcept_name (
            id serial not null,
            name character varying(1000),
            constraint fk_wikiconcept_name_id foreign key (id)
                references wikiconcept (id) on update cascade on delete cascade
        )", Context),
    ok.

%% @doc Schedule the initial import only for an empty table. Use the admin reimport
%% action to refresh existing concepts without removing keyword connections.
-spec install_data(Context) -> ok when
    Context :: z:context().
install_data(Context) ->
    case z_db:q1("select exists(select 1 from wikiconcept)", Context) of
        true ->
            ok;
        false ->
            {ok, _} = z_pivot_rsc:insert_task(?MODULE, load, <<>>, Context),
            ok
    end.
