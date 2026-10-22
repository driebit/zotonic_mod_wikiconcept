%% @author Driebit BV
%% @copyright 2024 Driebit BV
%% @doc Use Wikipedia concepts as keywords
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

-module(mod_wikiconcept).
-moduledoc(#{
    zotonic_keywords => [
        "reference", "site_administrator", "backend_developer", "module",
        "search_and_discovery", "api_and_integration", "semantic_web"
    ]
}).
-moduledoc("
Import Wikidata concepts from OpenAlex and connect them to Zotonic keyword
resources. Imported concepts and multilingual search names are stored locally;
searching the local catalogue does not require an OpenAlex request.

## Setup and installation

Enable `mod_wikiconcept`, which depends on `mod_admin`. Installation creates
`wikiconcept` and `wikiconcept_name`, their constraints and search indexes.
The database needs `pg_trgm` in the `public` schema. The installer uses
`create extension if not exists pg_trgm with schema public`; the database
user must be able to create the extension when absent and use that schema.
There are no module-specific configuration keys.

Schema version 2 preserves existing concepts, search names and keyword links.
It converts the legacy `props_json` bytea column containing UTF-8 JSON to
jsonb in place. Invalid JSON aborts the schema transaction without discarding
data. Reinstallation never drops the tables or the shared extension.

An OpenAlex import task is scheduled only when the concept table is empty.
Use the Reimport button on the admin status page to refresh an existing or
partially imported catalogue. Imports update matching concepts and their
names without clearing keyword connections; they do not prune concepts
missing from the upstream response.

## Administration and search

The WikiConcepts entry under the admin Modules menu lists imported concepts,
with live search by name, Wikidata ID, OpenAlex ID or URL and numbered pagination.
The overview and concept dialog require `use mod_admin`. Click a row or its name
button to view the summary, source links, ancestors, and available title and
summary languages. Keyword and source links remain directly clickable.

For an unconnected concept, expand **Create a keyword** in the dialog and select
the keyword category or an allowed subcategory. Creation checks admin access
and resource insert permissions, publishes the keyword, and updates the dialog
and table with its edit link. An existing connection is reused.

Keyword creation keeps nonempty translations in the site's editable languages.
When none match, available English text is copied to the site's default language,
not the current interface language. Titles fall back to the concept's display
name or identifier if necessary, so newly created keywords always have a title.

The supplied keyword-editing UI also supports finding, connecting and
disconnecting concepts. The media dialog supports creating keywords from concepts.
When resources are merged, links to the losing keyword move to the winner.

Interface translation catalogs accompany the module for Arabic, German, Spanish,
French, Indonesian, Italian, Dutch, Polish, Portuguese, Brazilian Portuguese,
Russian, Swedish, Turkish, Ukrainian and Simplified Chinese. English is the source
language. Interface translations are separate from imported concept translations.

Two searches are exposed through `m.search`:

```django
{% with m.search.wikiconcept::%{ text: \"biology\" } as result %}
    {% for concept in result %}{{ concept.display_name|escape }}{% endfor %}
{% endwith %}
```

`wikiconcept` takes `text` and searches local names or identifiers. It returns an
exact total for the standard pager; use `m.search.paged.wikiconcept` for pagination.
`wikiconcept_descendant` takes `concept` and finds concepts at the next level
whose ancestor list contains that concept. The results are concept maps,
not resource IDs. Escape imported text when rendering it.

The model's read paths and the search observers do not apply resource ACL
filtering. Connecting and disconnecting through `m_wikiconcept` require an
editable keyword resource; creating a keyword uses `m_rsc` insert permissions.
The Reimport postback itself has no explicit ACL check in its event handler.
See `m_wikiconcept` for the model paths and direct Erlang APIs.
").

-mod_title("Wikipedia concepts").
-mod_description("Wikipedia concepts as keywords").
-mod_author("Driebit BV").
-mod_depends([ mod_admin ]).
-mod_schema(2).

-author("Driebit BV").

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").

-export([
    observe_admin_menu/3,
    observe_search_query/2,
    observe_rsc_merge/2,

    event/2,

    manage_schema/2,
    manage_data/2
    ]).

observe_admin_menu(#admin_menu{}, Acc, _Context) ->
    [ #menu_item{
        id = admin_wikiconcepts,
        parent = admin_modules,
        label = "WikiConcepts",
        url = {admin_wikiconcepts, []},
        visiblecheck = {acl, use, mod_admin}
    } | Acc ].

observe_search_query(#search_query{
        name = <<"wikiconcept">>,
        offsetlimit = {Offset, Limit},
        args = Args
    }, Context) ->
    Text = z_search:lookup_qarg_value(<<"text">>, Args, <<>>),
    case m_wikiconcept:find(Text, Offset, Limit, Context) of
        {ok, Concepts} ->
            #search_result{
                result = Concepts,
                total = m_wikiconcept:count(Text, Context),
                is_total_estimated = false
            };
        {error, _} ->
            #search_result{
            }
    end;
observe_search_query(#search_query{
        name = <<"wikiconcept_descendant">>,
        offsetlimit = {Offset, Limit},
        args = Args
    }, Context) ->
    Concept = z_search:lookup_qarg_value(<<"concept">>, Args, <<>>),
    case m_wikiconcept:find_descendant(Concept, Offset, Limit, Context) of
        {ok, Concepts} ->
            #search_result{
                result = Concepts
            };
        {error, _} ->
            #search_result{
            }
    end;
observe_search_query(#search_query{ }, _Context) ->
    undefined.


%% @doc Handle the merger of two keywords.
observe_rsc_merge(#rsc_merge{ winner_id = WinnerId, loser_id = LoserId }, Context) ->
    m_wikiconcept:rsc_merge(WinnerId, LoserId, Context).

event(#postback_notify{
        message = <<"feedback">>,
        trigger = <<"wikiconcept-search">>,
        target = TargetId
    }, Context) ->
    Text = z_convert:to_binary(z_context:get_q(<<"triggervalue">>, Context)),
    Context1 = z_render:update(
        "wikiconcept-search-result",
        #render{
            template = "_wikiconcept_search_result.tpl",
            vars = [
                {text, z_string:trim(Text)},
                {target, TargetId}
            ]
        },
        Context),
    z_render:wire({remove_class, [{target, TargetId}, {class, "loading"}]}, Context1);
event(#submit{message={create_keyword, Args}}, Context) ->
    ConceptId = proplists:get_value(wikidata_id, Args),
    Category = m_rsc:rid(z_context:get_q(<<"category_id">>, Context), Context),
    Result = case z_acl:is_allowed(use, mod_admin, Context) andalso is_integer(Category) of
        true -> m_wikiconcept:insert_keyword(ConceptId, Category, Context);
        false -> {error, eacces}
    end,
    case Result of
        {ok, KeywordId} ->
            {ok, Concept} = m_wikiconcept:get_concept(ConceptId, Context),
            Target = <<"wikiconcept-keyword-", (integer_to_binary(maps:get(<<"id">>, Concept)))/binary>>,
            Context1 = z_render:update(Target, #render{
                template = "_wikiconcept_keyword_link.tpl",
                vars = [{keyword_id, KeywordId}]
            }, Context),
            z_render:dialog(?__("Concept information", Context), "_dialog_wikiconcept_info.tpl",
                [{concept_id, ConceptId}], Context1);
        {error, _} ->
            z_render:growl_error(?__("Sorry, the keyword could not be created in this category.", Context), Context)
    end;
event(#postback{message={reimport, _}}, Context) ->
    case z_pivot_rsc:insert_task(m_wikiconcept, load, <<>>, Context) of
        {ok, _} ->
            z_render:growl(?__("A task to (re)import wikiconcept has started. Check the admin status page for progress.", Context), Context);
        _ ->
            z_render:growl_error(?__("Sorry, a reimport couldn't be started.", Context), Context)
    end.

manage_schema(_Version, Context) ->
    m_wikiconcept:install(Context).

manage_data(_Version, Context) ->
    m_wikiconcept:install_data(Context).
