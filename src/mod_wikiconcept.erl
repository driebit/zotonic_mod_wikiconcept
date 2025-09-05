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

-mod_title("Wikipedia concepts").
-mod_description("Wikipedia concepts as keywords").
-mod_author("Driebit BV").
-mod_depends([ mod_admin ]).
-mod_schema(1).

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
                result = Concepts
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
