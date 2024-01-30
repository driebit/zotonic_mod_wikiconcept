{# Panel for the wikiconcept tab #}
{% if (not tabs_enabled or "wikiconcept"|member:tabs_enabled) and not "wikiconcept"|member:tabs_disabled %}
{% if m.acl.is_allowed.insert.keyword %}
{% if (intent == 'create' and (not cat or m.category[cat].is_a.keyword))
   or (intent == 'connect' and m.predicate.is_valid_object_category[predicate].keyword)
%}
<div class="tab-pane" id="{{ tab }}-wikiconcept">
    {% if intent == 'connect' %}
        <p class="help-block">{_ Create and connect a keyword from the OpenAlex list of Wikipedia concepts. _}</p>
    {% else %}
        <p class="help-block">{_ Create a keyword from the OpenAlex list of Wikipedia concepts. _}</p>
    {% endif %}

    <div class="form-group">
        <input class="form-control do_autofocus" type="text" name="qtext" value="" id="wikiconcept-search" placeholder="{_ Type to search... _}">
    </div>

    <div id="wikiconcept-search-result" class="do_feedback"
        data-feedback="trigger: 'wikiconcept-search', delegate: 'mod_wikiconcept'">

        {% include "_wikiconcept_search_result.tpl" %}
    </div>

    {% if intent == "connect" or intent == "select" %}
        {% wire name="wikiconcept_find"
            action={postback
                delegate=`mod_admin`
                postback={admin_connect_select
                    intent=intent
                    id=id
                    subject_id=subject_id
                    object_id=object_id
                    predicate=predicate
                    callback=callback
                    language=language
                    action=action
                    actions=actions
                    autoclose=autoclose
                    is_connect_toggle=not is_zmedia
                }
            }
        %}
    {% else %}
        {% wire name="wikiconcept_find"
            action={postback
                delegate=`mod_admin`
                postback={admin_rsc_redirect
                    redirect=redirect
                    language=language
                    autoclose=autoclose
                }
            }
        %}
    {% endif %}

    {% javascript %}
        $("#wikiconcept-search-result")
            .on('click', '.wikiconcept__ancestors a, .wikiconcept__descendant a', function(e) {
                e.preventDefault();

                const wiki_id = $(this).attr('href').substr(1);
                $('#wikiconcept-search').val(wiki_id).change();
            });

        $("#wikiconcept-search-result").on('click', '.wikiconcept__title a', function(e) {
            e.preventDefault();

            const wc = $(this).closest('.wikiconcept');
            const wcid = wc.attr('data-wikiconcept');

            cotonic.broker
                .call("bridge/origin/model/wikiconcept/post/insert/keyword", { wikidata: wcid })
                .then(function(e) {
                    if (e.payload.status == "ok") {
                        const kwid = e.payload.result;
                        z_event('wikiconcept_find', {
                            select_id: kwid,
                            is_connected: $(this).hasClass('thumbnail-connected')
                        });
                        wc.toggleClass("thumbnail-connected");
                    } else {
                        z_growl_add("{_ Could not create the keyword. _}", false, "error");
                    }
                });

            wc.effect("highlight");
        });
    {% endjavascript %}
</div>
{% endif %}
{% endif %}
{% endif %}
