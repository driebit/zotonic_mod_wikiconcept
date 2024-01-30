{% with m.search.wikiconcept::%{ text: text } as result %}
    {% if result %}
        <div id="wikiconcept-results">
            {% include "_wikiconcept_search_result_items.tpl" result=result %}
        </div>
    {% else %}
        <span class="text-muted">{_ Nothing found. _}</span>
    {% endif %}

    {% lazy action={moreresults
            result=result
            target="wikiconcept-results"
            template="_wikiconcept_search_result_items.tpl"
            is_result_render
            visible
        }
     %}
{% endwith %}

{# If query on exact concept, then search for concepts directly below the concept as well #}
{% if text|match:"^\\s*([Qq][0-9]+$|https:\\/\\/)" %}
    {% with m.search.wikiconcept_descendant::%{ concept: text } as result %}
        {% if result %}
            <p><b>{_ Descending concepts _}</b></p>

            <div id="wikiconcept-descendants">
                {% include "_wikiconcept_search_result_items.tpl" result=result %}
            </div>

            {% lazy action={moreresults
                    result=result
                    target="wikiconcept-descendants"
                    template="_wikiconcept_search_result_items.tpl"
                    is_result_render
                    visible
                }
             %}
        {% endif %}
    {% endwith %}
{% endif %}

{% javascript %}
    $("#wikiconcept-results").closest(".modal").scrollTop(0);
{% endjavascript %}

