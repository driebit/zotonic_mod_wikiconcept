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
