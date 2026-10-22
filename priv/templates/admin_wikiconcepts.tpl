{% extends "admin_base.tpl" %}

{% block title %}{_ WikiConcepts Overview _}{% endblock %}

{% block content %}
    <div class="admin-header">
        <h2>{_ WikiConcepts Overview _}</h2>
        <p>{_ Total imported concepts: _} <strong>{{ m.wikiconcept.total }}</strong></p>
        <p>{_ To link or import concepts, use the Advanced section on a keyword’s edit page or the new media dialog. _}</p>
    </div>

    {% wire id=#search_form type="submit" action={script script=""} %}
    <form id="{{ #search_form }}" role="search" method="get" action="{% url admin_wikiconcepts %}">
        <div class="form-group">
            <label for="{{ #search }}">{_ Search concepts _}</label>
            <input id="{{ #search }}" type="search" name="text" class="form-control"
                   value="{{ q.text|escape }}" autocomplete="off"
                   placeholder="{_ Search by name, Wikidata ID, OpenAlex ID or URL _}">
            <input type="hidden" name="page" value="1">
        </div>
    </form>

    <div id="wikiconcepts-results" class="do_feedback" aria-live="polite"
         data-feedback='{ "trigger": "{{ #search_form }}", "template": "_admin_wikiconcepts_results.tpl" }'>
        {% include "_admin_wikiconcepts_results.tpl" %}
    </div>

    {% wire name="wikiconcept-info"
            action={dialog_open title=_"Concept information" template="_dialog_wikiconcept_info.tpl"}
    %}
    {% javascript %}
        document.getElementById('wikiconcepts-results').addEventListener('click', function(event) {
            const row = event.target.closest('tr[data-wikiconcept]');
            if (!row || event.target.closest('a')) {
                return;
            }
            z_event('wikiconcept-info', { wikidata_id: row.dataset.wikiconcept });
        });
    {% endjavascript %}
{% endblock %}
