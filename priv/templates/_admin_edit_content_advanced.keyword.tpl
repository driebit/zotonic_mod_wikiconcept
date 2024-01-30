{% extends "_admin_edit_content_advanced.tpl" %}

{% block edit_advanced_extra %}
    <p class="help-block">
        {_ Associate one or more Wikidata concepts with this keyword. This is useful for mapping keywords when exchanging data between sites. _}
    </p>

    {% live topic=[ "bridge", "origin", "model", "rsc", "event", id, "wikiconcept" ]
            template="_wikiconcept_connected.tpl"
            id=id
    %}

    <p>
        <button class="btn btn-default" id="connect-wikidata-concept">
            {_ Connect Wikidata concept _}
        </button>
    </p>
    {% wire id="connect-wikidata-concept"
            action={dialog_open
                title=_"Connect a Wikidata concept"
                template="_dialog_wikiconcept_connect.tpl"
                id=id
            }
    %}
{% endblock %}
