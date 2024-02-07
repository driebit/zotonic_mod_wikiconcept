{% comment %}{% print m.wikiconcept.connected[id] %}{% endcomment %}
<ol>
    {% for concept in m.wikiconcept.connected[id] %}
        {% with forloop.counter as nr %}
        <li>
            <b>{{ concept.title|escape }}</b>
            <button id="{{ #d.nr }}" class="btn btn-default btn-xs" title="{_ Disconnect this concept from the keyword. _}">{_ Disconnect _}</button>
            <br>
            <p>
                {% if concept.summary as summary %}{{ summary|escape }}<br>{% endif %}
                <a href="{{ concept.wikidata|escape }}" target="_blank">
                    <small>
                        <tt>{{ concept.wikidata|escape }}</tt>
                        <i class="fa fa-external-link" aria-hidden="true"></i>
                    </small>
                </a>
                &nbsp; <button class="btn btn-default btn-xs" id="{{ #s.nr }}">{_ Set as URI _}</button>
            </p>
        </li>
        {% wire id=#d.nr
                action={publish
                    topic="bridge/origin/model/wikiconcept/post/disconnect"
                    id=id
                    wikidata=concept.wikidata_id
                }
        %}
        {% wire id=#s.nr
                action={set_value
                    target="field-uri"
                    value=concept.wikidata
                }
        %}
        {% endwith %}
    {% endfor %}
</ol>
