{% if m.acl.use.mod_admin %}
    {% with m.wikiconcept.id[concept_id|default:q.wikidata_id] as concept %}
        {% if concept %}
            <h3>{{ concept.display_name|escape }}</h3>
            {% if concept.summary %}
                <p>{{ concept.summary|escape|linebreaksbr }}</p>
            {% endif %}

            <dl class="dl-horizontal">
                <dt>{_ Wikidata ID _}</dt>
                <dd>{{ concept.wikidata_id|escape }}</dd>

                <dt>{_ Level _}</dt>
                <dd>{{ concept.level|escape }}</dd>

                <dt>{_ Title languages _}</dt>
                <dd>
                    {% for code in concept.title|trans_filter_filled|trans_languages|sort %}
                        {{ m.translation.all_languages[code].name_en|default:code|escape }} ({{ code|escape }}){% if not forloop.last %}, {% endif %}
                    {% empty %}
                        <em>{_ None _}</em>
                    {% endfor %}
                </dd>

                <dt>{_ Summary languages _}</dt>
                <dd>
                    {% for code in concept.summary|trans_filter_filled|trans_languages|sort %}
                        {{ m.translation.all_languages[code].name_en|default:code|escape }} ({{ code|escape }}){% if not forloop.last %}, {% endif %}
                    {% empty %}
                        <em>{_ None _}</em>
                    {% endfor %}
                </dd>

                <dt>{_ Keyword _}</dt>
                <dd>
                    {% if concept.keyword_id %}
                        <a href="{% url admin_edit_rsc id=concept.keyword_id %}">
                            {{ concept.keyword_id.title }} (#{{ concept.keyword_id }})
                        </a>
                    {% else %}
                        <em>{_ None _}</em>
                    {% endif %}
                </dd>

                <dt>{_ Last modified _}</dt>
                <dd>{{ concept.modified|date:_"d M Y, H:i" }}</dd>

                {% if concept.wikipedia %}
                    <dt>Wikipedia</dt>
                    <dd><a href="{{ concept.wikipedia|sanitize_url|escape }}">{{ concept.wikipedia|escape }}</a></dd>
                {% endif %}
                {% if concept.wikidata %}
                    <dt>Wikidata</dt>
                    <dd><a href="{{ concept.wikidata|sanitize_url|escape }}">{{ concept.wikidata|escape }}</a></dd>
                {% endif %}
                {% if concept.openalex %}
                    <dt>OpenAlex</dt>
                    <dd><a href="{{ concept.openalex|sanitize_url|escape }}">{{ concept.openalex|escape }}</a></dd>
                {% endif %}

                {% if concept.wikidata_ancestor_ids %}
                    <dt>{_ Ancestor concepts _}</dt>
                    <dd>
                        <ul class="list-unstyled">
                            {% for ancestor_id in concept.wikidata_ancestor_ids %}
                                <li>{{ m.wikiconcept.id[ancestor_id].display_name|default:ancestor_id|escape }}</li>
                            {% endfor %}
                        </ul>
                    </dd>
                {% endif %}
            </dl>

            {% if not concept.keyword_id %}
                {% wire id=#create_keyword type="submit"
                        postback={create_keyword wikidata_id=concept.wikidata_id}
                        delegate="mod_wikiconcept"
                %}
                <details>
                    <summary class="admin-padding">{_ Create a keyword _}</summary>
                    <form id="{{ #create_keyword }}" method="post" action="postback" class="admin-padding">
                        <p class="text-muted">{_ Create a keyword using this concept’s title and summary. Choose a keyword category below. The new keyword will be published and connected to this concept. _}</p>
                        <div class="form-group">
                            <label for="{{ #category }}">{_ Keyword category _}</label>
                            <select id="{{ #category }}" name="category_id" class="form-control" required>
                                {% for category in m.category.keyword.tree_flat %}
                                    {% if m.acl.insert[category.id.name|as_atom] %}
                                        <option value="{{ category.id }}">{{ category.indent }}{{ category.id.title }}</option>
                                    {% endif %}
                                {% endfor %}
                            </select>
                        </div>
                        <button type="submit" class="btn btn-primary">{_ Create keyword _}</button>
                    </form>
                </details>
            {% endif %}
        {% else %}
            <p>{_ Concept not found. _}</p>
        {% endif %}
    {% endwith %}
{% else %}
    <p>{_ You do not have permission to view this information. _}</p>
{% endif %}

<div class="modal-footer">
    {% button class="btn btn-default" text=_"Close" action={dialog_close} %}
</div>
