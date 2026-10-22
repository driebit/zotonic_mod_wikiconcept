{% if m.acl.use.mod_admin %}
    {% with m.search.paged.wikiconcept::%{ text: q.text, page: q.page|to_integer|default:1|max:1, pagelen: 50 } as result %}
        <table class="table table-striped table-hover">
            <thead>
                <tr>
                    <th width="5%">{_ Keyword _}</th>
                    <th width="10%">{_ Name _}</th>
                    <th width="10%">{_ Last modified _}</th>
                    <th width="25%">{_ Wikipedia URL _}</th>
                    <th width="25%">{_ Wikidata URL _}</th>
                    <th width="25%">{_ OpenAlex URL _}</th>
                </tr>
            </thead>

            <tbody>
            {% for concept in result %}
                <tr class="clickable" data-wikiconcept="{{ concept.wikidata_id|escape }}">
                    <td id="wikiconcept-keyword-{{ concept.id }}">
                        {% if concept.keyword_id %}
                            <a href="{% url admin_edit_rsc id=concept.keyword_id %}">
                                #{{ concept.keyword_id }}
                            </a>
                        {% else %}
                            <em>{_ None _}</em>
                        {% endif %}
                    </td>
                    <td>
                        <button type="button" class="btn btn-link btn-inline" aria-haspopup="dialog">
                            {{ concept.display_name|escape }}
                        </button>
                    </td>
                    <td>{{ concept.modified|date:_"d M Y, H:i" }}</td>
                    <td>
                        {% if concept.wikipedia %}
                            <a href="{{ concept.wikipedia|sanitize_url|escape }}">
                                {{ concept.wikipedia|escape }}
                            </a>
                        {% else %}
                            <em>{_ Unknown _}</em>
                        {% endif %}
                    </td>
                    <td>
                        {% if concept.wikidata %}
                            <a href="{{ concept.wikidata|sanitize_url|escape }}">
                                {{ concept.wikidata|escape }}
                            </a>
                        {% else %}
                            <em>{_ Unknown _}</em>
                        {% endif %}
                    </td>
                    <td>
                        {% if concept.openalex %}
                            <a href="{{ concept.openalex|sanitize_url|escape }}">
                                {{ concept.openalex|escape }}
                            </a>
                        {% else %}
                            <em>{_ Unknown _}</em>
                        {% endif %}
                    </td>
                </tr>
            {% empty %}
                <tr>
                    <td colspan="6">
                        {_ No concepts found. _}
                    </td>
                </tr>
            {% endfor %}
            </tbody>
        </table>
        {% pager result=result dispatch="admin_wikiconcepts" text=q.text hide_single_page %}
    {% endwith %}
{% endif %}
