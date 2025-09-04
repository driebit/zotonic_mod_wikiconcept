{% extends "admin_base.tpl" %}

{% block title %}{_ WikiConcepts Overview _}{% endblock %}

{% block content %}
    <div class="admin-header">
        <h2>WikiConcepts Overview</h2>
        <p>This is a simple list of all imported wikiconcepts, for a total of: <strong>{{ m.wikiconcept.total }}</strong></p>
        <p>To search, navigate, link and import concepts use either the 'Advanced' section in any keyword's edit page or to the dialog for new media items.</p>

    </div>

    {% with m.wikiconcept.list_all as concepts %}
        <table class="table table-striped">
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
            {% for concept in concepts %}
                <tr data-href="">
                    <td>
                        {% if concept.keyword_id %}
                            <a href="{% url admin_edit_rsc id=concept.keyword_id %}">
                                #{{ concept.keyword_id }}
                            </a>
                        {% else %}
                            <em>{_ None _}</em>
                        {% endif %}
                    </td>
                    <td>{{ concept.display_name }}</td>
                    <td>{{ concept.modified|date:_"d M Y, H:i" }}</td>
                    <td>
                        {% if concept.wikipedia %}
                            <a href="{{ concept.wikipedia|escape }}">
                                {{ concept.wikipedia|escape }}
                            </a>
                        {% else %}
                            <em>{_ Unknown _}</em>
                        {% endif %}
                    </td>
                    <td>
                        {% if concept.wikidata %}
                            <a href="{{ concept.wikidata|escape }}">
                                {{ concept.wikidata|escape }}
                            </a>
                        {% else %}
                            <em>{_ Unknown _}</em>
                        {% endif %}
                    </td>
                    <td>
                        {% if concept.openalex %}
                            <a href="{{ concept.openalex|escape }}">
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
                        {_ None _}
                    </td>
                </tr>
            {% endfor %}
            </tbody>
        </table>
        <ul class="pagination pagination-centered">
            {% with q.page|default:1 as current_page %}
                {% for nr in (current_page - 3)|max:1|range:(current_page + 9) %}
                    <li {% if nr == current_page %}class="active"{% endif %}>
                        <a href="?page={{ nr }}">{{ nr }}</a>
                    </li>
                {% endfor %}
            {% endwith %}
        </ul>
    {% endwith %}
{% endblock %}
