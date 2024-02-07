{% for c in result %}
<div class="panel wikiconcept" data-wikiconcept="{{ c.wikidata_id|escape }}">
    <div class="row">
        <div class="col-md-8">
            <h4 class="wikiconcept__title">
                <a href="#{{ c.wikidata_id|escape }}" title="{_ Click to view concepts below _}">
                    {{ c.title|escape }}
                </a>

                <button class="btn btn-xs btn-default">
                    {_ Select _}
                </button>
            </h4>

            {% if c.summary %}
                <p>
                    {{ c.summary|escape }}
                </p>
            {% endif %}

            <p>
                {% if c.wikipedia %}
                    <a class="btn btn-default" href="{{ c.wikipedia|escape }}" target="_blank" rel="noopener" title="{_ View at Wikipedia _}">
                        {_ Wikipedia _} <i class="fa fa-external-link text-muted" aria-hidden="true"></i>
                    </a>
                {% endif %}
                <a class="btn btn-default" href="{{ c.wikidata|escape }}" target="_blank" rel="noopener" title="{_ View at Wikidata _}">
                    {_ Wikidata _} <i class="fa fa-external-link text-muted" aria-hidden="true"></i>
                </a>
                <a class="btn btn-default" href="{{ c.openalex|escape }}" target="_blank" rel="noopener" title="{_ View at OpenAlex _}">
                    {_ OpenAlex _} <i class="fa fa-external-link text-muted" aria-hidden="true"></i>
                </a>
            </p>
        </div>
        <div class="col-md-4">
            {% if c.wikidata_ancestor_ids %}
                <p class="text-muted">{_ Ascending concepts _}</p>
                <ul class="wikiconcept__ancestors">
                {% for anc in c.wikidata_ancestor_ids %}
                    <li>
                        <a href="#{{ anc|escape }}">{{ m.wikiconcept.id[anc].title|escape }}</a>
                    </li>
                {% endfor %}
                </ul>
            {% endif %}
        </div>
    </div>
</div>
{% endfor %}
