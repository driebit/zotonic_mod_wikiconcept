{# Tab title for the wikiconcept tab #}
{% if (not tabs_enabled or "wikiconcept"|member:tabs_enabled) and not "wikiconcept"|member:tabs_disabled %}
{% if m.acl.is_allowed.insert.keyword %}
{% if (intent == 'create' and (not cat or m.category[cat].is_a.keyword))
   or (intent == 'connect' and m.predicate.is_valid_object_category[predicate].keyword)
%}
    <li><a data-toggle="tab" href="#{{ tab }}-wikiconcept">{_ Wikidata Concepts _}</a></li>
{% endif %}
{% endif %}
{% endif %}
