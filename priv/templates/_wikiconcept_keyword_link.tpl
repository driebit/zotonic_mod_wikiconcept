{% if m.acl.use.mod_admin %}
    <a href="{% url admin_edit_rsc id=keyword_id %}">#{{ keyword_id|escape }}</a>
{% endif %}
