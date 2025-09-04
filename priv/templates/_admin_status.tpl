<div class="form-group">
    <div>
        {% button
            class="btn btn-default"
            text=_"Reimport wikiconcepts"
            postback={reimport}
            delegate='mod_wikiconcept'
        %}
        <span class="help-block">
            {_ Trigger a reimport of wikiconcepts from the OpenAlex API. _}
        </span>
    </div>
</div>

