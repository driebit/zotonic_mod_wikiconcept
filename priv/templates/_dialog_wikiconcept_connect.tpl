{# Select wikidata concepts to connect with the keyword #}
<div class="tab-pane">
    <p>{_ Find a Wikiconcept to associate with this keyword. _}</p>

    <div class="form-group">
        <input class="form-control do_autofocus" type="text" name="qtext" value="" id="wikiconcept-search" placeholder="{_ Type to search... _}">
    </div>

    <div id="wikiconcept-search-result" class="do_feedback"
        data-feedback="trigger: 'wikiconcept-search', delegate: 'mod_wikiconcept'">

        {% include "_wikiconcept_search_result.tpl" %}
    </div>

    {% javascript %}
        $("#wikiconcept-search-result")
            .on('click', '.wikiconcept__ancestors a, .wikiconcept__descendant a', function(e) {
                e.preventDefault();

                const wiki_id = $(this).attr('href').substr(1);
                $('#wikiconcept-search').val(wiki_id).change();
            });

        $("#wikiconcept-search-result").on('click', '.wikiconcept__title a', function(e) {
            e.preventDefault();

            const wc = $(this).closest('.wikiconcept');
            const wcid = wc.attr('data-wikiconcept');

            cotonic.broker
                .call(
                    "bridge/origin/model/wikiconcept/post/connect",
                    { wikidata: wcid, id: {{ id }} })
                .then(function(e) {
                    if (e.payload.status == "ok") {
                        const kwid = e.payload.result;
                        wc.toggleClass("thumbnail-connected");
                    } else {
                        z_growl_add("{_ Could not connect the concept. _}", false, "error");
                    }
                });

            wc.effect("highlight");
        });
    {% endjavascript %}
</div>
