# zotonic_mod_wikiconcept
Zotonic module to use Wikidata concepts from OpenAlex as keywords.

## Admin overview

Enable `mod_wikiconcept` and open **Modules → WikiConcepts** in the admin.
Access requires permission to use `mod_admin`.

The overview provides live search by name, Wikidata ID, OpenAlex ID or URL,
with numbered pagination. Click a row or its name button to view the concept's
summary, source links, ancestors, and available title and summary languages.
Existing keyword and source links remain directly clickable.

If no keyword is connected, expand **Create a keyword**, select a keyword category,
and click **Create keyword** to import its title and summary and connect it.
The selector includes keyword subcategories allowed by your insert permissions.
New keywords are published. The dialog and table then show a link to the keyword's
edit page. Repeated submissions reuse an already connected keyword.

Creation retains nonempty translations matching the site's editable languages.
If none match, available English text is copied to the site's default language,
independently of the current admin interface language.
For titles, a missing English translation falls back to the concept's display
name, or its identifier if no name is available, so new keywords always have a title.

## Interface translations

English is the source language. Complete catalogs are provided for Arabic (`ar`),
German (`de`), Spanish (`es`), French (`fr`), Indonesian (`id`), Italian (`it`),
Dutch (`nl`), Polish (`pl`), Portuguese (`pt`), Brazilian Portuguese (`pt-br`),
Russian (`ru`), Swedish (`sv`), Turkish (`tr`), Ukrainian (`uk`), and Simplified
Chinese (`zh`). These languages also have translations in Zotonic core.

The `.po` sources and compiled `.mo` catalogs are in `priv/translations`;
the source catalog is `priv/translations/template/mod_wikiconcept.pot`.
Interface translations are separate from the concept translations imported
from OpenAlex.

## PostgreSQL Trigram search

This module uses the `pg_trgm` extension for trigram similarity search of concept names.

The extension will be created in the `public` schema, ensure that the Zotonic
database user has access to that schema.

Note that `pg_trgm` is “trusted”, that is, it can be installed by non-superusers who
have CREATE privilege on the current database.

The query used to create the extension:

```sql
create extension if not exists pg_trgm with schema public
```

## Installation and reinstallation

Installation preserves existing concepts, search names and keyword connections.
Schema version 2 migrates the legacy `props_json` column from UTF-8 JSON stored
in `bytea` to `jsonb` in place. Invalid JSON aborts the schema transaction without
discarding data; repair the affected data before retrying. The shared `pg_trgm`
extension is never dropped.

An initial OpenAlex import is scheduled only when the concept table is empty.
To refresh existing concepts or resume an incomplete import, use the explicit
Reimport button on the admin status page. Reimport updates matching concepts
without clearing their keyword connections.

Earlier versions rebuilt both tables on reinstall to change the `props_json`
column type. This also removed local keyword connections; the in-place migration
replaces that behavior.

## Tests

`test/m_wikiconcept_keyword_tests.erl` uses EUnit and meck to check keyword
category selection, denied insertion, reuse of existing connections, translated
titles, and fallback to the default site language. It requires no database.

`test/m_wikiconcept_install_tests.erl` exercises fresh and repeated installation,
legacy JSON conversion (including Unicode and NULL), preservation of keyword
connections and search names, exact search totals for pagination, foreign-key behavior, import scheduling, and
rollback on invalid legacy data.

Run it against a **disposable local PostgreSQL database**. Set
`WIKICONCEPT_TEST_PORT` to its port; the test connects to `postgres` on
`127.0.0.1` as user `postgres` with an empty password. It creates and removes its
own schema and may install `pg_trgm` in `public`. Without that environment
variable, the database test is skipped. The test requires EUnit, epgsql and meck.
