# zotonic_mod_wikiconcept
Zotonic module to use Wikidata concepts from OpenAlex as keywords.

## PostgreSQL Trigram search

This module uses the `pg_trgm` extension for fulltext search of concepts.

The extension will be created in the `public` schema, ensure that the Zotonic
database user has access to that schema.

Note that `pg_trgm` is “trusted”, that is, it can be installed by non-superusers who
have CREATE privilege on the current database.

The query used to create the extension:

```sql
create extension if not exists pg_trgm with schema public
```
