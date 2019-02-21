# vfbr 0.3.3

* Update GMR-GAL4 id searches forthcoming changes to primary label of ids (#13)
* Add `vfb_download_url_from_vfbid()` function to get URLs to download image stacks
  for any VFB ids
* doc improvements

# vfbr 0.3.2

* Fix GMR-GAL4 to VFB id translation in vfb_nblast (#10)

# vfbr 0.3.1

* Allow vfb_nblast to accept and return VFB ids (#9)

# vfbr 0.3

* add vfb_neo4j_query
* fix vfb_nblast, vfb_solr_query URLs
* export gmr_stack_urls_for_ids function
* pkgdown site

# vfbr 0.2.1


* Additional arguments for vfb_synonym_query
* New function vfb_autocomplete_query to replicate the VFB website
* vfb_solr_query can accept all solr query terms as additional arguments
  (including repeated terms)
* Documentation fixes/improvements

# vfbr 0.2

* New function vfb_synonym_query
* fix vfb_fromvfbids for multiple ids (#5)
* fix problem downloading stacks on Windows (#4)
* fix vfb_nblast failure (#2)
* dev: add tests
