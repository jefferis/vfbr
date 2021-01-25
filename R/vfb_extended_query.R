#' Find canononical ontology term using VFB SOLR to query synonyms
#'
#' @param query A character vector containing one or more queries - see details.
#'   Wildcards (\code{?*}) can be used, and have a similar meaning to their use
#'   in the shell. Queries are case insensitive.
#' @param exact Whether to do an exact match (i.e. the query must match the
#'   whole string, the default) or to allow partial matches against synonyms.
#'   See \bold{Query details} and examples.
#' @param quote Whether to quote the query so that individual terms must all be
#'   matched in the order given or to \bold{OR} each term in the query. The
#'   default \code{quote=NA} will quote when \code{exact=TRUE} and the query
#'   does not contain wildcards. See \bold{Query details} and \bold{Examples}.
#' @param searchfields Character vector specifying fields to search. The default
#'   searches both \bold{synonym}s and the canonical term \bold{label} (since
#'   the canonical term is not included in the synonym list).
#' @param fields The fields to return (defaults to 'short_form label synonym').
#'   Setting \code{fields=""} implies all fields (but note that not all results
#'   may have the same fields - see discussion in details)
#' @param verbose Whether to print messages to the screen.
#' @param ... Additional arguments passed to \code{\link{vfb_solr_query}}. You
#'   can use this e.g. to set the number of returned \code{rows}.
#' @return A data.frame containing one or more result rows, ordered according to
#'   the solr result score, with attributres
#' @section Query details: The \code{exact} and \code{quote} arguments have a
#'   profound effect on how SOLR carries out searches. Setting both arguments to
#'   \code{FALSE} gives the least specific search and you may want to do this if
#'   you do not find any results with an initial query using the default values.
#'
#'   In general SOLR will break any text in fields like synonym into separate
#'   tokens based on whitespace, hypens etc, so the value :
#'
#'   \code{inter-antennal lobe tract}
#'
#'   will map onto 4 tokens.
#'
#'   \code{inter antennal lobe tract}
#'
#'   This means that a search with query \code{"inter antennal lobe tract",
#'   exact=F} will be successful (note missing hyphen).
#'
#'   When \code{exact=TRUE}, the values will not be tokenised and the query must
#'   match the whole field.
#'
#'   The \code{quote} argument modifies the query. By default the query is
#'   tokenised as just described for fields in the database, tokens can be
#'   matched in any order and only one token must match (i.e. the are
#'   \bold{OR}ed together).  When \code{quote=TRUE}, the query must match in the
#'   order given (this is achieved by wrapping it in double quotes before
#'   passing to SOLR). For example
#'
#'   \code{vfb_synonym_query("antennal lobe tract", exact=F, quote=T)}
#'
#'   returns 2 results at the time of writing whereas
#'
#'   \code{vfb_synonym_query("antennal lobe tract", exact=F, quote=F)}
#'
#'   returns 194 results.
#'
#'   See
#'   \url{https://github.com/EBISPOT/OLS/blob/master/ols-solr/src/main/solr-conf/ontology/conf/schema.xml}
#'    for definition of the of the \code{synonym_s} exact match field.
#'
#'
#' @details Note that data.frame that is returned may contain a list in the
#'   synonym column because there will likely be multiple synonyms for a given
#'   query.
#'
#'   When \code{x} has length > 1 i.e. multiple query terms, then multiple calls
#'   to \code{\link{vfb_solr_query}} are wrapped in a \code{\link[base]{sapply}}
#'   statement. You can pass arguments to \code{sapply} in \code{...} such as
#'   \code{simplify=FALSE} if you wish. By default the return value will be a
#'   matrix with rows that you can index by field name and columns that you can
#'   index by queries. However the form of this is not guaranteed if you ask for
#'   fields that are present only for some of the results.
#' @seealso \code{\link{vfb_solr_query}}, \code{\link{vfb_autocomplete_query}}
#' @export
#' @examples
#' vfb_synonym_query("SOG")
#' # anything with a synonym that includes SOG
#' vfb_synonym_query("SOG", exact=FALSE)
#'
#' \donttest{# query for PAM exactly
#' vfb_synonym_query("PAM")
#' # any term with a synonym containing PAM
#' vfb_synonym_query("PAM", exact = FALSE)
#' }
#' # terms with synonyms that start with "PAM-"
#' vfb_synonym_query("PAM-*")
#' # nb this doesn't return any results when exact=FALSE because solr matches
#' # against tokenised strings (i.e. strings that have broken on spaces and
#' # other non word characters such as dashes and underscores).
#' vfb_synonym_query("PAM-*", exact = FALSE)
#' \donttest{# However if you quote the query you will get results
#' vfb_synonym_query("PAM-*", exact = FALSE, quote=TRUE)
#' }
#'
#' \donttest{
#' # Search for MBON-01 to MBON-22 (sprintf is used to 0 pad the numbers)
#' vfb_synonym_query(sprintf("MBON-%02d",1:22))
#'
#' # You can also use a wild card search, which is much faster since it only
#' # makes a single solr query but the hits are returned in an arbitrary order.
#' mbondf=vfb_synonym_query("MBON-??")
#' # then you can pick out your preferred synonym
#' # note that we use the glob2rx function to convert solr's simple shell-style
#' # wild card syntax to a regular expression
#' mbondf$aso=sapply(mbondf$synonym, function(x) grep(glob2rx("MBON-??"), x, value=TRUE))
#' }
vfb_synonym_query<-function(query, exact=TRUE, quote=NA, searchfields=c("synonym","label"),
                            fields='short_form label synonym',
                            verbose=interactive(), ...){
  if(length(query)>1) return(sapply(query, vfb_synonym_query, exact=exact,
                                    searchfields=searchfields, fields=fields,
                                    quote=quote, verbose=verbose, ...))
  searchfields=match.arg(searchfields, several.ok = TRUE)
  if(is.na(quote)){
    # quote=F unless exact=T and query does not contain wildcards
    quote=exact && !grepl("[*?]", query)
  }
  if(quote) query=sprintf('"%s"', query)
  if(exact) searchfields=paste0(searchfields, "_s")
  query=paste0(searchfields,":",query, collapse=" ")
  res=vfb_solr_query(filterquery=query, fields=fields, ...)

  if(verbose){
    nfound=attr(res,'numFound')
    if(nrow(res) < nfound)
      message("There are ", nfound, " results. ",
              "Return them all by setting rows=Inf!")
    if(nfound==0 && exact)
      message("No results found! You could try setting exact=FALSE.")
  }
  res
}

#' Replicate the VFB site's autocomplete query
#'
#' @details This is a rather non-selective query that emphasises exact matches and terms
#' in the BRAINNAME defining key ontology components by returning them in the
#' first rows of the result list.
#' @inheritParams vfb_synonym_query
#' @export
#' @seealso \code{\link{vfb_synonym_query}}
vfb_autocomplete_query<-function(query, ...){
  munged_query=paste0("*", gsub(" ","?", query), "*")
  vfb_solr_query(hl="true",
  fields="short_form,label,synonym,id,facets_annotation",
  filter_query=c("-facets_annotation:(Deprecated)","shortform_autosuggest:VFB* OR shortform_autosuggest:FBbt_*"),
  hl.simple.pre="<b>",
  bq=sprintf('shortform_autosuggest:VFB*^110.0 shortform_autosuggest:FBbt*^100.0 label_s:\"\"^2 synonym_s:\"\" short_form:FBbt_00003982^2 facets_annotation:Deprecated^0.001',
             query, query),
  query=munged_query,
  defType="edismax",
  hl.simple.post="</b>:",
  qf="label^100 synonym^100 label_autosuggest_ws label_autosuggest_e label_autosuggest synonym_autosuggest_ws synonym_autosuggest shortform_autosuggest",
  hl.fl="label_autosuggest",
  hl.fl="label",
  hl.fl="synonym_autosuggest",
  hl.fl="synonym", ...)
}
