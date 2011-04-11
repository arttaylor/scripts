#include <stdio.h>
#include <malloc.h>
#include <string.h>
#include <stdlib.h>

#include "httpd.h"
#include "http_config.h"
#include "http_core.h"
#include "http_log.h"
#include "http_protocol.h"

typedef struct _rewrite_rule {
  char *m_pszUrl;
  int m_iCurrentCounter;
  char ** m_rgpszRedirs;
  struct _rewrite_rule * m_pNext;
} rewrite_rule;


int URL_LEN = 256;
int LINE_LEN = 256;
int REDIR_LEN = 256;

static rewrite_rule * pHead = NULL;

void dump_rewrite_rules () {
  rewrite_rule * prr;
  char * redir;
  int i;

  for ( prr = pHead; 
	prr != NULL; 
	prr = prr->m_pNext ) {
    fprintf(stderr, "url:= \'%s\'\n", prr->m_pszUrl);
    for ( i = 0; i < 100; ++i ) {
      if ( NULL == redir || strcmp(redir, prr->m_rgpszRedirs[i])) {
	redir = prr->m_rgpszRedirs[i];
	fprintf(stderr, "  redir:= \'%s\', pct:=%d\n", redir, i);
      }
    }
  }
}

void free_rewrite_rules () {
  while ( pHead ) {
    rewrite_rule * pNext = pHead->m_pNext;
    /* todo: free strdup()'d redirs */
    free(pHead->m_rgpszRedirs);
    free(pHead);
    pHead = pNext;
  }
}

rewrite_rule * find_rewrite_rule ( const char * url ) {
  int bFound = 0;
  rewrite_rule * prr = NULL;

  for ( prr = pHead; 
	NULL != prr; 
	prr = prr->m_pNext ) {
    if ( !strcmp(prr->m_pszUrl, url) ) {
      
      bFound = 1;
      break;
    }
  }

  if ( !bFound ) 
    prr = NULL;

  return prr;
}

rewrite_rule * find_or_create_rewrite_rule ( const char * url ) {
  int bFound = 0, i;
  rewrite_rule * prr = find_rewrite_rule(url);

  if ( !prr ) {
    prr = (rewrite_rule*) malloc(sizeof(rewrite_rule));
    /* todo: check prr != NULL */
    prr->m_pszUrl = strdup(url);
    prr->m_rgpszRedirs = (char**)malloc(sizeof(char*)*100);
    /* todo: check != NULL */
      for ( i = 0; i < 100; ++i ) 
	prr->m_rgpszRedirs[i] = NULL;
    prr->m_iCurrentCounter = 0;
    prr->m_pNext = pHead;
    pHead = prr;
  }
  return prr;
}

void read_config_file(char * fileName) {
  FILE *fp;
  char line[LINE_LEN];
  char url[URL_LEN], *redir;
  int pct;
  rewrite_rule * prr = NULL;
  int i = 0;
  
  free_rewrite_rules();

  fp = fopen(fileName, "r");

  if ( NULL == fp ) {
    fprintf(stderr, "couldn't open file!");
    exit(-1);
  }

  /* todo: clean this up, maybe using lex */
  while ( NULL != fgets(line, LINE_LEN, fp) ) {
    if ( strlen(line) == 0 )
      continue;
    /* comment */
    if ( line[0] != '[' )
      continue;
    redir = (char*) malloc(REDIR_LEN);
    /* todo: check != NULL */
    sscanf(line, "[ %s | %d | %s ]\n", url, &pct, redir);

    prr = find_or_create_rewrite_rule(url);

    for ( i = prr->m_iCurrentCounter;
	  i < 100 && i < (prr->m_iCurrentCounter + pct);
	  ++i ) {
      prr->m_rgpszRedirs[i] = redir;
    }
    prr->m_iCurrentCounter += pct;
  }

  fclose(fp);
}

char * rewrite_url_given_gate ( const char * url, int gate ) {
  rewrite_rule * prr = find_rewrite_rule(url);
  if ( prr ) {
    if ( prr->m_rgpszRedirs[gate] ) 
      return prr->m_rgpszRedirs[gate];
  }
  return (char*)url;
}

char * rewrite_url ( const char * url ) {
  rewrite_rule * prr = find_rewrite_rule(url);
  if ( prr ) {
    int gate = (rand()%100);
    if ( prr->m_rgpszRedirs[gate] ) 
      return prr->m_rgpszRedirs[gate];
  }
  return (char*)url;
}



module mod_gating_module;

char * COOKIE_PREFIX = "mod_gating_";

typedef struct {
  char * config_file_path;
} mod_gating_config;

/* stolen from oreilly */
table * util_parse_cookie(request_rec * r ) {
  const char * data = ap_table_get(r->headers_in, "Cookie");
  table * cookies;
  const char * pair;
  if (!data) return NULL;
  
  cookies = ap_make_table(r->pool, 4);
  while ( *data && (pair = ap_getword(r->pool, &data, ';'))) {
    const char * name, * value;
    if ( *data == ' ' ) ++data;
    name = ap_getword(r->pool, &pair, '=');
    while ( *pair && (value = ap_getword(r->pool, &pair, '&'))) {
      ap_unescape_url((char*) value);
      ap_table_add(cookies, name, value);
    }
  }
  return cookies;
}

char * get_cookie_name(request_rec *r) {
  char * cookie_name = (char*)ap_palloc(r->pool, strlen(COOKIE_PREFIX) + strlen(r->uri));
  /* todo: check for null */
  
  strcpy(cookie_name, COOKIE_PREFIX);
  strcat(cookie_name, r->uri);
  return cookie_name;
}

char * get_gate_from_cookie(request_rec *r) {
  char * cookie_name, * cookie_val;
  table * cookies;
  cookies = util_parse_cookie(r);

  cookie_name = get_cookie_name(r);

  if ( cookies ) {
    cookie_val = (char *)ap_table_get(cookies, cookie_name);
  } else
    cookie_val = NULL;

  return cookie_val;
}

void set_gate_cookie(request_rec *r, const char * uri, char * gate ) {
  char * cookie_name = get_cookie_name (r);
  /* todo: fix this */
  char cookie [512];
  /* todo: fix the expire date */
  sprintf(cookie,"%s=%s; expires=Mon, 08-Jan-2007 12:00:00 GMT; path=/", uri, gate);
  ap_table_add(r->headers_out, "Set-cookie", cookie);
}

int mod_gating_handler (request_rec *r) {
  mod_gating_config * config;
  config = (mod_gating_config*)
    ap_get_module_config(r->per_dir_config, &mod_gating_module);

  /* todo: do this on a timed stat() basis */
  read_config_file(config->config_file_path);

  if ( find_rewrite_rule(r->uri) ) {
    char * gate;
    gate = get_gate_from_cookie(r);
    if ( NULL == gate ) 
      gate = rewrite_url(r->uri);

    set_gate_cookie(r, r->uri, gate);

    r->uri = (char*) ap_pstrdup(r->pool, gate);
  }
  
  return DECLINED;
}

static void * mod_gating_create_config(pool * p, char * path ) {
  mod_gating_config * config = 
    (mod_gating_config*) ap_pcalloc(p, sizeof(mod_gating_config));
  config->config_file_path="/mod_gating.conf";
  return (void*) config;
}

static const char * mod_gating_config_file_path(cmd_parms * parms,
						void * mconfig,
						char * file_path) {
  mod_gating_config * config = (mod_gating_config *) mconfig;
  config->config_file_path = (char*) ap_pstrdup(parms->pool, file_path);
  return NULL;
}

static command_rec mod_gating_cmds[] = {
  {
    "ConfigFilePath",
    mod_gating_config_file_path,
    NULL,
    OR_ALL,
    TAKE1,
    "Where we find the configuration file."
  },
  {NULL}
};

module mod_gating_module = {
  STANDARD_MODULE_STUFF,
  NULL,                     /* initializer */
  mod_gating_create_config, /* dir config creator */
  NULL,                     /* dir merger --- default is to override */
  NULL,                     /* server config */
  NULL,                     /* merge server config */
  mod_gating_cmds,          /* command table */
  NULL,                     /* handlers */
  mod_gating_handler,       /* filename translation */
  NULL,                     /* check_user_id */
  NULL,                     /* check auth */
  NULL,                     /* check access */
  NULL,                     /* type_checker */
  NULL,                     /* fixups */
  NULL,                     /* logger */
  NULL                      /* header parser */
};

