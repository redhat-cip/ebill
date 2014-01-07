%% Default values
-define(EBILL_SERVER_TCP_PORT, 8080).
-define(EBILL_SERVER_MAX_CONN, 100).
-define(EBILL_STORAGE_TCP_PORT, 8090).
-define(EBILL_STORAGE_MAX_CONN, 100).
-define(EBILL_COOKIE, 'Y0UR34LLYN33DT0CH4NG3TH1SF0RS3CUR1TY').
-define(EBILL_STORAGE_DB_HOST, "localhost").
-define(EBILL_STORAGE_DB_PORT, 5984).
-define(EBILL_STORAGE_DB_NAME, "ebill").

%% Configuration paths
-define(EBILL_CONFIG_PATH, [
  "/etc/ebill.conf",
  "/Library/Application Support/ebill/ebill.conf",
  "~/Library/Application Support/ebill/ebill.conf",
  "~/.ebill/ebill.conf",
  "ebill.conf"
]).

%% Configuration record
-record(ebillconfig, {
  tcp_server_port = ?EBILL_SERVER_TCP_PORT,
  max_server_conn = ?EBILL_SERVER_MAX_CONN,
  tcp_storage_port = ?EBILL_STORAGE_TCP_PORT,
  max_storage_conn = ?EBILL_STORAGE_MAX_CONN,
  cookie = ?EBILL_COOKIE,
  db_storage_host = ?EBILL_STORAGE_DB_HOST,
  db_storage_port = ?EBILL_STORAGE_DB_PORT,
  db_storage_name = ?EBILL_STORAGE_DB_NAME
}).
