#!/bin/bash
#
# facets-preview container entrypoint.
#
# Logging: when FP_LOG_DIR is set (and writable) all R output goes to a
# per-session file there instead of the container's stdout. This matters on the
# VM, where every session is its own container and the Docker json-file driver
# has no size cap -- unbounded stdout fills /var/lib/docker and eventually stops
# the daemon. With FP_LOG_DIR unset the behavior is unchanged (stdout), so local
# and interactive use is unaffected.

set -u

FP_LOG_DIR="${FP_LOG_DIR:-}"
FP_USER_ID="${FP_USER_ID:-unknown}"
FP_SESSION_ID="${FP_SESSION_ID:-$(hostname)}"

R_CMD="facets_preview_config_file = '/usr/bin/facets-preview/fp_config.json' ; options(shiny.port = 3838, shiny.host = '0.0.0.0', shiny.launch.browser = FALSE) ; library(facetsPreview); facetsPreview::launch_application()"

if [ -n "${FP_LOG_DIR}" ] && mkdir -p "${FP_LOG_DIR}" 2>/dev/null && [ -w "${FP_LOG_DIR}" ]; then
  LOG_FILE="${FP_LOG_DIR}/fp_${FP_USER_ID}_${FP_SESSION_ID}_$(date -u +%Y%m%dT%H%M%SZ).log"
  {
    echo "=== facets-preview session log ==="
    echo "started      : $(date -u +%Y-%m-%dT%H:%M:%SZ)"
    echo "version      : ${FACETS_PREVIEW_VERSION:-unknown}"
    echo "user         : ${FP_USER_ID}"
    echo "session      : ${FP_SESSION_ID}"
    echo "mode         : ${FP_MODE:-local}"
    echo "workdir      : ${FP_USER_WORKDIR:-unset}"
    echo "access level : ${FP_ACCESS_LEVEL:-unset}"
    echo "=================================="
  } >> "${LOG_FILE}" 2>&1
  echo "[FP] logging to ${LOG_FILE}"   # one line on stdout so docker logs points at the file
  exec Rscript -e "${R_CMD}" >> "${LOG_FILE}" 2>&1
fi

exec Rscript -e "${R_CMD}"
