#!/bin/bash
ABOUT="""
This script needs curl, jq to run
"""
API_KEY=ABCDEFGHIJKLMNOPQRSTUVWXYZ123456789
ORIGIN_URL=localhost
UA_STRING="Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/77.0.3865.90 Safari/537.36"

print_test_header () {
  echo ''
  echo "**************************************************"
  echo $1
  echo "**************************************************"
}

print_test_header "localhost/session"
sessionId=$(\
  curl -v \
  -H "Access-Control-Request-Headers: content-type" \
  -H "Access-Control-Request-Method: GET" \
  -H "Origin: $ORIGIN_URL" \
  -H "Sec-Fetch-Mode: cors" \
  -H "User-Agent: $UA_STRING" \
  http://localhost:8080/session/?auth=$API_KEY)
echo $sessionId
echo '...'
