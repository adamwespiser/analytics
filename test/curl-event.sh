#!/bin/bash
ABOUT="""
This script needs curl, jq to run
"""
API_KEY=ABCDEFGHIJKLMNOPQRSTUVWXYZ123456789
localhost=127.0.0.1

print_test_header () {
  echo ''
  echo "**************************************************"
  echo $1
  echo "**************************************************"
}

print_test_header "localhost/session"
sessionId=$(\
  curl -v -H "Accept: application/json"\
  -H "Content-type: application/json"\
  -H "Access-Control-Request-Method: GET" \
  -H "Access-Control-Request-Headers: X-Requested-With" \
  $localhost:8080/session/?auth=$API_KEY | jq ".userSessionId" | sed s/\"//g)
echo $sessionId

print_test_header "localhost/page"
curl -v \
  -H "Content-type: application/json"\
  -H "Access-Control-Request-Method: POST" \
  -H "Access-Control-Request-Headers: X-Requested-With" \
  -d "{\"pgUserSessionId\": \"${sessionId}\", \"pgUrlFilePath\": \"test23\"}" \
  $localhost:8080/page/?auth=$API_KEY 2>&1
echo ""

print_test_header "localhost/event"
curl -v \
  -H "Content-type: application/json"\
  -H "Access-Control-Request-Method: POST" \
  -H "Access-Control-Request-Headers: X-Requested-With" \
  -d "{\"evUserSessionId\": \"${sessionId}\", \"evCategory\": \"cat1\", \"evLabel\": \"label1\"}" \
  $localhost:8080/event/?auth=$API_KEY 2>&1
echo ""
