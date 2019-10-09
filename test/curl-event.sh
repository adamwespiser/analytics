#!/bin/bash
ABOUT = """
This script needs curl, jq to run
"""
API_KEY=ABCDEFGHIJKLMNOPQRSTUVWXYZ123456789

print_test_header () {
  echo ''
  echo "**************************************************"
  echo $1
  echo "**************************************************"
}

print_test_header "localhost/session"
sessionId=$(\
  curl -H "Accept: application/json"\
  -H "Content-type: application/json"\
  -H "Authorization: $API_KEY" \
  localhost:8080/session | jq ".userSessionId" | sed s/\"//g)
echo $sessionId

print_test_header "localhost/page"
curl -v \
  -H "Accept: application/json" \
  -H "Content-type: application/json" \
  -H "Authorization: $API_KEY" \
  -d "{\"pgUserSessionId\": \"${sessionId}\", \"pgUrlFilePath\": \"test23\"}" \
  localhost:8080/page 2>&1
echo ""

print_test_header "localhost/event"
curl -v \
  -H "Accept: application/json" \
  -H "Content-type: application/json" \
  -H "Authorization: $API_KEY" \
  -d "{\"evUserSessionId\": \"${sessionId}\", \"evCategory\": \"cat1\", \"evLabel\": \"label1\"}" \
  localhost:8080/event 2>&1
echo ""
