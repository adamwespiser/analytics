#!/bin/bash
print_test_header () {
  echo "\n"
  echo "**************************************************"
  echo $1
  echo "**************************************************"
}

API_KEY=ABCDEFGHIJKLMNOPQRSTUVWXYZ123456789

print_test_header "localhost/page"
curl -v \
  -H "Accept: application/json" \
  -H "Content-type: application/json" \
  -H "Authorization: $API_KEY" \
  -d '{"pgSessionTrackingId":1, "pgUrlFilePath": "test23"}' \
  localhost:8080/page 2>&1
echo ""

print_test_header "localhost/event"
curl -v \
  -H "Accept: application/json" \
  -H "Content-type: application/json" \
  -H "Authorization: $API_KEY" \
  -d '{"evSessionTrackingId": 1, "evCategory": "cat1", "evLabel": "label1"}' \
  localhost:8080/event 2>&1
echo ""

print_test_header "localhost/session"
curl -v \
  -H "Accept: application/json" \
  -H "Content-type: application/json" \
  -H "Authorization: $API_KEY" \
  localhost:8080/session 2>&1
echo ""
