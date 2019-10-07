#!/bin/bash
echo "localhost/page"
curl \
  -H "Accept: application/json" \
  -H "Content-type: application/json" \
  -H "Authorization: ABCDEFGHIJKLMNOPQRSTUVWXYZ123456789" \
  -d '{"pgSessionTrackingID":"test1", "pgUrlFilePath": "test23"}' \
  localhost:8080/page
echo ""

echo "localhost/event"
curl -v \
  -H "Accept: application/json" \
  -H "Content-type: application/json" \
  -H "Authorization: ABCDEFGHIJKLMNOPQRSTUVWXYZ123456789" \
  -d '{"evSessionTrackingId":"test1", "evCategory": "cat1", "evLabel":"label1"}' \
  localhost:8080/event 2>&1
echo ""

