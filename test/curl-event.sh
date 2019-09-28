#!/bin/bash
echo "localhost/page"
curl \
  -X POST \
  -H "content-type: application/json" \
  -H "Authorization: ABCDEFGHIJKLMNOPQRSTUVWXYZ123456789" \
  -d '{"pgSessionTrackingID":"test1", "pgUrlFilePath": "test23"}' \
  localhost:8080/page
echo ""

echo "localhost/event"
curl \
  -X POST \
  -H "content-type: application/json" \
  -H "Authorization: ABCDEFGHIJKLMNOPQRSTUVWXYZ123456789" \
  -d '{"evSessionTrackingId":"test1", "evCategory": "cat1", "evLabel":"label1"}' \
  localhost:8080/event
echo ""

