#!/bin/sh
coffee --watch --lint --output lib/ src/[a-zA-Z]*.coffee test/[a-zA-Z]*.coffee
