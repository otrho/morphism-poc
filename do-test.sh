#!/usr/bin/env bash

test_dir=$(dirname ${0})

diff -u "${test_dir}/expected.txt" <("${test_dir}/interp.janet" "${test_dir}/test.ism")
