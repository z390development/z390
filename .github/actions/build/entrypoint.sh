#!/bin/sh -l
git config --global --add safe.directory ${GITHUB_WORKSPACE}
echo "Running ${GITHUB_WORKSPACE}/build.sh"
${GITHUB_WORKSPACE}/build.sh