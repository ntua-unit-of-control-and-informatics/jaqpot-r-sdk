#!/bin/bash

set -e  # Exit immediately if a command exits with a non-zero status.

JAQPOT_API_PATH=../jaqpot-api
TEMP_DIR=$(mktemp -d)
TARGET_DIR=./jaqpotpy/api/openapi

# Install openapi-generator-cli if not already installed
if ! command -v openapi-generator-cli &> /dev/null
then
    echo "openapi-generator could not be found, installing..."
    npm install @openapitools/openapi-generator-cli -g
fi

# Update Jaqpot API
cd $JAQPOT_API_PATH
git pull
cd -
# Generate the OpenAPI client in a temporary directory
openapi-generator-cli generate \
    -i $JAQPOT_API_PATH/src/main/resources/openapi.yaml \
    -g r \
    -o ./openapi \
#
## Ensure the target directory exists
#mkdir -p $TARGET_DIR
#
## Move only the necessary files
#cp -r $TEMP_DIR/jaqpotpy/api/openapi/* $TARGET_DIR/
#
## Clean up
#rm -rf $TEMP_DIR

echo "OpenAPI client generated successfully in $TARGET_DIR"
