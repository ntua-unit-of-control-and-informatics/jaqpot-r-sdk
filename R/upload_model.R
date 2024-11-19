# somehow import
source('../openapi/R')

read('Give me api key')
read('Give me api secret')

api_client = ApiClient$new(default_headers = list(`X-Api-Key` = env$JAQPOT_API_KEY, `X-Api-Secret` = env$JAQPOT_API_SECRET))
model_api <- ModelApi$new(api_client = api_client)
# Example usage to create a model
response <- model_api$CreateModel(rawModel = rawModel)
print(response)


