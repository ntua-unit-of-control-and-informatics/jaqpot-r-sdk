# somehow import
source('../openapi/R')

model_api <- ModelApi$new()
# Example usage to create a model
response <- model_api$CreateModel(rawModel = rawModel)
print(response)
