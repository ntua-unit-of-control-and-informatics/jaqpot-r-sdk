# openapi::MulticlassClassificationScores


## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**labels** | **array[character]** |  | [optional] 
**yName** | **character** |  | 
**folds** | **integer** |  | [optional] 
**accuracy** | **numeric** |  | [optional] 
**balancedAccuracy** | **numeric** |  | [optional] 
**precision** | **array[numeric]** |  | [optional] [Max. items: 1000] 
**recall** | **array[numeric]** |  | [optional] [Max. items: 1000] 
**f1Score** | **array[numeric]** |  | [optional] [Max. items: 1000] 
**jaccard** | **array[numeric]** |  | [optional] [Max. items: 1000] 
**matthewsCorrCoef** | **numeric** |  | [optional] 
**confusionMatrix** | [**array[array[numeric]]**](array.md) |  | [optional] [Max. items: 100] 


