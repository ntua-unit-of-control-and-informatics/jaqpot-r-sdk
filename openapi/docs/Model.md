# openapi::Model


## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **integer** |  | [optional] 
**name** | **character** |  | [Max. length: 255] [Min. length: 3] 
**description** | **character** |  | [optional] [Max. length: 50000] [Min. length: 3] 
**type** | [**ModelType**](ModelType.md) |  | [Enum: ] 
**jaqpotpyVersion** | **character** |  | 
**doas** | [**array[Doa]**](Doa.md) |  | [optional] [Max. items: 50] 
**libraries** | [**array[Library]**](Library.md) |  | [Max. items: 1000] 
**dependentFeatures** | [**array[Feature]**](Feature.md) |  | [Max. items: 1000] 
**independentFeatures** | [**array[Feature]**](Feature.md) |  | [Max. items: 1000] 
**sharedWithOrganizations** | [**array[Organization]**](Organization.md) |  | [optional] 
**visibility** | [**ModelVisibility**](ModelVisibility.md) |  | [Enum: ] 
**task** | [**ModelTask**](ModelTask.md) |  | [Enum: ] 
**torchConfig** | [**map(AnyType)**](AnyType.md) |  | [optional] [Max. items: 20] 
**preprocessors** | [**array[Transformer]**](Transformer.md) |  | [optional] [Max. items: 50] 
**featurizers** | [**array[Transformer]**](Transformer.md) |  | [optional] [Max. items: 50] 
**rawPreprocessor** | **character** | A base64 representation of the raw preprocessor. | [optional] [Max. length: 10000000] 
**rawModel** | **character** | A base64 representation of the raw model. | [Max. length: 10000000] 
**creator** | [**User**](User.md) |  | [optional] 
**canEdit** | **character** | If the current user can edit the model | [optional] 
**isAdmin** | **character** |  | [optional] 
**selectedFeatures** | **array[character]** |  | [optional] [Max. items: 1000] 
**tags** | **character** |  | [optional] [Max. length: 1000] 
**legacyPredictionService** | **character** |  | [optional] 
**scores** | [**ModelScores**](Model_scores.md) |  | [optional] 
**rPbpkConfig** | [**RPbpkConfig**](RPbpkConfig.md) |  | [optional] 
**createdAt** | **character** | The date and time when the feature was created. | [optional] 
**updatedAt** | **character** | The date and time when the feature was last updated. | [optional] 


