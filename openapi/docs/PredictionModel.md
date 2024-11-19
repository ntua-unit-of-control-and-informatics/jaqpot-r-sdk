# openapi::PredictionModel


## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **integer** | Unique identifier for the prediction model | [optional] 
**dependentFeatures** | [**array[Feature]**](Feature.md) | List of dependent features for the model | 
**independentFeatures** | [**array[Feature]**](Feature.md) | List of independent features for the model | 
**type** | [**ModelType**](ModelType.md) |  | [Enum: ] 
**rawModel** | **character** | Raw model data in serialized format | 
**rawPreprocessor** | **character** | Raw preprocessor data in serialized format | [optional] 
**doas** | [**array[PredictionDoa]**](PredictionDoa.md) | List of Domain of Applicability (DoA) configurations | [optional] 
**selectedFeatures** | **array[character]** | List of feature names selected for the model | [optional] 
**task** | [**ModelTask**](ModelTask.md) |  | [Enum: ] 
**featurizers** | [**array[Transformer]**](Transformer.md) | List of featurizer configurations applied to the model | [optional] 
**preprocessors** | [**array[Transformer]**](Transformer.md) | List of preprocessor configurations applied to the model | [optional] 
**torchConfig** | [**map(AnyType)**](AnyType.md) | Torch configuration settings, optional | [optional] 
**rPbpkOdeSolver** | **character** |  | [optional] 
**legacyAdditionalInfo** | [**map(AnyType)**](AnyType.md) | Legacy additional information settings, optional | [optional] 
**legacyPredictionService** | **character** | Legacy prediction service information, if available | [optional] 


