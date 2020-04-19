module NoiseParameters exposing (..)

type alias NoiseParameters =
    { seed: Int
    , baseRoughness: Float
    , numLayers: Int
    , roughness: Float
    , persistance: Float
    , strength: Float
    , minValue: Float
    }


emptyNoiseParams: NoiseParameters
emptyNoiseParams = { seed = 42, baseRoughness = 3, numLayers = 4, roughness = 0.5, persistance = 0.7, strength = 0.35, minValue = 0.5 }

type UpdateParams
    = UpdateBaseRoughness String
    | UpdateRoughness String
    | UpdatePersistance String
    | UpdateNumLayers String
    | UpdateSeed String
    | UpdateStrength String
    | UpdateMinValue String

updateParameter: NoiseParameters -> UpdateParams -> NoiseParameters
updateParameter prevParams updateMsg =
    case updateMsg of 
        UpdateBaseRoughness baseRoughness ->
            { prevParams | baseRoughness = String.toFloat baseRoughness |> Maybe.withDefault prevParams.baseRoughness }
        UpdateRoughness roughness ->
            { prevParams | roughness = String.toFloat roughness |> Maybe.withDefault prevParams.roughness }
        UpdatePersistance persistance ->
            { prevParams | persistance = String.toFloat persistance |> Maybe.withDefault prevParams.persistance }
        UpdateNumLayers numLayers ->
            { prevParams | numLayers = String.toInt numLayers |> Maybe.withDefault prevParams.numLayers }
        UpdateSeed seed ->
            { prevParams | seed = String.toInt seed |> Maybe.withDefault prevParams.seed }
        UpdateStrength strength ->
            { prevParams | strength = String.toFloat strength |> Maybe.withDefault prevParams.strength }
        UpdateMinValue minValue ->
            { prevParams | minValue = String.toFloat minValue |> Maybe.withDefault prevParams.strength }