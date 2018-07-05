
# Model estimation --------------------------------------------------------

model_estimation <- model_estimate_distances_v2(
  dat_input = dat_input,
  dat_db = dat_db_msr,
  dat_station = dat_station,
  dist_measures = dist_measures
)
# model_estimation[[1]][[1]]

# NOTE:
# * First level: data input vectors
# * Second level: station
# * Third level: distance measure values

# Identify best choices ---------------------------------------------------

model_prediction_index <- model_predict_index_distances_v2(
  model_estimation = model_estimation,
  knn = knn
)
# model_prediction_index

# Model prediction --------------------------------------------------------

model_prediction <- model_predict_distances_v2(
  model_prediction_index,
  dat_input = dat_input,
  dat_db = dat_db,
  dat_station = dat_station,
  knn = knn
)
print("DEBUG")
model_prediction_ensemble <- model_predict_ensemble_distances(
  model_prediction = model_prediction,
  dist_measure = dist_measure_final
)
# model_prediction_ensemble[[1]]$prediction_ensemble$dim_dist_measure

# Model output ------------------------------------------------------------

# model_result <- model_output(model_prediction_ensemble)

model_result_gathered <- model_output_gathered(model_prediction_ensemble)

if (settings$output$show) {
  model_result_render(
    model_result_gathered,
    knn = knn,
    dist_measures = dist_measures,
    dist_measure_final = dist_measure_final
  )
}

