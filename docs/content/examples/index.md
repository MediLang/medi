# Medi Examples

This section provides practical examples of Medi code for various healthcare applications.

## Example Categories

* [Clinical Decision Support](clinical-decision-support.md)
* [Genomic Analysis](genomic-analysis.md)
* [Real-time Patient Monitoring](patient-monitoring.md)
* [Medical Imaging](medical-imaging.md)
* [Clinical Trials](clinical-trials.md)
* [Hospital Operations](hospital-operations.md)

## Featured Example: Diabetes Risk Prediction

Below is a complete example of a diabetes risk prediction model using Medi:

```medi
// Diabetes Risk Prediction Model
// Uses federated learning across multiple hospitals

// Configure federation
federated diabetes_prediction {
  sites: ["hospital_a", "hospital_b", "hospital_c"],
  privacy: {
    epsilon: 0.1,  // Differential privacy parameter
    secure_aggregation: true
  }
};

// Load FHIR data from each site
dataset patients = fhir_query("Patient", filter: "age>30");
dataset labs = fhir_query("Observation", filter: "code=glucose,hba1c,bmi");

// Join datasets
dataset training_data = patients
  |> join(labs, on: "patient_id")
  |> preprocess();

// Define model
model = diabetes_model {
  type: "random_forest",
  features: ["age", "gender", "bmi", "glucose", "hba1c", "family_history"],
  target: "diabetes_diagnosis",
  hyperparameters: {
    n_estimators: 100,
    max_depth: 10
  }
};

// Train federated model
diabetes_prediction.train(model, data: training_data);

// Evaluate model
metrics = diabetes_prediction.evaluate();
print("Model accuracy: " + metrics.accuracy);
print("Model AUC: " + metrics.auc);

// Predict for new patients
dataset new_patients = load_csv("new_patients.csv");
predictions = model.predict(new_patients);

// Visualize results
visualize {
  plot_roc(metrics, title: "Diabetes Model ROC Curve");
  plot_feature_importance(model, title: "Feature Importance");
  plot_risk_scores(new_patients, predictions, title: "Risk Distribution");
};

// Generate report
report {
  template: "clinical_model",
  data: {
    model: model,
    metrics: metrics,
    predictions: predictions
  },
  output: "diabetes_risk_model_report.pdf"
};
```

## Additional Resources

* [Standard Library Reference](../reference/standard-library.md)
* [Best Practices](../key-features/best-practices.md)
* [Community Examples Repository](https://github.com/MediLang/examples)
