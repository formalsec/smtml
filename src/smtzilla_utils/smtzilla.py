import argparse
import json
import pandas as pd
import numpy as np
from sklearn.tree import export_text, DecisionTreeRegressor, _tree
from sklearn.model_selection import cross_val_score, KFold
from sklearn.ensemble import GradientBoostingRegressor

parser = argparse.ArgumentParser(
    prog="SMTZilla",
    description="Using machine learning to generate a model "
    "that intelligently selects the best solver for a given query using its "
    "features",
)

parser.add_argument(
    "path",
    help="Path to a CSV file containing the training data (query features and \
      runtimes by solver)",
)

parser.add_argument(
    "--gradient-boost",
    action=argparse.BooleanOptionalAction,
    default=True,
    help="Use gradient boosting regressor",
)

parser.add_argument(
    "-e",
    "--export",
    nargs="?",
    default=None,
    help="Optional path to a JSON file where the models will be exported",
)

parser.add_argument(
    "-s",
    "--simulation",
    action=argparse.BooleanOptionalAction,
    default=False,
    help="Run a simulation with the provided training data",
)

parser.add_argument(
    "--pp-stats",
    action=argparse.BooleanOptionalAction,
    default=False,
    help="Print some stats about the training data",
)

parser.add_argument(
    "--debug",
    action=argparse.BooleanOptionalAction,
    default=False,
    help="Print some debugging information",
)

args = parser.parse_args()
model_col = "model"
solver_col = "solver"
time_col = "time"
query_size_col = "query_size"
delta_col = "delta"


def mk_data(path):
    data = pd.read_csv(path)
    feature_cols = [
        c for c in data.columns if c not in [solver_col, time_col, model_col]
    ]
    data[query_size_col] = data[feature_cols].sum(axis=1)
    # Oracle times: the minimum time achieved by any solver for the same features
    oracle_times = data.groupby(feature_cols)[time_col].transform("min")
    # Delta: the difference between current solver time and the best possible time
    data[delta_col] = data[time_col] - oracle_times
    return (data, feature_cols)


def mk_models(data, feature_cols, gradient_boost=True, debug=False):
    # Train one regression model per solver
    models = {}
    for solver in data[solver_col].unique():
        df_solver = data[data[solver_col] == solver]
        X = df_solver[feature_cols]
        y = df_solver[delta_col]

        if gradient_boost:
            model = GradientBoostingRegressor(
                n_estimators=5, max_depth=5, random_state=42
            )
        else:
            model = DecisionTreeRegressor(max_depth=5, random_state=42)

        model.fit(X, y)
        models[solver] = model

        # Cross-validation MAE (Mean Absolute Error) for this solver
        kf = KFold(n_splits=5, shuffle=True, random_state=42)
        cv_scores = cross_val_score(
            model, X, y, cv=kf, scoring="neg_mean_absolute_error"
        )
        if debug:
            print(
                f"[CV] Solver {solver:10} | Average MAE = \
                  {-cv_scores.mean():.2f} ± {cv_scores.std():.2f}\n"
            )
    return models


def pp_stats(data, feature_cols):
    # Compute stats per set of features with a given solver
    stats = (
        data.groupby(feature_cols + [solver_col])[time_col]
        .agg(count="count", mean="mean", median="median", std="std")
        .reset_index()
    )

    # Compute the relative difference between mean and median
    stats["mean_median_diff"] = (stats["mean"] - stats["median"]).abs()
    stats["rel_diff"] = stats["mean_median_diff"] / stats["mean"]

    # Filter groups where dispersion is significant
    print("")
    for threshold in [0.5, 0.8, 0.9, 0.95, 0.99]:
        high_dispersion = stats[stats["rel_diff"] > threshold]
        print(
            f"[INFO] Number of groups with high dispersion (>{int(threshold*100)}%): \
              {len(high_dispersion)}"
        )
    print("")


# currently not used
# feature_cols = [c for c in data.columns if c not in [model_col, solver_col, time_col]]


def debug_gb(data, models, feature_cols):
    for model_name in models:
        model = models[model_name]
        feature_importance = model.feature_importances_
        sorted_idx = np.argsort(feature_importance)[::-1]
        for solver in data[solver_col].unique():
            X = data[data[solver_col] == solver][feature_cols]
            print(f"\n=== Top 10 Features for {solver}:")
            for i in sorted_idx[:10]:  # top 10 features
                print(f"{X.columns[i]}: {feature_importance[i]:.4f}")

            n_estimators, n_classes = model.estimators_.shape
            for i in range(n_estimators):
                for j in range(n_classes):
                    tree = model.estimators_[i, j]
                    print(export_text(tree, feature_names=list(X.columns)))


def debug_no_gb(data, models, feature_cols):
    for solver in data[solver_col].unique():
        X = data[data[solver_col] == solver][feature_cols]
        model = models[solver]
        print(f"\n=== Decision Tree Rules for solver {solver} ===\n")
        print(export_text(model, feature_names=list(X.columns)))
        print("=" * 60, "\n")


def tree_to_dict(tree, feature_names):
    feature_name = [
        feature_names[i] if i != _tree.TREE_LEAF else "undefined!" for i in tree.feature
    ]

    def recurse(node):
        if node != _tree.TREE_LEAF:
            return {
                "feature": feature_name[node],
                "threshold": float(tree.threshold[node]),
                "left": recurse(tree.children_left[node]),
                "right": recurse(tree.children_right[node]),
            }
        else:
            return {"value": float(tree.value[node][0, 0])}

    return recurse(0)


def gbrt_to_dict(model, feature_names):
    model_dict = {
        "n_estimators": model.n_estimators,
        "init_value": (
            float(model.init_.constant_) if hasattr(model.init_, "constant_") else 0.0
        ),
        "trees": [],
    }
    for _, stage in enumerate(model.estimators_[:, 0]):  # regression => 1 column
        tree_dict = tree_to_dict(stage.tree_, feature_names)
        model_dict["trees"].append(tree_dict)
    return model_dict


def export_models_to_json(models, feature_cols):
    output_json = {}
    for model_name in models:
        gradient_boost = isinstance(models[model_name], GradientBoostingRegressor)
        assert gradient_boost or isinstance(models[model_name], DecisionTreeRegressor)
        if gradient_boost:
            model_dict = gbrt_to_dict(models[model_name], feature_cols)
        else:
            model_dict = tree_to_dict(models[model_name].tree_, feature_cols)
        output_json[model_name] = {
            "gradient_boost": gradient_boost,
            "model": model_dict,
        }
    return output_json


def simulation(data, models, feature_cols):
    oracle_total = 0
    regression_total = 0
    n_queries = 0
    data["query_hash"] = data[feature_cols].astype(str).agg("-".join, axis=1)
    static_totals = {solver: 0.0 for solver in data["solver"].unique()}

    for _, df in data.groupby("query_hash"):
        features = df[feature_cols].iloc[0:1]
        # Same features for this group

        # Oracle: the best actual time recorded
        times_by_solver = df.set_index("solver")["time"].to_dict()
        best_time = min(times_by_solver.values())
        oracle_total += best_time

        # Model: predict delta for each solver
        deltas = {
            solver: model.predict(features)[0] for solver, model in models.items()
        }
        # Pick solver with the lowest predicted delta (best predicted performance)
        predicted_solver = min(deltas.items(), key=lambda item: item[1])[0]

        if predicted_solver in times_by_solver:
            regression_total += times_by_solver[predicted_solver]
        else:
            # Fallback to mean if solver not in data for this query
            regression_total += np.mean(list(times_by_solver.values()))

        # Baseline: Fixed solver comparison
        for solver in static_totals:
            if solver in times_by_solver:
                static_totals[solver] += times_by_solver[solver]
            else:
                static_totals[solver] += float(np.mean(list(times_by_solver.values())))

        n_queries += 1

    print(f"\nTotal queries compared: {n_queries}")
    print(f"Total time with optimal choice (oracle) : {oracle_total / 1e9:.2f} s")
    print(f"Total time via regression prediction    : {regression_total / 1e9:.2f} s")
    for solver, total in static_totals.items():
        print(f"Total time with fixed solver {solver}: {total / 1e9:.2f} s")
    print(f"\nModel overhead vs. oracle: {(regression_total - oracle_total)/1e9:.2f} s")


data, feature_cols = mk_data(str(args.path))
data.drop("model", axis=1, inplace=True)

if args.pp_stats:
    pp_stats(data, feature_cols)

models = mk_models(data, feature_cols, args.gradient_boost, args.debug)

if args.debug:
    if args.gradient_boost:
        debug_gb(data, models, feature_cols)
    else:
        debug_no_gb(data, models, feature_cols)

if args.export:
    models_json = export_models_to_json(models, feature_cols)
    print(f"Exporting to: {args.export}")
    with open(args.export, "w") as f:
        json.dump(models_json, f)

if args.simulation:
    simulation(data, models, feature_cols)
